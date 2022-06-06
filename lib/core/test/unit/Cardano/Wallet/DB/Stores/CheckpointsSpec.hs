{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.DB.Stores.CheckpointsSpec
    ( spec
    ) where

import Prelude

import Cardano.DB.Sqlite
    ( SqliteContext (runQuery) )
import Cardano.Wallet.DB.Arbitrary
    ( GenState, InitialCheckpoint (..) )
import Cardano.Wallet.DB.Checkpoints.AddressBook
    ( AddressBookIso (..), Prologue, getPrologue )
import Cardano.Wallet.DB.Checkpoints.Model
    ( DeltaCheckpoints (..), fromWallet, getSlot )
import Cardano.Wallet.DB.Stores.Fixtures
    ( assertWith, initializeWallet, withDBInMemory )
import Cardano.Wallet.DB.Wallets.State
    ( DeltaWalletState, DeltaWalletState1 (..), fromGenesis, getLatest )
import Cardano.Wallet.DB.Wallets.Store
    ( PersistAddressBook (..), mkStoreWallet )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (Mainnet) )
import Cardano.Wallet.Primitive.AddressDerivation.Shared
    ( SharedKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState )
import Cardano.Wallet.Primitive.AddressDiscovery.Shared
    ( Readiness (Pending), SharedState (..) )
import Cardano.Wallet.Primitive.Types
    ( SlotNo (..), WalletId (..), WithOrigin (..) )
import Data.Bifunctor
    ( second )
import Data.Generics.Internal.VL.Lens
    ( over, (^.) )
import Fmt
    ( Buildable (..), pretty )
import Test.DBVar
    ( GenDelta, prop_StoreUpdates )
import Test.Hspec
    ( Spec, around, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , choose
    , counterexample
    , frequency
    , property
    , vectorOf
    )
import Test.QuickCheck.Monadic
    ( PropertyM, monadicIO, monitor, run )

import qualified Data.Map.Strict as Map

spec :: Spec
spec = around withDBInMemory $ do
    describe "Writing and loading" $ do
        it "loadPrologue . insertPrologue = id  for SeqState" $
            property . prop_prologue_load_write @(SeqState 'Mainnet ShelleyKey) id

        it "loadPrologue . insertPrologue = id  for RndState" $
            property . prop_prologue_load_write @(RndState 'Mainnet) id

        it "loadPrologue . insertPrologue = id  for SharedState" $
            property . prop_prologue_load_write @(SharedState 'Mainnet SharedKey)
                (\s -> s { ready = Pending })

    describe "Update" $ do
        it "mkStoreWallet" $
            property . prop_StoreWallet @(SeqState 'Mainnet ShelleyKey)

{-------------------------------------------------------------------------------
    Properties
-------------------------------------------------------------------------------}
-- | Check that writing and loading the 'Prologue' works.
prop_prologue_load_write
    :: forall s.
    ( PersistAddressBook s
    , Buildable (Prologue s)
    , Eq (Prologue s)
    )
    => (s -> s) -> SqliteContext -> (WalletId, s) -> Property
prop_prologue_load_write preprocess db (wid, s) =
    monadicIO $ run (toIO setup) >> prop
  where
    toIO = runQuery db
    setup = initializeWallet wid
    prop = prop_loadAfterWrite toIO (insertPrologue wid) (loadPrologue wid) pro
    pro = getPrologue $ preprocess s
    -- FIXME during ADP-1043: See note at 'multisigPoolAbsent'

-- | Checks that loading a value after writing it to a database table
-- is successful.
prop_loadAfterWrite
    :: ( Monad m, Buildable (f a) , Eq (f a), Applicative f )
    => (forall b. m b -> IO b)
    -- ^ Function to embed the monad in 'IO'
    -> (a -> m ())
    -- ^ Write operation
    -> (m (f a))
    -- ^ Load operation
    -> a
    -- ^ Property arguments
    -> PropertyM IO ()
prop_loadAfterWrite toIO writeOp loadOp a = do
    res <- run . toIO $ writeOp a >> loadOp
    let fa = pure a
    monitor $ counterexample $ "\nInserted\n" <> pretty fa
    monitor $ counterexample $ "\nRead\n" <> pretty res
    assertWith "Inserted == Read" (res == fa)

{-------------------------------------------------------------------------------
    Update
-------------------------------------------------------------------------------}
prop_StoreWallet
    :: forall s.
    ( PersistAddressBook s
    , GenState s
    )
    => SqliteContext
    -> (WalletId, InitialCheckpoint s)
    -> Property
prop_StoreWallet db (wid, InitialCheckpoint cp0) =
    monadicIO (setup >> prop)
  where
    toIO = run . runQuery db
    setup = toIO $ initializeWallet wid
    prop = do
        let Just w0 = fromGenesis cp0 -- mempty
        prop_StoreUpdates toIO (mkStoreWallet wid) (pure w0) genDeltaWalletState

genDeltaWalletState
    :: (GenState s, AddressBookIso s)
    => GenDelta (DeltaWalletState s)
genDeltaWalletState wallet = frequency . map (second updateCheckpoints) $
    [ (8, genPutCheckpoint)
    , (1, pure $ RollbackTo Origin)
    , (1, RollbackTo . At . SlotNo <$> choose (0, slotLatest))
    , (1, RollbackTo . At . SlotNo <$> choose (slotLatest+1, slotLatest+10))
    , (2, RestrictTo <$> genFilteredSlots)
    , (1, pure $ RestrictTo [])
    ]
  where
    updateCheckpoints gen = (\x -> [UpdateCheckpoints x]) <$> gen

    slotLatest = case getSlot . snd . fromWallet $ getLatest wallet of
        Origin -> 0
        At (SlotNo s) -> s
    genSlotNo = SlotNo . (slotLatest +) <$> choose (1,10)

    genPutCheckpoint = do
        slot <- genSlotNo
        cp   <- over (#currentTip . #slotNo) (const slot) <$> arbitrary
        pure $ PutCheckpoint (At slot) (snd $ fromWallet cp)

    genFilteredSlots = do
        let slots = Map.keys $ wallet ^. (#checkpoints . #checkpoints)
        keeps <- vectorOf (length slots) arbitrary
        pure . map fst . filter snd $ zip slots keeps


