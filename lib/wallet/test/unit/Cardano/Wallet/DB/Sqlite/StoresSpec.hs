{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Wallet.DB.Sqlite.StoresSpec
    ( spec
    ) where

import Prelude

import Cardano.DB.Sqlite
    ( ForeignKeysSetting (..), SqliteContext (runQuery) )
import Cardano.Wallet.Address.Book
    ( AddressBookIso (..), Prologue, getPrologue )
import Cardano.Wallet.Checkpoints
    ( DeltaCheckpoints (..) )
import Cardano.Wallet.DB.Arbitrary
    ( GenState, InitialCheckpoint (..) )
import Cardano.Wallet.DB.Fixtures
    ( initializeWallet, withDBInMemory )
import Cardano.Wallet.DB.Sqlite.Stores
    ( PersistAddressBook (..), mkStoreWallet )
import Cardano.Wallet.DB.WalletState
    ( DeltaWalletState
    , DeltaWalletState1 (..)
    , fromGenesis
    , fromWallet
    , getLatest
    , getSlot
    )
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
import Control.Monad
    ( forM_ )
import Data.Bifunctor
    ( second )
import Data.DBVar
    ( Store (..) )
import Data.Delta
    ( Base, Delta (..) )
import Data.Generics.Internal.VL.Lens
    ( over, (^.) )
import Fmt
    ( Buildable (..), listF, pretty )
import Test.Hspec
    ( Spec, around, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Blind (..)
    , Gen
    , Property
    , choose
    , counterexample
    , frequency
    , property
    , sized
    , vectorOf
    )
import Test.QuickCheck.Monadic
    ( PropertyM, assert, monadicIO, monitor, pick, run )
import UnliftIO.Exception
    ( impureThrow )

import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    around (withDBInMemory ForeignKeysEnabled) $ do
        describe "Writing and loading" $ do
            it "loadPrologue . insertPrologue = id  for SeqState" $
                property . prop_prologue_load_write @(SeqState 'Mainnet ShelleyKey) id

            it "loadPrologue . insertPrologue = id  for RndState" $
                property . prop_prologue_load_write @(RndState 'Mainnet) id

            it "loadPrologue . insertPrologue = id  for SharedState" $
                property . prop_prologue_load_write @(SharedState 'Mainnet SharedKey)
                    (\s -> s { ready = Pending })

    around (withDBInMemory ForeignKeysEnabled) $ do
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
    toIO = runQuery db
    setup = run . toIO $ initializeWallet wid
    prop = do
        let Just w0 = fromGenesis cp0
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
    updateCheckpoints gen = (\x -> [UpdateCheckpoints [x]]) <$> gen

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

-- | Given a value, generate a random delta starting from this value.
type GenDelta da = Base da -> Gen da

-- | A sequence of updates and values after updating.
-- The update that is applied *last* appears in the list *first*.
newtype Updates da = Updates [(Base da, da)]

instance Show da => Show (Updates da) where
    show (Updates xs) = show . map snd $ xs

-- | Randomly generate a sequence of updates
genUpdates :: Delta da => Gen (Base da) -> GenDelta da -> Gen (Updates da)
genUpdates gen0 more = sized $ \n -> go n [] =<< gen0
  where
    go 0 das _  = pure $ Updates das
    go n das a0 = do
        da <- more a0
        let a1 = apply da a0
        go (n-1) ((a1,da):das) a1

-- | Test whether 'updateS' and 'loadS' behave as expected.
--
-- TODO: Shrinking of the update sequence.
prop_StoreUpdates
    :: ( Monad m, Delta da, Eq (Base da), Buildable da )
    => (forall b. m b -> IO b)
    -- ^ Function to embed the monad in 'IO'
    -> Store m da
    -- ^ Store that is to be tested.
    -> Gen (Base da)
    -- ^ Generator for the initial value.
    -> GenDelta da
    -- ^ Generator for deltas.
    -> PropertyM IO ()
prop_StoreUpdates toIO store gen0 more = do
    let runs = run . toIO

    -- randomly generate a sequence of updates
    Blind a0 <- pick $ Blind <$> gen0
    Blind (Updates adas) <- pick $ Blind <$> genUpdates (pure a0) more
    let as  = map fst adas ++ [a0]
        das = map snd adas

    monitor $ counterexample $
        "\nUpdates applied:\n" <> pretty (listF das)

    -- apply those updates
    ea <- runs $ do
        writeS store a0
        -- first update is applied last!
        let updates = reverse $ zip das (drop 1 as)
        forM_ updates $ \(da,a) -> updateS store (Just a) da
        loadS store

    -- check whether the last value is correct
    case ea of
        Left err -> impureThrow err
        Right a  -> do
            assert $ a == head as

{-------------------------------------------------------------------------------
    QuickCheck utilities
-------------------------------------------------------------------------------}
-- | Like 'assert', but allow giving a label / title before running a assertion
assertWith :: String -> Bool -> PropertyM IO ()
assertWith lbl condition = do
    let flag = if condition then "✓" else "✗"
    monitor (counterexample $ lbl <> " " <> flag)
    assert condition
