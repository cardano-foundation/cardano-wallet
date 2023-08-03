{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Wallet.DB.Store.Checkpoints.StoreSpec
    ( spec
    , genDeltaCheckpoints
    ) where

import Prelude

import Cardano.DB.Sqlite
    ( ForeignKeysSetting (..), SqliteContext, runQuery )
import Cardano.Wallet.Address.Book
    ( AddressBookIso (..), Prologue, getPrologue )
import Cardano.Wallet.Address.Derivation.Shared
    ( SharedKey )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Address.Discovery.Random
    ( RndState )
import Cardano.Wallet.Address.Discovery.Sequential
    ( SeqState )
import Cardano.Wallet.Address.Discovery.Shared
    ( Readiness (Pending), SharedState (..) )
import Cardano.Wallet.Checkpoints
    ( Checkpoints, DeltaCheckpoints (..), checkpoints, getLatest )
import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Fixtures
    ( initializeWalletTable, withDBInMemory )
import Cardano.Wallet.DB.Store.Checkpoints.Store
    ( PersistAddressBook (..) )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( SlotNo (..), WalletId, WithOrigin (..) )
import Fmt
    ( Buildable (..), pretty )
import Test.Hspec
    ( Spec, around, describe, it )
import Test.QuickCheck
    ( Gen
    , Property
    , arbitrary
    , choose
    , counterexample
    , frequency
    , property
    , vectorOf
    )
import Test.QuickCheck.Monadic
    ( PropertyM, assert, monadicIO, monitor, run )

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

{-------------------------------------------------------------------------------
    Properties
-------------------------------------------------------------------------------}
-- | Check that writing and loading the 'Prologue' works.
prop_prologue_load_write
    :: forall s.
    ( PersistAddressBook s
    , Buildable (Prologue s)
    )
    => (s -> s) -> SqliteContext -> (WalletId, s) -> Property
prop_prologue_load_write preprocess db (wid, s) =
    monadicIO $ run (toIO setup) >> prop
  where
    toIO = runQuery db
    setup = initializeWalletTable wid
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
    Generators
-------------------------------------------------------------------------------}
genDeltaCheckpoints
    :: (SlotNo -> Gen a) -> Checkpoints a -> Gen (DeltaCheckpoints a)
genDeltaCheckpoints genCheckpoint cps = frequency
    [ (8, genPutCheckpoint)
    , (1, pure $ RollbackTo Origin)
    , (1, RollbackTo . At . SlotNo <$> choose (0, slotLatest))
    , (1, RollbackTo . At . SlotNo <$> choose (slotLatest+1, slotLatest+10))
    , (2, RestrictTo <$> genFilteredSlots)
    , (1, pure $ RestrictTo [])
    ]
  where
    slotLatest = case fst $ getLatest cps of
        Origin -> 0
        At (SlotNo s) -> s
    genSlotNo = SlotNo . (slotLatest +) <$> choose (1,10)

    genPutCheckpoint = do
        slotNo <- genSlotNo
        cp <- genCheckpoint slotNo
        pure $ PutCheckpoint (At slotNo) cp

    genFilteredSlots = do
        let slots = Map.keys $ checkpoints cps
        keeps <- vectorOf (length slots) arbitrary
        pure . map fst . filter snd $ zip slots keeps

{-------------------------------------------------------------------------------
    QuickCheck utilities
-------------------------------------------------------------------------------}
-- | Like 'assert', but allow giving a label / title before running a assertion
assertWith :: String -> Bool -> PropertyM IO ()
assertWith lbl condition = do
    let flag = if condition then "✓" else "✗"
    monitor (counterexample $ lbl <> " " <> flag)
    assert condition
