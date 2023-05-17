{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Wallet.DB.Store.WalletState.StoreSpec
    ( spec
    ) where

import Prelude

import Cardano.DB.Sqlite
    ( ForeignKeysSetting (..), SqliteContext (runQuery) )
import Cardano.Wallet.Address.Derivation
    ( Depth (..) )
import Cardano.Wallet.Checkpoints
    ( DeltaCheckpoints (..) )
import Cardano.Wallet.DB.Arbitrary
    ( GenState, InitialCheckpoint (..) )
import Cardano.Wallet.DB.Fixtures
    ( initializeWallet, withDBInMemory )
import Cardano.Wallet.DB.Store.Checkpoints.Store
    ( PersistAddressBook (..) )
import Cardano.Wallet.DB.Store.Info.Store
    ( WalletInfo (WalletInfo) )
import Cardano.Wallet.DB.Store.WalletState.Store
    ( mkStoreWallet )
import Cardano.Wallet.DB.WalletState
    ( DeltaWalletState
    , DeltaWalletState1 (..)
    , fromGenesis
    , fromWallet
    , getLatest
    , getSlot
    )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyGenesisParameters )
import Cardano.Wallet.Flavor
    ( KeyOf, WalletFlavorS (ShelleyWallet) )
import Cardano.Wallet.Primitive.Types
    ( SlotNo (..), WalletId (..), WithOrigin (..) )
import Cardano.Wallet.Read.NetworkId
    ( NetworkDiscriminant (..) )
import Control.Monad
    ( forM_ )
import Data.Bifunctor
    ( second )
import Data.Delta
    ( Base, Delta (..) )
import Data.Generics.Internal.VL.Lens
    ( over, (^.) )
import Data.Maybe
    ( fromJust )
import Data.Store
    ( Store (..) )
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

import Cardano.Address.Derivation
    ( XPrv )
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    around (withDBInMemory ForeignKeysEnabled) $ do
        describe "Update" $ do
            it "mkStoreWallet" $
                property . prop_StoreWallet (ShelleyWallet @'Mainnet)

{-------------------------------------------------------------------------------
    Properties
-------------------------------------------------------------------------------}
prop_StoreWallet
    :: forall s
     . ( PersistAddressBook s
       , GenState s, Eq (KeyOf s 'RootK XPrv)
       )
    => WalletFlavorS s
    -> SqliteContext
    -> (WalletId, InitialCheckpoint s)
    -> Property
prop_StoreWallet wF db (wid, InitialCheckpoint cp0) =
    monadicIO (setup >> prop)
  where
    toIO = runQuery db
    setup = run . toIO $ initializeWallet wid
    genState = do
        wi <-
            WalletInfo wid
                <$> arbitrary
                <*> pure dummyGenesisParameters
        pure $ fromJust . fromGenesis cp0 $ wi
    prop = do
        prop_StoreUpdates
            toIO
            (mkStoreWallet wF wid)
            genState
            genDeltaWalletState

genDeltaWalletState
    :: GenState s
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
    -> Store m qa da
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
