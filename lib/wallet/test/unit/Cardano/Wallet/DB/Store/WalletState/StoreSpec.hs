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
import Cardano.Wallet.DB.Arbitrary
    ( GenState, InitialCheckpoint (..) )
import Cardano.Wallet.DB.Fixtures
    ( initializeWallet, withDBInMemory )
import Cardano.Wallet.DB.Store.Checkpoints.Store
    ( PersistAddressBook (..) )
import Cardano.Wallet.DB.Store.Checkpoints.StoreSpec
    ( genDeltaCheckpoints )
import Cardano.Wallet.DB.Store.Info.Store
    ( WalletInfo (WalletInfo) )
import Cardano.Wallet.DB.Store.WalletState.Store
    ( mkStoreWallet )
import Cardano.Wallet.DB.WalletState
    ( DeltaWalletState, DeltaWalletState1 (..), fromGenesis, fromWallet )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyGenesisParameters )
import Cardano.Wallet.Flavor
    ( KeyOf, WalletFlavorS (ShelleyWallet) )
import Cardano.Wallet.Primitive.Types
    ( WalletId (..) )
import Cardano.Wallet.Read.NetworkId
    ( NetworkDiscriminant (..) )
import Control.Monad
    ( forM_ )
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
    , counterexample
    , property
    , sized
    )
import Test.QuickCheck.Monadic
    ( PropertyM, assert, monadicIO, monitor, pick, run )
import UnliftIO.Exception
    ( impureThrow )

import Cardano.Address.Derivation
    ( XPrv )

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
genDeltaWalletState =
    withCheckpoints $ genDeltaCheckpoints genCheckpoint
  where
    withCheckpoints f = fmap updateCheckpoints . f . (^. #checkpoints)
    updateCheckpoints x = [UpdateCheckpoints [x]]

    genCheckpoint slotNo = do
        cp <- over (#currentTip . #slotNo) (const slotNo) <$> arbitrary
        pure . snd $ fromWallet cp

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
