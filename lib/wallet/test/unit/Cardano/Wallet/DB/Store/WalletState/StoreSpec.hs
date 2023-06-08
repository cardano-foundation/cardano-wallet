{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Wallet.DB.Store.WalletState.StoreSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.DB.Sqlite
    ( ForeignKeysSetting (..), SqliteContext (runQuery) )
import Cardano.Wallet.Address.Derivation
    ( Depth (..) )
import Cardano.Wallet.DB.Arbitrary
    ( GenState, InitialCheckpoint (..) )
import Cardano.Wallet.DB.Fixtures
    ( initializeWalletTable, withDBInMemory )
import Cardano.Wallet.DB.Store.Checkpoints.Store
    ( PersistAddressBook (..) )
import Cardano.Wallet.DB.Store.Checkpoints.StoreSpec
    ( genDeltaCheckpoints )
import Cardano.Wallet.DB.Store.Info.Store
    ( WalletInfo (WalletInfo) )
import Cardano.Wallet.DB.Store.WalletState.Store
    ( mkStoreWallet )
import Cardano.Wallet.DB.WalletState
    ( DeltaWalletState
    , DeltaWalletState1 (..)
    , WalletState
    , fromGenesis
    , fromWallet
    )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyGenesisParameters )
import Cardano.Wallet.Flavor
    ( KeyOf, WalletFlavorS (ShelleyWallet) )
import Cardano.Wallet.Primitive.Types
    ( WalletId (..) )
import Cardano.Wallet.Read.NetworkId
    ( NetworkDiscriminant (..) )
import Data.Generics.Internal.VL.Lens
    ( over, (^.) )
import Data.Maybe
    ( fromJust )
import Test.Hspec
    ( Spec, around, describe, it )
import Test.QuickCheck
    ( Arbitrary (..), Property, property )
import Test.QuickCheck.Monadic
    ( monadicIO, run )
import Test.Store
    ( GenDelta, prop_StoreUpdates )

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
    setup = run . toIO $ initializeWalletTable wid
    genState = do
        wi <-
            WalletInfo wid
                <$> arbitrary
                <*> pure dummyGenesisParameters
        pure $ fromJust . fromGenesis cp0 $ wi
    prop =
        prop_StoreUpdates
            (run . toIO)
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

instance Show (WalletState s) where
    show _ = "WalletState{…}"
