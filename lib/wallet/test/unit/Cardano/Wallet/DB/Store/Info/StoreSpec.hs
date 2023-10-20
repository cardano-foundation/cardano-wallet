module Cardano.Wallet.DB.Store.Info.StoreSpec (spec) where

import Prelude

import Cardano.DB.Sqlite
    ( ForeignKeysSetting (..)
    , runQuery
    )
import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Fixtures
    ( WalletProperty
    , logScale
    , withDBInMemory
    )
import Cardano.Wallet.DB.Store.Info.Store
    ( DeltaWalletInfo (..)
    , WalletInfo (..)
    , mkStoreInfo
    )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyGenesisParameters
    )
import Test.Hspec
    ( Spec
    , around
    , describe
    , it
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , property
    )
import Test.Store
    ( prop_StoreUpdate
    )

spec :: Spec
spec = do
    around (withDBInMemory ForeignKeysDisabled) $ do
        describe "wallet info store" $ do
            it "respects store laws"
                $ property . prop_StoreWalletInfo

prop_StoreWalletInfo :: WalletProperty
prop_StoreWalletInfo db _ =
    prop_StoreUpdate
        (runQuery db)
        (pure mkStoreInfo)
        ( WalletInfo
            <$> arbitrary
            <*> arbitrary
            <*> pure dummyGenesisParameters
        )
        (logScale . genDeltas)

genDeltas :: WalletInfo -> Gen DeltaWalletInfo
genDeltas _ = UpdateWalletMetadata <$> arbitrary
