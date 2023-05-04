module Cardano.Wallet.DB.Store.Info.StoreSpec (spec) where

import Prelude

import Cardano.DB.Sqlite
    ( ForeignKeysSetting (ForeignKeysDisabled) )
import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Fixtures
    ( WalletProperty, logScale, withDBInMemory, withInitializedWalletProp )
import Cardano.Wallet.DB.Store.Info.Store
    ( DeltaWalletInfo (..), WalletInfo (..), mkStoreInfo )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyGenesisParameters )
import Test.Hspec
    ( Spec, around, describe, it )
import Test.QuickCheck
    ( Arbitrary (..), Gen, property )
import Test.Store
    ( prop_StoreUpdates )

spec :: Spec
spec = do
    around (withDBInMemory ForeignKeysDisabled) $ do
        describe "wallet metadata store" $ do
            it "respects store laws"
                $ property . prop_WalletMetadataStore

prop_WalletMetadataStore :: WalletProperty
prop_WalletMetadataStore = withInitializedWalletProp
    $ \_ runQ ->
        prop_StoreUpdates
            runQ
            mkStoreInfo
            ( WalletInfo
                <$> arbitrary
                <*> arbitrary
                <*> pure dummyGenesisParameters
            )
            (logScale . genDeltas)

genDeltas :: WalletInfo -> Gen DeltaWalletInfo
genDeltas _ = UpdateWalletMetadata <$> arbitrary
