

module Cardano.Wallet.DB.Store.Submissions.StoreSpec ( spec ) where

import Prelude

import Cardano.DB.Sqlite
    ( ForeignKeysSetting (ForeignKeysDisabled) )
import Cardano.Wallet.DB.Fixtures
    ( WalletProperty, logScale, withDBInMemory, withInitializedWalletProp )
import Cardano.Wallet.DB.Store.Submissions.ModelSpec
    ( genDeltas )
import Cardano.Wallet.DB.Store.Submissions.Store
    ( mkStoreSubmissions )
import Control.Monad
    ( void )
import Test.DBVar
    ( prop_StoreUpdates )
import Test.Hspec
    ( Spec, around, describe, it )
import Test.QuickCheck
    ( property )

spec :: Spec
spec = do
    around (withDBInMemory ForeignKeysDisabled) $ do
        describe "local-submission-transactions single wallet store" $ do
            it "respects store laws" $ property . prop_SingleWalletStoreLaws

prop_SingleWalletStoreLaws :: WalletProperty
prop_SingleWalletStoreLaws = withInitializedWalletProp $ \wid runQ -> do
    void $ prop_StoreUpdates
        runQ
        (mkStoreSubmissions wid)
        (pure mempty)
        (logScale . genDeltas wid)

