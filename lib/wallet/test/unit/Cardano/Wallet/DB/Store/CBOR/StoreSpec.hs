module Cardano.Wallet.DB.Store.CBOR.StoreSpec ( spec ) where

import Prelude

import Cardano.DB.Sqlite
    ( ForeignKeysSetting (ForeignKeysDisabled) )
import Cardano.Wallet.DB.Fixtures
    ( StoreProperty, logScale, withDBInMemory, withStoreProp )
import Cardano.Wallet.DB.Store.CBOR.ModelSpec
    ( genDeltas )
import Cardano.Wallet.DB.Store.CBOR.Store
    ( mkStoreCBOR )
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
        describe "CBOR store" $ do
            it "respects store laws" $ property . prop_SingleWalletStoreLaws

prop_SingleWalletStoreLaws :: StoreProperty
prop_SingleWalletStoreLaws = withStoreProp $ \runQ -> do
    void $ prop_StoreUpdates
        runQ
        mkStoreCBOR
        (pure mempty)
        (logScale . genDeltas)

