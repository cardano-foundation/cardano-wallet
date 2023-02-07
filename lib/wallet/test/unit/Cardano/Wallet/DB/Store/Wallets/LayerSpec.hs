{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.DB.Store.Wallets.LayerSpec
    ( spec
    ) where

import Prelude

import Cardano.DB.Sqlite
    ( ForeignKeysSetting (..) )
import Cardano.Wallet.DB.Fixtures
    ( WalletProperty, logScale, withDBInMemory, withInitializedWalletProp )
import Cardano.Wallet.DB.Store.QueryStore
    ( QueryStore (store) )
import Cardano.Wallet.DB.Store.Wallets.Layer
    ( newQueryStoreTxWalletsHistory )
import Cardano.Wallet.DB.Store.Wallets.StoreSpec
    ( genDeltaTxWallets )
import Test.DBVar
    ( prop_StoreUpdates )
import Test.Hspec
    ( Spec, around, describe, it )
import Test.QuickCheck
    ( property )

spec :: Spec
spec = do
    around (withDBInMemory ForeignKeysDisabled) $ do
        describe "newQueryStoreTxWalletsHistory" $ do
            it "respects store laws" $ property . prop_StoreWalletsLaws

prop_StoreWalletsLaws :: WalletProperty
prop_StoreWalletsLaws =
  withInitializedWalletProp $ \wid runQ -> do
    qs <- runQ newQueryStoreTxWalletsHistory
    prop_StoreUpdates
      runQ
      (store qs)
      (pure mempty)
      (logScale . genDeltaTxWallets wid)
