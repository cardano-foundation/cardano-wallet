{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.DB.Store.Wallets.LayerSpec
  ( spec
  )
where

import Cardano.DB.Sqlite
  ( ForeignKeysSetting (..)
  , runQuery
  )
import Cardano.Wallet.DB.Fixtures
  ( WalletProperty
  , logScale
  , withDBInMemory
  )
import Cardano.Wallet.DB.Store.Wallets.Layer
  ( newQueryStoreTxWalletsHistory
  )
import Cardano.Wallet.DB.Store.Wallets.StoreSpec
  ( genDeltaTxWallets
  )
import Test.Hspec
  ( Spec
  , around
  , describe
  , it
  )
import Test.QuickCheck
  ( property
  )
import Test.Store
  ( prop_StoreUpdate
  )
import Prelude

spec :: Spec
spec = do
  around (withDBInMemory ForeignKeysDisabled) $ do
    describe "newQueryStoreTxWalletsHistory" $ do
      it "respects store laws" $ property . prop_StoreWalletsLaws

prop_StoreWalletsLaws :: WalletProperty
prop_StoreWalletsLaws db wid =
  prop_StoreUpdate
    (runQuery db)
    (pure newQueryStoreTxWalletsHistory)
    (pure mempty)
    (logScale . genDeltaTxWallets wid)
