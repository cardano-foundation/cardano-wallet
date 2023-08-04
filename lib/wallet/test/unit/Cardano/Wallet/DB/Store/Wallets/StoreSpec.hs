{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.DB.Store.Wallets.StoreSpec
  ( spec
  , genDeltaTxWallets
  )
where

import Cardano.DB.Sqlite
  ( ForeignKeysSetting (..)
  , runQuery
  )
import Cardano.Wallet.DB.Arbitrary
  (
  )
import Cardano.Wallet.DB.Fixtures
  ( WalletProperty
  , initializeWalletTable
  , logScale
  , withDBInMemory
  )
import Cardano.Wallet.DB.Sqlite.Schema
  ( TxMeta (..)
  )
import Cardano.Wallet.DB.Store.Meta.Layer
  ( mkQueryStoreTxMeta
  )
import Cardano.Wallet.DB.Store.Meta.Model
  ( TxMetaHistory (..)
  )
import Cardano.Wallet.DB.Store.Wallets.Model
  ( DeltaTxWalletsHistory (..)
  )
import Cardano.Wallet.DB.Store.Wallets.Store
  ( mkStoreTxWalletsHistory
  )
import Cardano.Wallet.Primitive.Types qualified as W
import Control.Concurrent.Class.MonadSTM
  ( MonadSTM
  )
import Control.Monad.Class.MonadThrow
  ( MonadThrow
  )
import Data.Delta
  ( Delta
  )
import Data.Map.Strict qualified as Map
import Data.Store
  ( UpdateStore
  , newStore
  )
import Test.Hspec
  ( Spec
  , around
  , describe
  , it
  )
import Test.QuickCheck
  ( Gen
  , NonEmptyList (..)
  , arbitrary
  , choose
  , frequency
  , property
  )
import Test.Store
  ( GenDelta
  , prop_StoreUpdate
  )
import Prelude

spec :: Spec
spec = do
  around (withDBInMemory ForeignKeysDisabled) $ do
    describe "wallets-transactions store no fk" $ do
      it "respects store laws" $ property . prop_StoreWalletsLaws
  around (withDBInMemory ForeignKeysEnabled) $ do
    describe "wallets-transactions store with fk " $ do
      it "respects store laws" $ property . prop_StoreWalletsLaws

prop_StoreWalletsLaws :: WalletProperty
prop_StoreWalletsLaws db wid =
  prop_StoreUpdate
    (runQuery db)
    setupStore
    (pure mempty)
    (logScale . genDeltaTxWallets wid)
  where
    setupStore = do
      initializeWalletTable wid
      storeTransactions <- newUpdateStore
      let
        storeWalletsMeta = mkQueryStoreTxMeta
      pure $ mkStoreTxWalletsHistory storeTransactions storeWalletsMeta

newUpdateStore
  :: (Delta da, MonadSTM m, MonadThrow m)
  => m (UpdateStore m da)
newUpdateStore = newStore

genDeltaTxWallets :: W.WalletId -> GenDelta DeltaTxWalletsHistory
genDeltaTxWallets wid (_, metas) = do
  let
    metaGens
      | null (relations metas) = []
      | otherwise =
          [
            ( 5
            , RollbackTxWalletsHistory . txMetaSlot
                <$> chooseFromMap (relations metas)
            )
          ]
  frequency
    $ (10, ExpandTxWalletsHistory wid . getNonEmpty <$> arbitrary)
      : metaGens

chooseFromMap :: Map.Map k a -> Gen a
chooseFromMap m = snd . (`Map.elemAt` m) <$> choose (0, Map.size m - 1)
