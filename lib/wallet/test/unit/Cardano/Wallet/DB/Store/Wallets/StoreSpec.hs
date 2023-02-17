{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.DB.Store.Wallets.StoreSpec
    ( spec
    , genDeltaTxWallets
    ) where

import Prelude

import Cardano.DB.Sqlite
    ( ForeignKeysSetting (..) )
import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Fixtures
    ( WalletProperty, logScale, withDBInMemory, withInitializedWalletProp )
import Cardano.Wallet.DB.Sqlite.Schema
    ( TxMeta (..) )
import Cardano.Wallet.DB.Store.Meta.Model
    ( TxMetaHistory (..) )
import Cardano.Wallet.DB.Store.Wallets.Model
    ( DeltaTxWalletsHistory (..) )
import Cardano.Wallet.DB.Store.Wallets.Store
    ( mkStoreTxWalletsHistory, mkStoreWalletsMeta )
import Data.DBVar
    ( newStore )
import Test.DBVar
    ( GenDelta, prop_StoreUpdates )
import Test.Hspec
    ( Spec, around, describe, it )
import Test.QuickCheck
    ( Gen, NonEmptyList (..), arbitrary, choose, frequency, property )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    around (withDBInMemory ForeignKeysDisabled) $ do
        describe "wallets-transactions store no fk" $ do
            it "respects store laws" $ property . prop_StoreWalletsLaws
    around (withDBInMemory ForeignKeysEnabled) $ do
        describe "wallets-transactions store with fk " $ do
            it "respects store laws" $ property . prop_StoreWalletsLaws

prop_StoreWalletsLaws :: WalletProperty
prop_StoreWalletsLaws =
  withInitializedWalletProp $ \wid runQ -> do
    -- Note: We have already tested `mkStoreTransactions`,
    -- so we use `newStore` here for a faster test.
    storeTransactions <- runQ newStore
    let storeWalletsMeta = mkStoreWalletsMeta
        storeTxWalletsHistory =
            mkStoreTxWalletsHistory storeTransactions storeWalletsMeta

    prop_StoreUpdates
      runQ
      storeTxWalletsHistory
      (pure mempty)
      (logScale . genDeltaTxWallets wid)

genDeltaTxWallets :: W.WalletId -> GenDelta DeltaTxWalletsHistory
genDeltaTxWallets wid (_,metaMap) = do
  let metaGens = case Map.lookup wid metaMap of
        Nothing -> []
        Just metas ->
          [ (5, RollbackTxWalletsHistory wid . txMetaSlot
                <$> chooseFromMap (relations metas) )
          , (1, pure $ RemoveWallet wid)
          ]
  frequency $
    (10, ExpandTxWalletsHistory wid . getNonEmpty <$> arbitrary) :
    metaGens

chooseFromMap :: Map.Map k a -> Gen a
chooseFromMap m = snd . (`Map.elemAt` m) <$> choose (0, Map.size m-1)
