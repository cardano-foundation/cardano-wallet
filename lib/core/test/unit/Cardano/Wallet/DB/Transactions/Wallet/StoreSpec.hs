{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Wallet.DB.Transactions.Wallet.StoreSpec(
    spec,
) where

import Prelude

import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Fixtures
    ( WalletProperty
    , logScale
    , logScale'
    , withDBInMemory
    , withInitializedWalletProp
    )
import Cardano.Wallet.DB.Sqlite.Schema
    ( TxMeta (txMetaSlot, txMetaSlotExpires) )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (TxId) )
import Cardano.Wallet.DB.Transactions.Meta.Model
    ( DeltaTxMetaHistory (ExpandTxMetaHistory)
    , TxMetaHistory (TxMetaHistory)
    , mkTxMetaHistory
    )
import Cardano.Wallet.DB.Transactions.Wallet.Store
    ( DeltaTxWalletsHistory (..), mkStoreTxWalletsHistory, mkStoreWalletsMeta )
import Data.DeltaMap
    ( DeltaMap (Adjust, Delete, Insert) )
import Data.Maybe
    ( catMaybes )
import Test.DBVar
    ( GenDelta, prop_StoreUpdates )
import Test.Hspec
    ( Spec, around, describe, it )
import Test.QuickCheck
    ( Arbitrary
    , Gen
    , NonEmptyList (getNonEmpty)
    , arbitrary
    , elements
    , frequency
    , property
    )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.Map.Strict as Map

spec :: Spec
spec = around withDBInMemory $ do
    describe "Wallets meta store" $ do
         it "respects store laws"
             $ property . prop_StoreMetaLaws
    describe "Wallets transactions store" $ do
        it "respects store laws"
            $ property . prop_StoreWalletsLaws

prop_StoreMetaLaws :: WalletProperty
prop_StoreMetaLaws = withInitializedWalletProp  $ \wid runQ ->
    prop_StoreUpdates
        runQ
        mkStoreWalletsMeta
        (pure mempty)
        (logScale' 1.5 . genDeltaWalletsMeta wid)

genDeltaWalletsMeta :: W.WalletId -> GenDelta (DeltaMap W.WalletId DeltaTxMetaHistory)
genDeltaWalletsMeta wid wmetas =
    frequency $
        [ (1, Insert wid . mkTxMetaHistory wid . getNonEmpty <$> arbitrary)
        , (1, pure $ Delete wid)
        ]
        <> case wmetas of
            (null -> True) -> []
            _ -> [ (5, Adjust wid
                        . ExpandTxMetaHistory
                        . mkTxMetaHistory wid
                        . getNonEmpty
                        <$> arbitrary
                        )
                    ]

prop_StoreWalletsLaws :: WalletProperty
prop_StoreWalletsLaws = withInitializedWalletProp  $ \wid runQ ->
    prop_StoreUpdates
        runQ
        mkStoreTxWalletsHistory
        (pure mempty)
        (logScale . genDeltaTxWallets wid)


elementsOrArbitrary :: Arbitrary a => (a -> b) -> [b] -> Gen b
elementsOrArbitrary f [] = f <$> arbitrary
elementsOrArbitrary _ xs = elements xs

genDeltaTxWallets :: W.WalletId -> GenDelta DeltaTxWalletsHistory
genDeltaTxWallets wid (_, wmeta) =  do
    let onWid f g = case Map.lookup wid wmeta of
            Nothing -> f
            Just (TxMetaHistory m ) -> g m -- (_, wmetas) =
    frequency
        [ (10, ExpandTxWalletsHistory wid . getNonEmpty <$> arbitrary)
        , (1, PruneTxWalletsHistory wid
                <$> elementsOrArbitrary TxId  (onWid [] Map.keys )
                )
        , (1, AgeTxWalletsHistory wid
                <$> elementsOrArbitrary id
                    (onWid [] $ catMaybes . foldMap (pure . txMetaSlotExpires))
                )
        -- , (1, RollBackTxWalletsHistory wid
        --         <$> elementsOrArbitrary id
        --             (onWid [] $ foldMap (pure . txMetaSlot))
        --         )

        ]
