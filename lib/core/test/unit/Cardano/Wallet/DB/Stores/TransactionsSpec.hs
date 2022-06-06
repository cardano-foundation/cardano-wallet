{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.DB.Stores.TransactionsSpec (
    spec,
) where

import Prelude

import Cardano.DB.Sqlite
    ( SqliteContext )
import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Sqlite.Schema
    ( TxMeta (txMetaStatus), txMetaSlotExpires, txMetaTxId )
import Cardano.Wallet.DB.Sqlite.Types
    ( getTxId )
import Cardano.Wallet.DB.Stores.Fixtures
    ( RunQuery (RunQuery)
    , logScale
    , runWalletProp
    , unsafeUpdateS
    , withDBInMemory
    )
import Cardano.Wallet.DB.Transactions.Delta
    ( DeltaTxHistory (AgeTxHistory, ExpandTxHistory, PruneTxHistory) )
import Cardano.Wallet.DB.Transactions.Store
    ( mkStoreTransactions )
import Cardano.Wallet.DB.Transactions.Types
    ( TxHistory, TxHistoryF (TxHistoryF), txHistory_relations )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxStatus (InLedger) )
import Control.Monad
    ( foldM )
import Data.Foldable
    ( toList )
import Data.Maybe
    ( catMaybes )
import Test.DBVar
    ( GenDelta, prop_StoreUpdates )
import Test.Hspec
    ( Spec, around, describe, it )
import Test.QuickCheck
    ( Gen, Property, arbitrary, elements, frequency, property )
import Test.QuickCheck.Monadic
    ( assert, pick, run )

import qualified Data.Map.Strict as Map

spec :: Spec
spec = around withDBInMemory $ do
    describe "Transactions store" $ do
        it "respects store laws" $
            property . prop_StoreTransactionsLaws
        it "can prune all not-in-ledger transaction" $
            property . prop_PrunePositive

--------------------------------------------------------------------------------
--  generate deltas
--------------------------------------------------------------------------------
genDeltas :: WalletId -> GenDelta DeltaTxHistory
genDeltas wid history =
        frequency
            [ (10, genExpand wid),
              (1, PruneTxHistory <$> arbitrary),
              -- probably just dropped
              (4, genPrune history),
              -- will try to prune a valid id
              (1, AgeTxHistory <$> arbitrary),
              -- probably just dropped
              (4, genAge history)
              -- will age valid ids
            ]

genAge :: TxHistory -> Gen DeltaTxHistory
genAge (TxHistoryF history) =
    case catMaybes $ txMetaSlotExpires . fst <$> toList history of
        [] -> AgeTxHistory <$> arbitrary
        xs -> AgeTxHistory <$> elements xs

genPrune :: TxHistory -> Gen DeltaTxHistory
genPrune history =
    case fmap getTxId $ Map.keys $ txHistory_relations history of
        [] -> PruneTxHistory <$> arbitrary
        xs -> PruneTxHistory <$> elements xs

genExpand :: WalletId -> Gen DeltaTxHistory
genExpand wid = ExpandTxHistory wid <$> arbitrary

prop_StoreTransactionsLaws ::
    SqliteContext ->
    WalletId ->
    Property
prop_StoreTransactionsLaws = runWalletProp $ \wid (RunQuery runQ) ->
    prop_StoreUpdates
        do runQ
        do mkStoreTransactions wid
        do pure mempty
        do logScale . genDeltas wid

allInLedger :: TxHistoryF f -> Bool
allInLedger (TxHistoryF txs) = all ((==) InLedger . txMetaStatus . fst) txs

pruneAll :: TxHistoryF f -> [DeltaTxHistory]
pruneAll (TxHistoryF txs) = do
    (meta, _tx) <- toList txs
    pure $ PruneTxHistory $ getTxId $ txMetaTxId meta

prop_PrunePositive :: SqliteContext -> WalletId -> Property
prop_PrunePositive = runWalletProp $ \wid (RunQuery runQ) -> do
    expansion <- pick $ logScale $ genExpand wid
    let store = mkStoreTransactions wid
    result <- run . runQ $ do
        unsafeUpdateS store mempty expansion >>=
            \ba -> foldM (unsafeUpdateS store) ba (pruneAll ba)
    assert $ allInLedger result


