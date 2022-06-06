{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

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
    ( TxId, getTxId )
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
    ( TxStatus (InLedger, Pending), status )
import Control.Arrow
    ( second, (***) )
import Control.Monad
    ( foldM )
import Data.Foldable
    ( toList )
import Data.Map.Strict
    ( Map )
import qualified Data.Map.Strict as Map
import Data.Maybe
    ( catMaybes )
import Data.Set
    ( Set )
import Test.DBVar
    ( GenDelta, prop_StoreUpdates )
import Test.Hspec
    ( Spec, around, describe, it )
import Test.QuickCheck
    ( Gen
    , Property
    , arbitrary
    , cover
    , elements
    , frequency
    , listOf1
    , property
    , suchThat
    )
import Test.QuickCheck.Monadic
    ( assert, pick, run )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Data.Set as Set

spec :: Spec
spec = around withDBInMemory $ do
    describe "Transactions store" $ do
        it "respects store laws" $
            property . prop_StoreTransactionsLaws
        it "can prune all not-in-ledger transaction" $
            property . prop_PruneToAllInLedger
        it "can mark pending transactions as expired based on current slot" $
            property . prop_AgeAllPending2Expire
        it "can mark past pending transactions as expired based on current slot" $
            property . prop_AgeSomePending2Expire
        it "can roll back to a given slot, removing incoming transactions" $
            property . prop_RollBackRemoveIncoming

prop_RollBackRemoveIncoming :: SqliteContext -> WalletId -> Property
prop_RollBackRemoveIncoming = error "not implemented"

-- AgeTxHistory verb

prop_AgeAllPending2Expire :: SqliteContext -> WalletId -> Property
prop_AgeAllPending2Expire = runWalletProp $ \wid (RunQuery runQ) -> do
    expansion <- pick $ logScale $ genExpand wid
    let store = mkStoreTransactions wid
    result <- run . runQ $ do
        unsafeUpdateS store mempty expansion
            >>= \ba -> foldM (unsafeUpdateS store) ba (firstPendingSlot ba)
    assert $ noPendingLeft result

firstPendingSlot :: TxHistory -> Maybe DeltaTxHistory
firstPendingSlot (TxHistoryF (null -> True)) = Nothing
firstPendingSlot (TxHistoryF txs) =
    fmap AgeTxHistory $ maximum $ txMetaSlotExpires . fst <$> toList txs

prop_AgeSomePending2Expire :: SqliteContext -> WalletId -> Property
prop_AgeSomePending2Expire = runWalletProp $ \wid (RunQuery runQ) -> do
    expansion <-
        pick $
            logScale $ ExpandTxHistory wid <$> listOf1 generatePendings
    let store = mkStoreTransactions wid
    bootHistory <- run . runQ $ do
        unsafeUpdateS store mempty expansion
    (NothingIsGreatest mSplitSlot, (unchanged, changed)) <-
        let pendings = allPendings bootHistory
         in if null pendings
                then pure (NothingIsGreatest Nothing, mempty)
                else pick $ splitPendingsG pendings
    cover
        50
        (length unchanged + length changed > 0)
        "pending transactions size"
        <$> case mSplitSlot of
            Nothing -> pure ()
            Just splitSlot -> do
                newHistory <- run . runQ $ do
                    unsafeUpdateS store bootHistory $ AgeTxHistory splitSlot
                assert $
                    snd (allPendingsPartitionedBySlot newHistory splitSlot)
                        == changed

generatePendings :: Gen (W.Tx, W.TxMeta)
generatePendings =
    biased
        arbitrary
        [ (20, \(_, meta) -> status meta == Pending),
          (1, const True)
        ]

noPendingLeft :: TxHistoryF f -> Bool
noPendingLeft (TxHistoryF txs) = all ((/=) Pending . txMetaStatus . fst) txs

allPendings :: TxHistoryF f -> Map (NothingIsGreatest W.SlotNo) [TxId]
allPendings (TxHistoryF txs) =
    Map.fromListWith (<>) $
        fmap (\(k, v) -> (gMaybe v, [k])) $
            Map.assocs $
                fmap txMetaSlotExpires $
                    Map.filter ((==) Pending . txMetaStatus) $
                        fst <$> txs

splitPendingsG ::
    Map (NothingIsGreatest W.SlotNo) [TxId] ->
    Gen (NothingIsGreatest W.SlotNo, (Set TxId, Set TxId))
splitPendingsG m = do
    k <- elements $ Map.keys m
    pure $
        (k,) $
            (foldMap Set.fromList *** foldMap Set.fromList) $
                Map.split k m

allPendingsPartitionedBySlot :: TxHistoryF f -> W.SlotNo -> (Set TxId, Set TxId)
allPendingsPartitionedBySlot (TxHistoryF txs) slotNo =
    (Map.keysSet *** Map.keysSet) $
        Map.partition (<= gJust slotNo) $
            fmap (gMaybe . txMetaSlotExpires) $
                Map.filter ((==) Pending . txMetaStatus) $
                    fst <$> txs

newtype NothingIsGreatest a = NothingIsGreatest (Maybe a) deriving (Show, Eq)

instance Ord a => Ord (NothingIsGreatest a) where
    NothingIsGreatest Nothing `compare` NothingIsGreatest (Just _) = GT
    NothingIsGreatest (Just _) `compare` NothingIsGreatest Nothing = LT
    NothingIsGreatest x `compare` NothingIsGreatest y = x `compare` y

gMaybe :: Maybe a -> NothingIsGreatest a
gMaybe = NothingIsGreatest

gJust :: a -> NothingIsGreatest a
gJust = gMaybe . Just

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

biased :: Gen a -> [(Int, a -> Bool)] -> Gen a
biased g fs = frequency $ second (suchThat g) <$> fs

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

prop_PruneToAllInLedger :: SqliteContext -> WalletId -> Property
prop_PruneToAllInLedger = runWalletProp $ \wid (RunQuery runQ) -> do
    expansion <- pick $ logScale $ genExpand wid
    let store = mkStoreTransactions wid
    result <- run . runQ $ do
        unsafeUpdateS store mempty expansion
            >>= \ba -> foldM (unsafeUpdateS store) ba (pruneAll ba)
    assert $ allInLedger result
