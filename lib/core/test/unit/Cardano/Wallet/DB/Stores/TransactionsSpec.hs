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

import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Sqlite.Schema
    ( TxMeta (txMetaDirection, txMetaSlot, txMetaStatus)
    , txMetaSlotExpires
    , txMetaTxId
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId, getTxId )
import Cardano.Wallet.DB.Stores.Fixtures
    ( WalletProperty
    , coverM
    , logScale
    , unsafeUpdateS
    , withDBInMemory
    , withInitializedWalletProp
    )
import Cardano.Wallet.DB.Transactions.Delta
    ( DeltaTxHistory (AgeTxHistory, ExpandTxHistory, PruneTxHistory, RollBackTxHistory)
    )
import Cardano.Wallet.DB.Transactions.Store
    ( mkStoreTransactions )
import Cardano.Wallet.DB.Transactions.Types
    ( TxHistory, TxHistoryF (TxHistoryF), overTxHistoryF, txHistory_relations )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (Incoming, Outgoing), TxStatus (InLedger, Pending), status )
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
    ( catMaybes, fromJust )
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
    , elements
    , frequency
    , listOf1
    , property
    , suchThat
    )
import Test.QuickCheck.Monadic
    ( PropertyM, assert, pick )

import Cardano.Wallet.DB.Transactions.Model
    ( mkTxHistory )
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import Data.DBVar
    ( Store )
import qualified Data.Set as Set
import Database.Persist.Sql
    ( SqlPersistT )

spec :: Spec
spec = around withDBInMemory $ do
    describe "Transactions store" $ do
        it "respects store laws"
            $ property . prop_StoreTransactionsLaws
        it "can prune all not-in-ledger transaction"
            $ property . prop_PruneToAllInLedger
        it "can mark pending transactions as expired based on current slot"
            $ property . prop_AgeAllPending2Expire
        it "can mark past pending transactions as expired based on current slot"
            $ property . prop_AgeSomePending2Expire
        it "can roll back to a given slot, removing all incoming transactions"
            $ property . prop_RollBackRemoveAfterSlot
        it "can roll back to a given slot, switching all outgoing transactions \
            \ after slot in pending state"
            $ property . prop_RollBackSwitchOutgoing
        it "can roll back to a given slot, preserving all outgoing transactions"
            $ property . prop_RollBackPreserveOutgoing
        it "can roll back to a given slot, leaving past untouched"
            $ property . prop_RollBackDoNotTouchPast

withExpanded
    :: Gen [(W.Tx, W.TxMeta)]
    -> ( (forall a . SqlPersistT IO a -> PropertyM IO a)
        -> Store (SqlPersistT IO) DeltaTxHistory
        -> TxHistory
        -> PropertyM IO Property
       )
    -> WalletProperty
withExpanded expandG f = withInitializedWalletProp $ \wid runQ -> do
    expansion <- pick
        $ logScale
        $ genExpand wid expandG
    let store = mkStoreTransactions wid
    bootHistory <- runQ $ do
        unsafeUpdateS store mempty expansion
    f runQ store bootHistory

withPropRollBack
    :: (( (TxHistory, TxHistory) -- bootHistory split
        , (TxHistory, TxHistory) -- newHistory split
        )
    -> PropertyM IO ())
    -> WalletProperty
withPropRollBack f =  withExpanded (listOf1 arbitrary)
    \run store bootHistory -> do
        slotNo <- pick $ internalSlotNoG bootHistory
        newHistory <- run $ do
            unsafeUpdateS store bootHistory $ RollBackTxHistory slotNo
        coverM 40 (not . null $ allIncoming bootHistory)
            "incoming transactions"
            $ coverM 40 (not . null $ allOutgoing bootHistory)
                "outgoing transactions"
                $ f (splitHistory slotNo bootHistory
                    , splitHistory slotNo newHistory
                    )

prop_RollBackRemoveAfterSlot :: WalletProperty
prop_RollBackRemoveAfterSlot =  withPropRollBack
    $ \((_afterBoot, _beforeBoot), (afterNew, _beforeNew)) ->
        assert $ null $ txHistory_relations  afterNew

prop_RollBackSwitchOutgoing :: WalletProperty
prop_RollBackSwitchOutgoing =  withPropRollBack
    $ \((_afterBoot, beforeBoot), (_afterNew, beforeNew)) -> do
        let future = overTxHistoryF beforeNew $ \beforeMap ->
                Map.difference beforeMap $ txHistory_relations beforeBoot
        assert $ all
            do (&&)
                <$> ((==) Pending . txMetaStatus . fst)
                <*> ((==) Outgoing . txMetaDirection . fst)
            do txHistory_relations future

prop_RollBackPreserveOutgoing :: WalletProperty
prop_RollBackPreserveOutgoing = withPropRollBack
    $ \((afterBoot, beforeBoot), (_afterNew, beforeNew)) -> do
        let future = overTxHistoryF beforeNew $ \beforeMap ->
                Map.difference beforeMap $ txHistory_relations beforeBoot
        assert $ (==)
            do Map.keys $ allOutgoing afterBoot
            do Map.keys $ txHistory_relations future

prop_RollBackDoNotTouchPast :: WalletProperty
prop_RollBackDoNotTouchPast =  withPropRollBack
    $ \((afterBoot, beforeBoot), (_afterNew, beforeNew)) -> do
        let past = overTxHistoryF beforeNew $ \beforeMap ->
                Map.difference beforeMap $ txHistory_relations afterBoot
        assert $ past == beforeBoot

allWithDirection :: Direction -> TxHistoryF f -> Map TxId TxMeta
allWithDirection dir (TxHistoryF txs) =
    Map.filter ((==) dir . txMetaDirection) $
        fst <$> txs

allIncoming :: TxHistoryF f -> Map TxId TxMeta
allIncoming= allWithDirection Incoming

allOutgoing :: TxHistoryF f -> Map TxId TxMeta
allOutgoing = allWithDirection Outgoing

allSlots :: TxHistoryF f -> Set W.SlotNo
allSlots (TxHistoryF txs) = Set.fromList . toList $ txMetaSlot . fst <$> txs

internalSlotNoG ::
    TxHistory ->
    Gen W.SlotNo
internalSlotNoG th = elements $ toList $ allSlots th

splitHistory :: W.SlotNo -> TxHistoryF f -> (TxHistoryF f, TxHistoryF f)
splitHistory slotSplit (TxHistoryF txs) =(TxHistoryF *** TxHistoryF)
        $ Map.partition ((> slotSplit) . txMetaSlot  . fst) txs

-- AgeTxHistory verb

prop_AgeAllPending2Expire :: WalletProperty
prop_AgeAllPending2Expire = withExpanded (listOf1 generatePendings)
    \runQ store bootHistory -> do
        result <- runQ $ do
            foldM (unsafeUpdateS store) bootHistory
                $ firstPendingSlot bootHistory
        coverM 50 (not . null $ allPendings bootHistory)
            "pending transactions size"
                $ assert (noPendingLeft result)

firstPendingSlot :: TxHistory -> Maybe DeltaTxHistory
firstPendingSlot (TxHistoryF (null -> True)) = Nothing
firstPendingSlot (TxHistoryF txs) =
    fmap AgeTxHistory $ maximum $ txMetaSlotExpires . fst <$> toList txs

prop_AgeSomePending2Expire :: WalletProperty
prop_AgeSomePending2Expire = withExpanded (listOf1 generatePendings)
    \runQ store bootHistory -> do
        let pendings = allPendings bootHistory
        if null pendings
            then pure $ property True
            else do
                (splitSlot, (unchanged, changed)) <-
                    pick $ splitPendingsG pendings
                coverM 50
                    (length unchanged + length changed > 0)
                    "pending transactions size"
                    $ do
                    newHistory <- runQ $ do
                        unsafeUpdateS store bootHistory
                            $ AgeTxHistory splitSlot
                    assert $
                        snd (pendingsPartitionedBySlot newHistory splitSlot)
                        == changed

generatePendings :: Gen (W.Tx, W.TxMeta)
generatePendings =
    biased
        arbitrary
        [ (20, \(_, meta) -> status meta == Pending)
        , (1, const True)
        ]

noPendingLeft :: TxHistoryF f -> Bool
noPendingLeft (TxHistoryF txs) = flip all txs
    $ \(meta, _) -> txMetaStatus meta /= Pending


allPendings :: TxHistoryF f -> Map W.SlotNo [TxId]
allPendings (TxHistoryF txs) =
    Map.fromListWith (<>) $
        fmap (\(k, v) -> (v, [k])) $
            Map.assocs $
                fmap (fromJust . txMetaSlotExpires) $
                    Map.filter ((==) Pending . txMetaStatus) $
                        fst <$> txs

splitPendingsG ::
    Map W.SlotNo [TxId] ->
    Gen (W.SlotNo, (Set TxId, Set TxId))
splitPendingsG m = do
    k <- elements $ Map.keys m
    pure $
        (k,) $
            (foldMap Set.fromList *** foldMap Set.fromList) $
                Map.split k m

pendingsPartitionedBySlot :: TxHistoryF f -> W.SlotNo -> (Set TxId, Set TxId)
pendingsPartitionedBySlot (TxHistoryF txs) slotNo =
    (Map.keysSet *** Map.keysSet) $
        Map.partition (<= slotNo) $
            fmap (fromJust . txMetaSlotExpires) $
                Map.filter ((==) Pending . txMetaStatus) $
                    fst <$> txs

--------------------------------------------------------------------------------
--  generate deltas
--------------------------------------------------------------------------------
genDeltas :: WalletId -> GenDelta DeltaTxHistory
genDeltas wid history =
    frequency
        [ (10, genExpand wid arbitrary),
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

genExpand :: WalletId -> Gen [(W.Tx, W.TxMeta)] -> Gen DeltaTxHistory
genExpand wid g = ExpandTxHistory . mkTxHistory wid <$> g

biased :: Gen a -> [(Int, a -> Bool)] -> Gen a
biased g fs = frequency $ second (suchThat g) <$> fs

prop_StoreTransactionsLaws :: WalletProperty
prop_StoreTransactionsLaws = withInitializedWalletProp $ \wid runQ ->
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

prop_PruneToAllInLedger :: WalletProperty
prop_PruneToAllInLedger = withInitializedWalletProp $ \wid runQ -> do
    expansion <- pick $ logScale $ genExpand wid arbitrary
    let store = mkStoreTransactions wid
    result <- runQ $ do
        unsafeUpdateS store mempty expansion
            >>= \ba -> foldM (unsafeUpdateS store) ba (pruneAll ba)
    assert $ allInLedger result
