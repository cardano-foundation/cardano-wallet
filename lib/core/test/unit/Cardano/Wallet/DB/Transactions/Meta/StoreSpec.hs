{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Wallet.DB.Transactions.Meta.StoreSpec(
    spec, deltasTrueFreq
) where

import Prelude

import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Fixtures
    ( WalletProperty
    , coverM
    , elementsOrArbitrary
    , frequencySuchThat
    , logScale
    , unsafeUpdateS
    , withDBInMemory
    , withInitializedWalletProp
    )
import Cardano.Wallet.DB.Sqlite.Schema
    ( TxMeta (txMetaDirection, txMetaSlot, txMetaStatus)
    , txMetaSlotExpires
    , txMetaTxId
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (TxId) )
import Cardano.Wallet.DB.Transactions.Meta.Model
    ( DeltaTxMetaHistory (..)
    , DeltaTxMetaHistoryAny (DeltaTxMetaHistoryAny)
    , TxMetaHistory (..)
    , TxMetaOp (Expansion, Manipulation)
    , mkTxMetaHistory
    , overTxMetaHistory
    )
import Cardano.Wallet.DB.Transactions.Meta.Store
    ( mkStoreTransactionsMeta )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (Incoming, Outgoing), TxStatus (InLedger, Pending), status )
import Control.Arrow
    ( (***) )
import Control.Monad
    ( foldM )
import Data.DBVar
    ( Store )
import Data.Foldable
    ( toList )
import Data.Map.Strict
    ( Map )
import qualified Data.Map.Strict as Map
import Data.Maybe
    ( catMaybes, fromJust )
import Data.Set
    ( Set )
import Database.Persist.Sql
    ( SqlPersistT )
import Test.DBVar
    ( prop_StoreUpdates )
import Test.Hspec
    ( Spec, around, describe, it )
import Test.QuickCheck
    ( Gen, Property, arbitrary, elements, frequency, listOf1, property )
import Test.QuickCheck.Monadic
    ( PropertyM, assert, pick )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Data.Set as Set

spec :: Spec
spec = around withDBInMemory $ do
    describe "Meta transactions store" $ do
        it "respects store laws"
            $ property . prop_StoreMetaLaws
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

--------------------------------------------------------------------------------
--  generate deltas
--------------------------------------------------------------------------------
genDeltas :: WalletId -> TxMetaHistory -> Gen DeltaTxMetaHistoryAny
genDeltas wid history =
    frequency $
        (10, DeltaTxMetaHistoryAny <$> genExpand wid arbitrary)
            : fmap (fmap (fmap DeltaTxMetaHistoryAny)) (deltasTrueFreq history)

deltasTrueFreq
    :: TxMetaHistory
    -> [(Int, Gen (DeltaTxMetaHistory 'Manipulation))]
deltasTrueFreq history =
        [
         (1, PruneTxMetaHistory . TxId <$> arbitrary)
        , (4, genPrune history)
        , (1, AgeTxMetaHistory <$> arbitrary)
        , (1, genAge history)
        , (3, genRollBack history)
        ]

genAge :: TxMetaHistory -> Gen (DeltaTxMetaHistory 'Manipulation)
genAge (TxMetaHistory history)
    = fmap AgeTxMetaHistory
    $ elementsOrArbitrary id
    $ catMaybes
    $ txMetaSlotExpires <$> toList history

genPrune :: TxMetaHistory -> Gen (DeltaTxMetaHistory 'Manipulation)
genPrune history
    = fmap PruneTxMetaHistory
    $ elementsOrArbitrary TxId
    $ Map.keys $ txMetaHistory_relations history

genExpand
    :: WalletId
    -> Gen [(W.Tx, W.TxMeta)]
    -> Gen (DeltaTxMetaHistory 'Expansion)
genExpand wid g = ExpandTxMetaHistory . mkTxMetaHistory wid <$> g

genRollBack :: TxMetaHistory -> Gen (DeltaTxMetaHistory 'Manipulation)
genRollBack (TxMetaHistory history)
    = fmap RollBackTxMetaHistory
    $ elementsOrArbitrary id
    $ txMetaSlot <$> toList history

prop_StoreMetaLaws :: WalletProperty
prop_StoreMetaLaws = withInitializedWalletProp $ \wid runQ ->
    prop_StoreUpdates
        runQ
        (mkStoreTransactionsMeta  wid)
        (pure mempty)
        (logScale . genDeltas wid)
withExpanded
    :: Gen [(W.Tx, W.TxMeta)]
    -> ( (forall a . SqlPersistT IO a -> PropertyM IO a)
        -> Store (SqlPersistT IO) DeltaTxMetaHistoryAny
        -> TxMetaHistory
        -> PropertyM IO Property
       )
    -> WalletProperty
withExpanded expandG f = withInitializedWalletProp $ \wid runQ -> do
    expansion <- pick
        $ logScale
        $ genExpand wid expandG
    let store = mkStoreTransactionsMeta wid
    bootHistory <- runQ $ do
        unsafeUpdateS store mempty $ DeltaTxMetaHistoryAny expansion
    f runQ store bootHistory

withPropRollBack
    :: (( (TxMetaHistory, TxMetaHistory) -- bootHistory split
        , (TxMetaHistory, TxMetaHistory) -- newHistory split
        )
    -> PropertyM IO ())
    -> WalletProperty
withPropRollBack f =  withExpanded (listOf1 arbitrary) 
    $ \run store bootHistory -> do
        slotNo <- pick $ internalSlotNoG bootHistory
        newHistory <- run $ do
            unsafeUpdateS store bootHistory
                $ DeltaTxMetaHistoryAny
                $ RollBackTxMetaHistory slotNo
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
        assert $ null $ txMetaHistory_relations  afterNew

prop_RollBackSwitchOutgoing :: WalletProperty
prop_RollBackSwitchOutgoing =  withPropRollBack
    $ \((_afterBoot, beforeBoot), (_afterNew, beforeNew)) -> do
        let future = overTxMetaHistory beforeNew $ \beforeMap ->
                Map.difference beforeMap $ txMetaHistory_relations beforeBoot
        assert $ all
            ((&&)
                <$> ((==) Pending . txMetaStatus)
                <*> ((==) Outgoing . txMetaDirection))
            (txMetaHistory_relations future)

prop_RollBackPreserveOutgoing :: WalletProperty
prop_RollBackPreserveOutgoing = withPropRollBack
    $ \((afterBoot, beforeBoot), (_afterNew, beforeNew)) -> do
        let future = overTxMetaHistory beforeNew $ \beforeMap ->
                Map.difference beforeMap $ txMetaHistory_relations beforeBoot
        assert $ (==)
            (Map.keys $ allOutgoing afterBoot)
            (Map.keys $ txMetaHistory_relations future)

prop_RollBackDoNotTouchPast :: WalletProperty
prop_RollBackDoNotTouchPast =  withPropRollBack
    $ \((afterBoot, beforeBoot), (_afterNew, beforeNew)) -> do
        let past = overTxMetaHistory beforeNew $ \beforeMap ->
                Map.difference beforeMap $ txMetaHistory_relations afterBoot
        assert $ past == beforeBoot

allWithDirection :: Direction -> TxMetaHistory -> Map TxId TxMeta
allWithDirection dir (TxMetaHistory txs) =
    Map.filter ((==) dir . txMetaDirection) txs

allIncoming :: TxMetaHistory -> Map TxId TxMeta
allIncoming= allWithDirection Incoming

allOutgoing :: TxMetaHistory -> Map TxId TxMeta
allOutgoing = allWithDirection Outgoing

allSlots :: TxMetaHistory -> Set W.SlotNo
allSlots (TxMetaHistory txs) = Set.fromList . toList $ txMetaSlot <$> txs

internalSlotNoG ::
    TxMetaHistory ->
    Gen W.SlotNo
internalSlotNoG th = elements $ toList $ allSlots th

splitHistory :: W.SlotNo -> TxMetaHistory -> (TxMetaHistory, TxMetaHistory)
splitHistory slotSplit (TxMetaHistory txs) =(TxMetaHistory *** TxMetaHistory)
        $ Map.partition ((> slotSplit) . txMetaSlot ) txs

-- AgeTxMetaHistory verb

prop_AgeAllPending2Expire :: WalletProperty
prop_AgeAllPending2Expire = withExpanded (listOf1 generatePendings)
    $ \runQ store bootHistory -> do
        result <- runQ $ do
            foldM (unsafeUpdateS store) bootHistory
                $ DeltaTxMetaHistoryAny
                <$> firstPendingSlot bootHistory
        coverM 50 (not . null $ allPendings bootHistory)
            "pending transactions size"
                $ assert (noPendingLeft result)

firstPendingSlot :: TxMetaHistory -> Maybe (DeltaTxMetaHistory 'Manipulation)
firstPendingSlot (TxMetaHistory (null -> True)) = Nothing
firstPendingSlot (TxMetaHistory txs) =
    fmap AgeTxMetaHistory $ maximum $ txMetaSlotExpires  <$> toList txs

prop_AgeSomePending2Expire :: WalletProperty
prop_AgeSomePending2Expire = withExpanded (listOf1 generatePendings)
    $ \runQ store bootHistory -> do
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
                            $ DeltaTxMetaHistoryAny
                            $ AgeTxMetaHistory splitSlot
                    assert $
                        snd (pendingsPartitionedBySlot newHistory splitSlot)
                        == changed

generatePendings :: Gen (W.Tx, W.TxMeta)
generatePendings =
    frequencySuchThat
        arbitrary
        [ (20, \(_, meta) -> status meta == Pending)
        , (1, const True)
        ]

noPendingLeft :: TxMetaHistory -> Bool
noPendingLeft (TxMetaHistory txs) = flip all txs
    $ \meta -> txMetaStatus meta /= Pending


allPendings :: TxMetaHistory -> Map W.SlotNo [TxId]
allPendings (TxMetaHistory txs)
    = Map.fromListWith (<>)
    $ fmap (\(k, v) -> (v, [k]))
    $ Map.assocs
    $ (fromJust . txMetaSlotExpires)
    <$> Map.filter ((==) Pending . txMetaStatus) txs

splitPendingsG ::
    Map W.SlotNo [TxId] ->
    Gen (W.SlotNo, (Set TxId, Set TxId))
splitPendingsG m = do
    k <- elements $ Map.keys m
    pure $
        (k,) $
            (foldMap Set.fromList *** foldMap Set.fromList) $
                Map.split k m

pendingsPartitionedBySlot :: TxMetaHistory -> W.SlotNo -> (Set TxId, Set TxId)
pendingsPartitionedBySlot (TxMetaHistory txs) slotNo
    = (Map.keysSet *** Map.keysSet)
    $ Map.partition (<= slotNo)
    $ (fromJust . txMetaSlotExpires)
    <$> Map.filter ((==) Pending . txMetaStatus) txs

allInLedger :: TxMetaHistory -> Bool
allInLedger (TxMetaHistory txs) = all ((==) InLedger . txMetaStatus) txs

pruneAll :: TxMetaHistory -> [DeltaTxMetaHistory 'Manipulation]
pruneAll (TxMetaHistory txs) = do
    meta <- toList txs
    pure
        $ PruneTxMetaHistory
        $ txMetaTxId meta

prop_PruneToAllInLedger :: WalletProperty
prop_PruneToAllInLedger = withInitializedWalletProp $ \wid runQ -> do
    expansion <- pick $ logScale $ genExpand wid arbitrary
    let store = mkStoreTransactionsMeta wid
    result <- runQ $ do
        ba <- unsafeUpdateS store mempty (DeltaTxMetaHistoryAny expansion)
        foldM (unsafeUpdateS store) ba (DeltaTxMetaHistoryAny <$> pruneAll ba)
    assert $ allInLedger result
