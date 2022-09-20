{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Wallet.DB.Store.Meta.ModelSpec
    ( spec
    , genDeltasForManipulate
    , genExpand ) where

import Prelude

import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Fixtures
    ( elementsOrArbitrary, frequencySuchThat, logScale )
import Cardano.Wallet.DB.Sqlite.Schema
    ( TxMeta (txMetaDirection, txMetaSlot, txMetaStatus)
    , txMetaSlotExpires
    , txMetaTxId
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (TxId) )
import Cardano.Wallet.DB.Store.Meta.Model
    ( DeltaTxMetaHistory (..)
    , ManipulateTxMetaHistory (..)
    , TxMetaHistory (..)
    , mkTxMetaHistory
    )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (Incoming, Outgoing), TxStatus (InLedger, Pending), status )
import Control.Arrow
    ( (***) )
import Data.Delta
    ( Delta (..) )
import Data.Foldable
    ( foldl', toList )
import Data.Generics.Internal.VL
    ( over )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( catMaybes, fromJust )
import Data.Set
    ( Set )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Gen, Property, arbitrary, cover, elements, listOf1, property )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "meta-transactions delta instance" $ do
        it "can prune all not-in-ledger transaction"
            $ property prop_PruneToAllInLedger
        it "can mark pending transactions as expired based on current slot"
            $ property prop_AgeAllPending2Expire
        it
            "can mark past pending transactions as expired based on current slot"
            $ property prop_AgeSomePending2Expire
        it "can roll back to a given slot, removing all incoming transactions"
            $ property prop_RollBackRemoveAfterSlot
        it
            "can roll back to a given slot, switching all outgoing transactions \
            \ after slot in pending state"
            $ property prop_RollBackSwitchOutgoing
        it
            "can roll back to a given slot, preserving all outgoing transactions"
            $ property prop_RollBackPreserveOutgoing
        it "can roll back to a given slot, leaving past untouched"
            $ property prop_RollBackDoNotTouchPast

genDeltasForManipulate :: TxMetaHistory -> [(Int, Gen ManipulateTxMetaHistory)]
genDeltasForManipulate history =
    [(1, PruneTxMetaHistory . TxId <$> arbitrary)
   , (4, genPrune history)
   , (1, AgeTxMetaHistory <$> arbitrary)
   , (1, genAge history)
   , (3, genRollBack history)
    ]

genAge :: TxMetaHistory -> Gen ManipulateTxMetaHistory
genAge (TxMetaHistory history) =
    fmap AgeTxMetaHistory
    $ elementsOrArbitrary id
    $ catMaybes
    $ txMetaSlotExpires <$> toList history

genPrune :: TxMetaHistory -> Gen ManipulateTxMetaHistory
genPrune history =
    fmap PruneTxMetaHistory
    $ elementsOrArbitrary TxId
    $ Map.keys
    $ relations history

genExpand :: WalletId -> Gen [(W.Tx, W.TxMeta)] -> Gen TxMetaHistory
genExpand wid g = mkTxMetaHistory wid <$> g

genRollBack :: TxMetaHistory -> Gen ManipulateTxMetaHistory
genRollBack (TxMetaHistory history) =
    fmap RollBackTxMetaHistory
    $ elementsOrArbitrary id
    $ txMetaSlot <$> toList history

withExpanded :: W.WalletId -> Gen [(W.Tx, W.TxMeta)] -> Gen TxMetaHistory
withExpanded wid expandG = do
    expansion <- logScale $ genExpand wid expandG
    pure $ apply (Expand expansion) mempty

type WithWalletProperty = WalletId -> Property

withPropRollBack
    :: (( (TxMetaHistory, TxMetaHistory) -- bootHistory split
        , (TxMetaHistory, TxMetaHistory) -- newHistory split
        )
        -> WithWalletProperty)
    -> WithWalletProperty
withPropRollBack f wid = property $ do
    bootHistory <- withExpanded
        wid
        (listOf1 arbitrary)
    slotNo <- internalSlotNoG bootHistory
    let newHistory =
            apply `flip` bootHistory
            $ Manipulate
            $ RollBackTxMetaHistory slotNo
    pure
        $ cover
            40
            (not . null $ allIncoming bootHistory)
            "incoming transactions"
        $ cover
            40
            (not . null $ allOutgoing bootHistory)
            "outgoing transactions"
        $ f (splitHistory slotNo bootHistory, splitHistory slotNo newHistory)

prop_RollBackRemoveAfterSlot :: WithWalletProperty
prop_RollBackRemoveAfterSlot =
    withPropRollBack $ \((_afterBoot,_beforeBoot),(afterNew,_beforeNew)) _
    -> property $ null $ relations afterNew

prop_RollBackSwitchOutgoing :: WithWalletProperty
prop_RollBackSwitchOutgoing =
    withPropRollBack $ \((_afterBoot,beforeBoot),(_afterNew,beforeNew)) _ -> do
        let future = overTxMetaHistory beforeNew $ \beforeMap
                -> Map.difference beforeMap $ relations beforeBoot
        property
            $ all
                ((&&) <$> ((==) Pending . txMetaStatus)
                 <*> ((==) Outgoing . txMetaDirection))
                (relations future)

prop_RollBackPreserveOutgoing :: WithWalletProperty
prop_RollBackPreserveOutgoing =
    withPropRollBack $ \((afterBoot,beforeBoot),(_afterNew,beforeNew)) _ -> do
        let future = overTxMetaHistory beforeNew $ \beforeMap
                -> Map.difference beforeMap $ relations beforeBoot
        property
            $ (==)
                (Map.keys $ allOutgoing afterBoot)
                (Map.keys $ relations future)

prop_RollBackDoNotTouchPast :: WithWalletProperty
prop_RollBackDoNotTouchPast =
    withPropRollBack $ \((afterBoot,beforeBoot),(_afterNew,beforeNew)) _ -> do
        let past = overTxMetaHistory beforeNew $ \beforeMap
                -> Map.difference beforeMap $ relations afterBoot
        property $ past == beforeBoot

overTxMetaHistory
    :: TxMetaHistory
    -> (Map TxId TxMeta -> Map TxId TxMeta)
    -> TxMetaHistory
overTxMetaHistory = flip $ over #relations

allWithDirection :: Direction -> TxMetaHistory -> Map TxId TxMeta
allWithDirection dir (TxMetaHistory txs) =
    Map.filter ((==) dir . txMetaDirection) txs

allIncoming :: TxMetaHistory -> Map TxId TxMeta
allIncoming = allWithDirection Incoming

allOutgoing :: TxMetaHistory -> Map TxId TxMeta
allOutgoing = allWithDirection Outgoing

allSlots :: TxMetaHistory -> Set W.SlotNo
allSlots (TxMetaHistory txs) = Set.fromList . toList $ txMetaSlot <$> txs

internalSlotNoG :: TxMetaHistory -> Gen W.SlotNo
internalSlotNoG th = elements $ toList $ allSlots th

splitHistory :: W.SlotNo -> TxMetaHistory -> (TxMetaHistory, TxMetaHistory)
splitHistory slotSplit (TxMetaHistory txs) =
    (TxMetaHistory *** TxMetaHistory)
    $ Map.partition ((> slotSplit) . txMetaSlot) txs

prop_AgeAllPending2Expire :: WithWalletProperty
prop_AgeAllPending2Expire wid = property $ do
    bootHistory <- withExpanded
        wid
        (listOf1 generatePendings)
    let result =
            foldl' (flip apply) bootHistory
            $ Manipulate <$> firstPendingSlot bootHistory
    pure
        $ cover
            50
            (not . null $ allPendings bootHistory)
            "pending transactions size"
        $ noPendingLeft result

firstPendingSlot
    :: TxMetaHistory
    -> Maybe (ManipulateTxMetaHistory)
firstPendingSlot
    (TxMetaHistory (null -> True)) = Nothing
firstPendingSlot (TxMetaHistory txs) =
    fmap AgeTxMetaHistory $ maximum $ txMetaSlotExpires <$> toList txs

prop_AgeSomePending2Expire :: WithWalletProperty
prop_AgeSomePending2Expire wid = property $ do
    bootHistory <- withExpanded
        wid
        (listOf1 generatePendings)
    let pendings = allPendings bootHistory
    if null pendings
        then pure $ property True
        else do
            (splitSlot,(unchanged,changed)) <- splitPendingsG pendings
            let newHistory =
                    apply `flip` bootHistory
                    $ Manipulate
                    $ AgeTxMetaHistory splitSlot
            pure
                $ cover
                    50
                    (length unchanged + length changed > 0)
                    "pending transactions size"
                $ snd (pendingsPartitionedBySlot newHistory splitSlot)
                == changed

generatePendings :: Gen (W.Tx, W.TxMeta)
generatePendings = frequencySuchThat arbitrary [(20, \(_,meta)
    -> status meta == Pending), (1, const True) ]

noPendingLeft :: TxMetaHistory -> Bool
noPendingLeft
    (TxMetaHistory txs) = flip all txs $ \meta -> txMetaStatus meta /= Pending

allPendings :: TxMetaHistory -> Map W.SlotNo [TxId]
allPendings (TxMetaHistory txs) =
    Map.fromListWith (<>)
    $ fmap (\(k,v) -> (v, [k ]))
    $ Map.assocs
    $ (fromJust . txMetaSlotExpires)
    <$> Map.filter ((==) Pending . txMetaStatus) txs

splitPendingsG :: Map W.SlotNo [TxId] -> Gen (W.SlotNo, (Set TxId, Set TxId))
splitPendingsG m = do
    k <- elements $ Map.keys m
    pure
        $ (k, )
        $ (foldMap Set.fromList *** foldMap Set.fromList)
        $ Map.split k m

pendingsPartitionedBySlot :: TxMetaHistory -> W.SlotNo -> (Set TxId, Set TxId)
pendingsPartitionedBySlot (TxMetaHistory txs) slotNo =
    (Map.keysSet *** Map.keysSet)
    $ Map.partition (<= slotNo)
    $ (fromJust . txMetaSlotExpires)
    <$> Map.filter ((==) Pending . txMetaStatus) txs

allInLedger :: TxMetaHistory -> Bool
allInLedger (TxMetaHistory txs) =
    all ((==) InLedger . txMetaStatus) txs

pruneAll :: TxMetaHistory -> [ManipulateTxMetaHistory]
pruneAll (TxMetaHistory txs) = do
    meta <- toList txs
    pure $ PruneTxMetaHistory $ txMetaTxId meta

prop_PruneToAllInLedger :: WithWalletProperty
prop_PruneToAllInLedger wid = property $ do
    expansion <- logScale $ genExpand wid arbitrary
    let ba = apply `flip` mempty $ Expand expansion
        result = foldl' (flip apply) ba (Manipulate <$> pruneAll ba)
    pure $ allInLedger result
