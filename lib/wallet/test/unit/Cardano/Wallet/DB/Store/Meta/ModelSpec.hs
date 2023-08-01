{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.DB.Store.Meta.ModelSpec
    ( spec
    , genRollback
    , genExpand ) where

import Prelude

import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Fixtures
    ( elementsOrArbitrary, logScale )
import Cardano.Wallet.DB.Sqlite.Schema
    ( TxMeta (..) )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId )
import Cardano.Wallet.DB.Store.Meta.Model
    ( DeltaTxMetaHistory (..), TxMetaHistory (..), mkTxMetaHistory )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
    ( Direction (Incoming, Outgoing) )
import Control.Arrow
    ( (***) )
import Data.Delta
    ( Delta (..) )
import Data.Foldable
    ( toList )
import Data.Map.Strict
    ( Map )
import Data.Set
    ( Set )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Gen, Property, arbitrary, cover, elements, listOf1, property )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Tx.Tx as W
    ( Tx )
import qualified Cardano.Wallet.Primitive.Types.Tx.TxMeta as W
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "meta-transactions delta instance" $ do
        it "can roll back to a given slot, removing all transactions"
            $ property prop_RollbackRemoveAfterSlot
        it "can roll back to a given slot, leaving past untouched"
            $ property prop_RollbackDoNotTouchPast

genExpand :: WalletId -> Gen [(W.Tx, W.TxMeta)] -> Gen TxMetaHistory
genExpand wid g = mkTxMetaHistory wid <$> g

genRollback :: TxMetaHistory -> Gen DeltaTxMetaHistory
genRollback (TxMetaHistory history) =
    fmap Rollback
    $ elementsOrArbitrary id
    $ txMetaSlot <$> toList history

withExpanded :: W.WalletId -> Gen [(W.Tx, W.TxMeta)] -> Gen TxMetaHistory
withExpanded wid expandG = do
    expansion <- logScale $ genExpand wid expandG
    pure $ apply (Expand expansion) mempty

type WithWalletProperty = WalletId -> Property

withPropRollback
    :: (( (TxMetaHistory, TxMetaHistory) -- bootHistory split
        , (TxMetaHistory, TxMetaHistory) -- newHistory split
        )
        -> WithWalletProperty)
    -> WithWalletProperty
withPropRollback f wid = property $ do
    bootHistory <- withExpanded
        wid
        (listOf1 arbitrary)
    slotNo <- internalSlotNoG bootHistory
    let newHistory =
            apply `flip` bootHistory
            $ Rollback slotNo
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

prop_RollbackRemoveAfterSlot :: WithWalletProperty
prop_RollbackRemoveAfterSlot =
    withPropRollback $ \((_afterBoot,_beforeBoot),(afterNew,_beforeNew)) _
    -> property $ null $ relations afterNew

prop_RollbackDoNotTouchPast :: WithWalletProperty
prop_RollbackDoNotTouchPast =
    withPropRollback $ \((afterBoot,beforeBoot),(_afterNew,beforeNew)) _ ->
        let past = (relations beforeNew) `Map.difference` (relations afterBoot)
        in  property $ TxMetaHistory past == beforeBoot

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
