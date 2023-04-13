{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
--
-- 'UTxOHistory' represents a history of a UTxO set that can be rolled
-- back (up to the 'finality' point).
module Cardano.Wallet.DB.Store.UTxOHistory.Model
    ( -- * Types
      UTxOHistory
    , Pruned (..)
    , Spent (..)

      -- * Observations
    , getTip
    , getFinality
    , empty
    , getUTxO

      -- * Changes
    , DeltaUTxOHistory (..)

      -- * For testing
    , getSpent

      -- * Store helpers
    , constrainingPrune
    , constrainingRollback
    , constrainingAppendBlock
    , reverseMapOfSets
    )
where

import Prelude

import Cardano.Slotting.Slot
    ( SlotNo, WithOrigin (..) )
import Cardano.Wallet.DB.Store.UTxOHistory.Model.Internal
    ( Pruned (..), UTxOHistory (..) )
import Cardano.Wallet.Primitive.Types
    ( Slot )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn )
import Cardano.Wallet.Primitive.Types.UTxO
    ( DeltaUTxO (..), UTxO, dom, excluding )
import Control.Error
    ( fromMaybe )
import Control.Monad
    ( guard )
import Data.Delta
    ( Delta (..) )
import Data.Foldable
    ( fold, foldl' )
import Data.Map.Strict
    ( Map )
import Data.Set
    ( Set )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Changes to the UTxO history.
data DeltaUTxOHistory
    = -- | New slot tip, changes within that block.
      AppendBlock SlotNo DeltaUTxO
    | -- | Rollback tip.
      Rollback Slot
    | -- | Move finality forward.
      Prune SlotNo
    deriving (Show, Eq)

-- | An empty UTxO history
empty :: UTxO -> UTxOHistory
empty utxo =
    UTxOHistory
        { history = utxo
        , creationSlots = creationSlots
        , creationTxIns = reverseMapOfSets creationSlots
        , spentSlots = mempty
        , spentTxIns = mempty
        , tip = Origin
        , finality = NotPruned
        , boot = utxo
        }
  where
    creationSlots = Map.singleton Origin $ dom utxo

-- | Returns the UTxO.
getUTxO :: UTxOHistory -> UTxO
getUTxO UTxOHistory {history, spentSlots} = history `excluding` fold spentSlots

-- | Returns the tip slot.
getTip :: UTxOHistory -> Slot
getTip UTxOHistory {tip} = tip

-- | Returns the finality slot.
getFinality :: UTxOHistory -> Pruned
getFinality UTxOHistory {finality} = finality

-- | Returns the spent TxIns that can be rolled back.
getSpent :: UTxOHistory -> Map TxIn SlotNo
getSpent UTxOHistory {spentTxIns} = spentTxIns

-- how to apply a DeltaUTxOHistory to a UTxOHistory
instance Delta DeltaUTxOHistory where
    type Base DeltaUTxOHistory = UTxOHistory
    apply (AppendBlock newTip delta) = appendBlock newTip delta
    apply (Rollback newTip) = rollback newTip
    apply (Prune newFinality) = prune newFinality

appendBlock :: SlotNo -> DeltaUTxO -> UTxOHistory -> UTxOHistory
appendBlock newTip delta
    noop@UTxOHistory
        { history
        , spentSlots
        , creationSlots
        , creationTxIns
        , spentTxIns
        , finality
        , boot
        }
  = constrainingAppendBlock noop noop newTip $
    UTxOHistory
        { history = history <> received delta
        , creationSlots =
            insertNonEmpty (At newTip) receivedTxIns creationSlots
        , creationTxIns =
            insertNonEmptyReversedMap
                (At newTip) receivedTxIns creationTxIns
        , spentSlots =
            insertNonEmpty newTip excludedTxIns spentSlots
        , spentTxIns =
            insertNonEmptyReversedMap newTip excludedTxIns spentTxIns
        , tip = At newTip
        , finality = finality
        , boot = boot
        }
  where
    receivedTxIns =
        dom (received delta) `Set.difference` dom history
    excludedTxIns =
        (excluded delta `Set.intersection` dom history)
        `Set.difference` fold spentSlots

rollback :: Slot -> UTxOHistory -> UTxOHistory
rollback newTip
    noop@UTxOHistory
        { history
        , spentSlots
        , creationSlots
        , creationTxIns
        , spentTxIns
        , finality
        , boot
        }
  = constrainingRollback noop noop newTip $ \case
    Just newTip' ->
        let
            (leftCreationSlots, rolledCreatedSlots) =
                Map.spanAntitone (<= newTip') creationSlots
            rolledSpentTxIns = fold $ case newTip' of
                Origin -> spentSlots
                At slot'' ->
                    Map.dropWhileAntitone
                        (<= slot'')
                        spentSlots
            rolledCreatedTxIns = fold rolledCreatedSlots
        in
            UTxOHistory
                { history = history `excluding` rolledCreatedTxIns
                , spentSlots = case newTip' of
                    Origin -> mempty
                    At slot'' ->
                        Map.takeWhileAntitone
                            (<= slot'')
                            spentSlots
                , creationSlots = leftCreationSlots
                , creationTxIns =
                    Map.withoutKeys
                        creationTxIns
                        rolledCreatedTxIns
                , spentTxIns =
                    Map.withoutKeys
                        spentTxIns
                        rolledSpentTxIns
                , tip = newTip'
                , finality = finality
                , boot = boot
                }
    Nothing -> empty boot

prune :: SlotNo -> UTxOHistory -> UTxOHistory
prune newFinality
    noop@UTxOHistory
        { history
        , spentSlots
        , creationSlots
        , creationTxIns
        , spentTxIns
        , tip
        , boot
        }
  = constrainingPrune noop noop newFinality $ \newFinality' ->
    let
        (prunedSpentSlots, leftSpentSlots) =
            Map.spanAntitone
                (<= newFinality')
                spentSlots
        prunedTxIns = fold prunedSpentSlots
    in
        UTxOHistory
            { history = history `excluding` prunedTxIns
            , creationSlots =
                creationSlots `differenceReversedMap`
                    (Map.restrictKeys creationTxIns prunedTxIns)
            , creationTxIns =
                Map.withoutKeys
                    creationTxIns
                    prunedTxIns
            , spentSlots = leftSpentSlots
            , spentTxIns =
                Map.withoutKeys
                    spentTxIns
                    prunedTxIns
            , tip = tip
            , finality = PrunedUpTo newFinality'
            , boot = boot
            }

-- | Helper to constraint the slot of an AppendBlock.
constrainingAppendBlock :: a -> UTxOHistory -> SlotNo -> a -> a
constrainingAppendBlock noop UTxOHistory{tip} newTip f
    | At newTip <= tip = noop
    | otherwise = f

-- | Helper to constraint the slot of a Rollback.
constrainingRollback :: a -> UTxOHistory -> Slot -> (Maybe Slot -> a) -> a
constrainingRollback noop UTxOHistory{finality, tip} newTip f
    | newTip >= tip = noop
    | otherwise = f $ case finality of
        NotPruned -> Just newTip
        PrunedUpTo finality' ->
            if newTip >= At finality'
                then Just newTip
                else Nothing

-- | Helper to constraint the slot of a Prune.
constrainingPrune :: a -> UTxOHistory -> SlotNo -> (SlotNo -> a) -> a
constrainingPrune noop UTxOHistory{finality, tip} newFinality f =
    fromMaybe noop $ do
        case finality of
            NotPruned -> pure ()
            PrunedUpTo finality' -> guard $ newFinality > finality'
        case tip of
            Origin -> Nothing
            At tip' -> pure $ f $ min newFinality tip'

-- | A datatype that represents the spent time of a TxOut.
data Spent = Spent SlotNo | Unspent
    deriving (Eq, Show)

{-----------------------------------------------------------------------------
    Helper functions
------------------------------------------------------------------------------}
-- | Insert a 'Set' into a 'Map' of 'Set' — but only if the 'Set' is nonempty.
insertNonEmpty
    :: Ord key => key -> Set v -> Map key (Set v) -> Map key (Set v)
insertNonEmpty key x = if null x then id else Map.insert key x

-- | Reverse the roles of key and values for a 'Map' of 'Set's.
reverseMapOfSets :: Ord v => Map k (Set v) -> Map v k
reverseMapOfSets m = Map.fromList $ do
    (k, vs) <- Map.toList m
    v <- Set.toList vs
    pure (v, k)

-- | Insert a 'Set' of items into a 'Map' that is
-- the result of 'reverseMapOfSets'.
insertNonEmptyReversedMap
    :: Ord v => key -> Set v -> Map v key -> Map v key
insertNonEmptyReversedMap key vs m0 =
    foldl' (\m v -> Map.insert v key m) m0 vs

-- | Take the difference between a 'Map' and another 'Map'
-- that was created by using 'reverseMapOfSets'.
differenceReversedMap
    :: (Ord v, Ord key)
    => Map key (Set v) -> Map v key -> Map key (Set v)
differenceReversedMap whole part =
    foldl' (flip delete) whole $ Map.assocs part
  where
    delete (v,key) = Map.update deleteFromSet key
      where
        deleteFromSet vs =
            let vs' = Set.delete v vs
            in  if null vs' then Nothing else Just vs'
