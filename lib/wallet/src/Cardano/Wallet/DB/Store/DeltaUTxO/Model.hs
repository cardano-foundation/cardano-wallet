{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.DB.Store.DeltaUTxO.Model
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
import Cardano.Wallet.DB.Store.DeltaUTxO.Model.Internal
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

reverseMapOfSets :: Ord v => Map k (Set v) -> Map v k
reverseMapOfSets m = Map.fromList $ do
    (k, vs) <- Map.toList m
    v <- Set.toList vs
    pure (v, k)

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

onNotNull :: Foldable t => t a -> b -> (t a -> b -> b) -> b
onNotNull x d f =
    if null x
        then d
        else f x d

-- how to apply a DeltaUTxOHistory to a UTxOHistory
instance Delta DeltaUTxOHistory where
    type Base DeltaUTxOHistory = UTxOHistory
    apply
        (AppendBlock newTip delta)
        noop@UTxOHistory
            { history
            , spentSlots
            , creationSlots
            , creationTxIns
            , spentTxIns
            , finality
            , boot
            } =
            constrainingAppendBlock noop noop newTip $
                UTxOHistory
                    { history = history <> received delta
                    , creationSlots = onNotNull
                        (dom $ received delta)
                        creationSlots
                        $ \received' ->
                            Map.insert
                                (At newTip)
                                (received' `Set.difference` dom history)
                    , creationTxIns =
                        foldl'
                            (\m txIn -> Map.insert txIn (At newTip) m)
                            creationTxIns
                            $ dom
                            $ received delta
                    , spentSlots = onNotNull
                        ( (excluded delta `Set.intersection` dom history)
                            `Set.difference` fold spentSlots
                        )
                        spentSlots
                        $ \excluded' -> Map.insert newTip excluded'
                    , spentTxIns =
                        foldl'
                            (\m txIn -> Map.insert txIn newTip m)
                            spentTxIns
                            (excluded delta)
                    , tip = At newTip
                    , finality = finality
                    , boot = boot
                    }
    apply
        (Rollback newTip)
        noop@UTxOHistory
            { history
            , spentSlots
            , creationSlots
            , creationTxIns
            , spentTxIns
            , finality
            , boot
            } = constrainingRollback noop noop newTip $ \case
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
    apply
        (Prune newFinality)
        noop@UTxOHistory
            { history
            , spentSlots
            , creationSlots
            , creationTxIns
            , spentTxIns
            , tip
            , boot
            } = constrainingPrune noop noop newFinality $ \newFinality' ->
            let
                (prunedSpentSlots, leftSpentSlots) =
                    Map.spanAntitone
                        (<= newFinality')
                        spentSlots
                prunedTxIns = fold prunedSpentSlots
                fixCreationSlot
                    (txIn, slotNo) = Map.alter f slotNo
                      where
                        f Nothing = Nothing
                        f (Just txIns) =
                            let
                                txIns' = Set.delete txIn txIns
                            in
                                if null txIns'
                                    then Nothing
                                    else Just txIns'
            in
                UTxOHistory
                    { history = history `excluding` prunedTxIns
                    , creationSlots =
                        foldl'
                            (flip fixCreationSlot)
                            creationSlots
                            $ Map.assocs
                            $ Map.restrictKeys creationTxIns prunedTxIns
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
constrainingAppendBlock noop UTxOHistory {tip} newTip f
    | At newTip <= tip = noop
    | otherwise = f

-- | Helper to constraint the slot of a Rollback.
constrainingRollback :: a -> UTxOHistory -> Slot -> (Maybe Slot -> a) -> a
constrainingRollback noop UTxOHistory {finality, tip} newTip f
    | newTip >= tip = noop
    | otherwise = f $ case finality of
        NotPruned -> Just newTip
        PrunedUpTo finality' ->
            if newTip >= At finality'
                then Just newTip
                else Nothing

-- | Helper to constraint the slot of a Prune.
constrainingPrune :: a -> UTxOHistory -> SlotNo -> (SlotNo -> a) -> a
constrainingPrune noop UTxOHistory {finality, tip} newFinality f =
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
