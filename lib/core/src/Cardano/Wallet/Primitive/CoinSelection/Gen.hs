{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.CoinSelection.Gen
    ( genSelectionLimit
    , genSelectionSkeleton
    , genSelectionState
    , shrinkSelectionLimit
    , shrinkSelectionSkeleton
    , shrinkSelectionState
    )
    where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection.Balance
    ( SelectionLimit
    , SelectionLimitOf (..)
    , SelectionSkeleton (..)
    , SelectionState (..)
    )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetId, shrinkAssetId )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxOut, shrinkTxOut )
import Cardano.Wallet.Primitive.Types.UTxOIndex.Gen
    ( genUTxOIndex, shrinkUTxOIndex )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( over, view )
import Test.QuickCheck
    ( Gen
    , NonNegative (..)
    , arbitrary
    , liftShrink2
    , listOf
    , oneof
    , shrink
    , shrinkList
    , shrinkMapBy
    )
import Test.QuickCheck.Extra
    ( liftShrink3 )

import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Selection limits
--------------------------------------------------------------------------------

genSelectionLimit :: Gen SelectionLimit
genSelectionLimit = oneof
    [ MaximumInputLimit . getNonNegative <$> arbitrary
    , pure NoLimit
    ]

shrinkSelectionLimit :: SelectionLimit -> [SelectionLimit]
shrinkSelectionLimit = \case
    MaximumInputLimit n ->
        MaximumInputLimit . getNonNegative <$> shrink (NonNegative n)
    NoLimit ->
        []

--------------------------------------------------------------------------------
-- Selection skeletons
--------------------------------------------------------------------------------

genSelectionSkeleton :: Gen SelectionSkeleton
genSelectionSkeleton = SelectionSkeleton
    <$> genSkeletonInputCount
    <*> genSkeletonOutputs
    <*> genSkeletonChange
  where
    genSkeletonInputCount =
        getNonNegative <$> arbitrary @(NonNegative Int)
    genSkeletonOutputs =
        listOf genTxOut
    genSkeletonChange =
        listOf (Set.fromList <$> listOf genAssetId)

shrinkSelectionSkeleton :: SelectionSkeleton -> [SelectionSkeleton]
shrinkSelectionSkeleton =
    shrinkMapBy tupleToSkeleton skeletonToTuple $ liftShrink3
        shrinkSkeletonInputCount
        shrinkSkeletonOutputs
        shrinkSkeletonChange
  where
    shrinkSkeletonInputCount =
        shrink @Int
    shrinkSkeletonOutputs =
        shrinkList shrinkTxOut
    shrinkSkeletonChange =
        shrinkList $
        shrinkMapBy Set.fromList Set.toList (shrinkList shrinkAssetId)

    skeletonToTuple (SelectionSkeleton a b c) = (a, b, c)
    tupleToSkeleton (a, b, c) = (SelectionSkeleton a b c)

--------------------------------------------------------------------------------
-- Selection states
--------------------------------------------------------------------------------

genSelectionState :: Gen SelectionState
genSelectionState =
    makeSelectionStateValid <$> genSelectionStateUnvalidated
  where
    genSelectionStateUnvalidated :: Gen SelectionState
    genSelectionStateUnvalidated = SelectionState
        <$> genUTxOIndex
        <*> genUTxOIndex

shrinkSelectionState :: SelectionState -> [SelectionState]
shrinkSelectionState = fmap makeSelectionStateValid <$>
    shrinkMapBy tupleToState stateToTuple
        (liftShrink2 shrinkUTxOIndex shrinkUTxOIndex)
  where
    stateToTuple (SelectionState a b) = (a, b)
    tupleToState (a, b) = (SelectionState a b)

makeSelectionStateValid :: SelectionState -> SelectionState
makeSelectionStateValid state = state
    & over #leftover (`UTxOIndex.difference` view #selected state)
