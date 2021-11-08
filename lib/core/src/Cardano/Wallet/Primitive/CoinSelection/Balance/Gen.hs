{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.CoinSelection.Balance.Gen
    ( genSelectionLimit
    , genSelectionSkeleton
    , shrinkSelectionLimit
    , shrinkSelectionSkeleton
    )
    where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection.Balance
    ( SelectionLimit, SelectionLimitOf (..), SelectionSkeleton (..) )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetId, shrinkAssetId )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxOut, shrinkTxOut )
import Generics.SOP
    ( NP (..) )
import Test.QuickCheck
    ( Gen
    , NonNegative (..)
    , arbitrary
    , listOf
    , oneof
    , shrink
    , shrinkList
    , shrinkMapBy
    )
import Test.QuickCheck.Extra
    ( genericRoundRobinShrink, (<:>), (<@>) )

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
shrinkSelectionSkeleton = genericRoundRobinShrink
    <@> shrinkSkeletonInputCount
    <:> shrinkSkeletonOutputs
    <:> shrinkSkeletonChange
    <:> Nil
  where
    shrinkSkeletonInputCount =
        shrink @Int
    shrinkSkeletonOutputs =
        shrinkList shrinkTxOut
    shrinkSkeletonChange =
        shrinkList $
        shrinkMapBy Set.fromList Set.toList (shrinkList shrinkAssetId)
