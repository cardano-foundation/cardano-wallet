{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.CoinSelection.Internal.Balance.Gen
    ( genSelectionLimit
    , genSelectionSkeleton
    , shrinkSelectionLimit
    , shrinkSelectionSkeleton
    )
    where

import Prelude

import Cardano.Wallet.CoinSelection.Internal.Balance
    ( SelectionLimit, SelectionLimitOf (..), SelectionSkeleton (..) )
import Cardano.Wallet.Primitive.Types.Address.Gen
    ( genAddress, shrinkAddress )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRange, shrinkTokenBundleSmallRange )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetId, shrinkAssetId )
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
    , suchThat
    )
import Test.QuickCheck.Extra
    ( genericRoundRobinShrink, (<:>), (<@>) )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
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
        listOf genSkeletonOutput
    genSkeletonOutput = (,)
        <$> genAddress
        <*> genTokenBundleSmallRange `suchThat` tokenBundleHasNonZeroCoin
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
        shrinkList shrinkSkeletonOutput
    shrinkSkeletonOutput =
        genericRoundRobinShrink
            <@> shrinkAddress
            <:> filter tokenBundleHasNonZeroCoin . shrinkTokenBundleSmallRange
            <:> Nil
    shrinkSkeletonChange =
        shrinkList $
        shrinkMapBy Set.fromList Set.toList (shrinkList shrinkAssetId)

tokenBundleHasNonZeroCoin :: TokenBundle -> Bool
tokenBundleHasNonZeroCoin b = TokenBundle.getCoin b /= Coin 0
