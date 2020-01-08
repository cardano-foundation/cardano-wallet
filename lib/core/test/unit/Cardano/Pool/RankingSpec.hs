{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cardano.Pool.RankingSpec
    ( spec
    ) where

import Prelude

import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( Lens', view, (.~) )
import Data.Generics.Labels
    ()
import Data.Ord
    ( Ordering (..) )
import Data.Word
    ( Word64 )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , NonNegative (..)
    , Positive (..)
    , Property
    , choose
    , classify
    , counterexample
    , forAll
    , property
    , (===)
    , (==>)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )

import qualified Cardano.Pool.Ranking as R

spec :: Spec
spec = do
    describe "Stake Pool Ranking" $ do
        describe "all else equal..." $ do
            it "higher margin => lower desirability (or equal)" $
                property $
                    allElseEqualProperty
                        "margin"
                        #margin
                        (show . R.getMargin)
                        NegativeEffect

            it "higher cost => lower desirability (or equal)" $
                property $ do
                    allElseEqualProperty
                        "cost"
                        #cost
                        (show . R.getLovelace)
                        NegativeEffect

            it "higher performance => higher desirability (or equal)" $
                property $
                    allElseEqualProperty
                        "performance"
                        #recentAvgPerformance
                        (show . R.getNonNegative)
                        PositiveEffect

            it "higher oversaturation => desirability NOT affected" $
                property prop_oversaturationDoesNotInfluenceDesirability

        describe "desiraility" $ do
            it "desirability >= 0"
                (property prop_desirabilityNonNegative)

        describe "saturatedPoolRewards" $ do
            it "a0 == 0 => saturatedPoolRewards == p*R*z0"
                (property prop_saturatedPoolRewardsReduces)
            it "saturatedPoolRewards >= 0"
                (property prop_saturatedPoolRewardsNonNegative)

        describe "mkRelativeStake" $ do
            it "stake > R, R >= 1 => mkRelativeStake ∈ [0,1] (even with conversions)"
                $ property prop_mkRelativeStakeBounds

prop_mkRelativeStakeBounds :: R.EpochConstants -> Word64 -> Word64 -> Property
prop_mkRelativeStakeBounds constants' tot stake =
    let
        rel = R.unRelativeStake
            $ R.mkRelativeStake
                (R.Lovelace $ fromIntegral stake)
                constants
    in
        counterexample ("relative stake = " ++ show rel) $
            tot >= 1 ==>
                tot >= stake ==>
                    property $ rel >= 0 && rel <= 1

  where
    constants = constants'{R.totalRewards = R.Lovelace . R.unsafeToNonNegative $ fromIntegral tot}

prop_desirabilityNonNegative :: R.EpochConstants -> R.Pool -> Property
prop_desirabilityNonNegative constants pool = property $
    R.desirability constants pool >= 0

prop_saturatedPoolRewardsNonNegative :: R.EpochConstants -> R.Pool -> Property
prop_saturatedPoolRewardsNonNegative constants pool = property $
    R.saturatedPoolRewards constants pool >= 0

prop_saturatedPoolRewardsReduces :: R.EpochConstants -> R.Pool -> Property
prop_saturatedPoolRewardsReduces constants' pool =
    let
        constants = constants'{R.leaderStakeInfluence = R.unsafeToNonNegative 0 }
        z0 = R.unRelativeStake $ R.saturatedPoolSize constants
        _R =  R.getNonNegative $ R.getLovelace $ R.totalRewards constants
        p = R.getNonNegative $ R.recentAvgPerformance pool
    in
        R.saturatedPoolRewards constants pool === p*_R*z0

prop_oversaturationDoesNotInfluenceDesirability
    :: R.Pool
    -> R.EpochConstants
    -> Property
prop_oversaturationDoesNotInfluenceDesirability pool constants =
    forAll arbitrary $ \v1 v2 -> do
        let (higherStake, lowerStake) = sortTuple (v1, v2)
        let z0 = R.unRelativeStake $ R.saturatedPoolSize constants
        higherStake > z0 ==>
            des higherStake === des lowerStake
  where
    sortTuple (a, b)
        | a > b     = (a,b)
        | otherwise = (b, a)

    des _stake = R.desirability constants pool
    -- TODO: set pool{R.stake = stake}, but that field doesn't even exist!

data EffectOnDesirability
    = PositiveEffect
    | NegativeEffect
    deriving (Show, Eq)

-- | All else equal, what effect on the desirability does changing a single
-- field have?
allElseEqualProperty
    :: (Arbitrary a, Show a, Ord a)
    => String
    -> Lens' R.Pool a
    -> (a -> String)
       -- ^ Debug print a field-value
    -> EffectOnDesirability
    -> R.Pool
       -- ^ The base pool which is later modified
    -> R.EpochConstants
       -- ^ Constants used
    -> Property
allElseEqualProperty desc field showValue eff p constants =
    forAll arbitrary $ \value1 value2 -> do
        let (higherVal, lowerVal) = sortTuple (value1, value2)

        let poolRankDesc = "Pools:\n" ++ showPool 1 (poolWith higherVal) ++ "\n" ++ showPool 2 (poolWith lowerVal)

        case des (poolWith higherVal) `compare` des (poolWith lowerVal) of
            GT -> eff === PositiveEffect
            EQ -> property True
            -- A reason we end up in this case could be that both pools are
            -- un-profitable. It would be interesting to re-run tests with
            -- a higher total reward pot. But as long we hit the other cases we
            -- should be fine, really.

            LT -> eff === NegativeEffect

            & counterexample poolRankDesc
            & classify
                    (des (poolWith higherVal) == des (poolWith lowerVal))
                    "equal desirability"

  where
    poolWith val = p & field .~ val
    sortTuple (a, b)
        | a > b = (a,b)
        | otherwise     = (b, a)

    showPool :: Int -> R.Pool -> String
    showPool rank x =
        show rank
            ++ ". "
            ++ desc
            ++ " = "
            ++ showValue (view field x)
            ++ ", desirability = "
            ++ show (des x)

    des = R.desirability constants

instance Arbitrary R.Pool where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (R.RelativeStakeOf a) where
    arbitrary = R.RelativeStake <$> choose (0, 1)
    shrink (R.RelativeStake x) = map R.RelativeStake $ filter (\a -> a >= 0 && a <= 1) $ shrink x

-- TODO: We should ideally not export the NonNegative and Positive constructors,
-- in which case we wouldn't be able to use derivingVia.
deriving via (NonNegative Double) instance (Arbitrary R.Lovelace)
deriving via (NonNegative Double) instance (Arbitrary (R.NonNegative Double))
deriving via (Positive Int) instance (Arbitrary (R.Positive Int))

instance Arbitrary R.Margin where
    arbitrary = R.unsafeMkMargin <$> choose (0, 1)
    shrink = map R.unsafeMkMargin
        . filter (\a -> a >= 0 && a <= 1)
        . map R.getMargin
        . shrink

instance Arbitrary R.EpochConstants where
    arbitrary = genericArbitrary
    shrink = genericShrink
