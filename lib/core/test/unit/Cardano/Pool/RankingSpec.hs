{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Pool.RankingSpec
    ( spec
    ) where

import Prelude

import Cardano.Pool.Ranking
    ( EpochConstants (..)
    , Lovelace (..)
    , Pool (..)
    , Ratio (..)
    , desirability
    , saturatedPoolRewards
    , saturatedPoolSize
    , unsafeMkRelativeStake
    , unsafeToNonNegative
    , unsafeToRatio
    )
import Control.Exception
    ( SomeException, evaluate, try )
import Data.Bifunctor
    ( bimap )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( (.~), (^.) )
import Data.Generics.Labels
    ()
import Data.Generics.Product.Fields
    ( HasField', field' )
import Data.Ord
    ( Ordering (..) )
import Data.Proxy
    ( Proxy (..) )
import Data.Word
    ( Word64 )
import GHC.TypeLits
    ( KnownSymbol, Symbol, symbolVal )
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
    , ioProperty
    , property
    , (.&&.)
    , (===)
    , (==>)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary )

import qualified Cardano.Pool.Ranking as R

spec :: Spec
spec = do
    describe "Stake Pool Ranking" $ do
        describe "all else equal..." $ do
            it "higher margin => lower desirability (or equal)"
                $ property
                $ allElseEqualProperty @"margin" NegativeEffect

            it "higher cost => lower desirability (or equal)"
                $ property
                $ allElseEqualProperty @"cost" NegativeEffect

            it "higher performance => higher desirability (or equal)"
                $ property
                $ allElseEqualProperty @"recentAvgPerformance" PositiveEffect

            it "higher oversaturation => desirability NOT affected"
                $ property prop_oversaturationDoesNotInfluenceDesirability

        describe "desirability" $ do
            it "desirability >= 0"
                $ property prop_desirabilityNonNegative

        describe "saturatedPoolRewards" $ do
            it "a0 == 0 => saturatedPoolRewards == p*R*z0"
                $ property prop_saturatedPoolRewardsReduces

            it "saturatedPoolRewards >= 0"
                $ property prop_saturatedPoolRewardsNonNegative

        describe "mkRelativeStake" $ do
            it "stake > R, R >= 1 => mkRelativeStake ∈ [0,1] (even with conversions)"
                $ property prop_mkRelativeStakeBounds

        describe "unsafeTo- newtypes" $ do
            it "unsafeToRatio requires ∈ [0,1]"
                $ property $ \v -> ioProperty $
                    prop_unsafeTo
                        (\x -> x >= 0 && x <= 1)
                        R.unsafeToRatio
                        R.getRatio
                        v
            it "unsafeToPositive requires > 0" $
                property $ \(v :: Int) -> ioProperty $
                    prop_unsafeTo
                        (> 0)
                        R.unsafeToPositive
                        R.getPositive
                        v
            it "unsafeToNonNegative requires >= 0" $
                property $ \(v :: Double) -> ioProperty $
                    prop_unsafeTo
                        (>= 0)
                        R.unsafeToNonNegative
                        R.getNonNegative
                        v

prop_unsafeTo
    :: (Eq a, Show a)
    => (a -> Bool)
    -> (a -> b)
    -> (b -> a)
    -> a
    -> IO Property
prop_unsafeTo isValid f g x = do
    r <- try . evaluate . f $ x
    let valid = isValid x
    return $
        case r of
            Left (_e::SomeException) -> valid === False
            Right x' -> g x' === x .&&. valid === True

        & classify valid "valid"
        & classify (not valid) "error"

prop_mkRelativeStakeBounds :: EpochConstants -> Lovelace -> Property
prop_mkRelativeStakeBounds constants stake =
    let
        s = getRatio $ unsafeMkRelativeStake stake constants
        total = totalRewards constants
    in
            total >= 1 ==>
                total >= stake ==>
                    property $ s >= 0 && s <= 1
            & counterexample ("relative stake = " ++ show s)

prop_desirabilityNonNegative :: EpochConstants -> Pool -> Property
prop_desirabilityNonNegative constants pool = property $
    desirability constants pool >= 0

prop_saturatedPoolRewardsNonNegative :: EpochConstants -> Pool -> Property
prop_saturatedPoolRewardsNonNegative constants pool = property $
    saturatedPoolRewards constants pool >= 0

prop_saturatedPoolRewardsReduces :: EpochConstants -> Pool -> Property
prop_saturatedPoolRewardsReduces constants' pool =
    let
        constants = constants'
            { leaderStakeInfluence = unsafeToNonNegative 0 }
        z0 = getRatio $ saturatedPoolSize constants
        _R = fromIntegral $ getLovelace $ totalRewards constants
        p  = R.getNonNegative $ recentAvgPerformance pool
    in
        saturatedPoolRewards constants pool === p*_R*z0

prop_oversaturationDoesNotInfluenceDesirability
    :: Pool
    -> EpochConstants
    -> Property
prop_oversaturationDoesNotInfluenceDesirability pool constants =
    forAll arbitrary $ \v1 v2 -> do
        let (higherStake, lowerStake) = sortTuple (v1, v2)
        let z0 = getRatio $ saturatedPoolSize constants
        higherStake > z0 ==>
            des higherStake === des lowerStake
  where
    sortTuple (a, b)
        | a > b     = (a, b)
        | otherwise = (b, a)

    des _stake = desirability constants pool
    -- TODO: set pool{R.stake = stake}, but that field doesn't even exist!

data EffectOnDesirability
    = PositiveEffect
    | NegativeEffect
    deriving (Show, Eq)

-- | All else equal, what effect on the desirability does changing a single
-- field have?
allElseEqualProperty
    :: forall (field :: Symbol) a.
        ( Arbitrary a
        , Show a
        , Ord a
        , HasField' field Pool a
        , KnownSymbol field
        )
    => EffectOnDesirability
        -- ^ Expected effect on the desirability
    -> Pool
       -- ^ The base pool which is later modified
    -> EpochConstants
       -- ^ Constants used
    -> Property
allElseEqualProperty expectedEffect pool constants =
    forAll arbitrary $ \(value1 :: a) (value2 :: a) -> do
        let (poolLower, poolHigher) = sortTuple (value1, value2)
                & bimap poolWith poolWith

        let (dLower, dHigher) =
                ( desirability constants poolLower
                , desirability constants poolHigher
                )

        let poolRankDesc = unlines
                [ "Pools:"
                , showPool poolLower  dLower
                , showPool poolHigher dHigher
                ]

        case dLower `compare` dHigher of
            EQ -> property True
            -- A reason we end up in this case could be that both pools are
            -- un-profitable. It would be interesting to re-run tests with
            -- a higher total reward pot. But as long we hit the other cases we
            -- should be fine, really.
            GT -> expectedEffect === PositiveEffect
            LT -> expectedEffect === NegativeEffect
          & counterexample poolRankDesc
          & classify (dLower == dHigher) "same desirability"
  where
    poolWith :: a -> Pool
    poolWith val = pool & (field' @field) .~ val

    sortTuple :: (a, a) -> (a, a)
    sortTuple (a, b)
        | a > b     = (a, b)
        | otherwise = (b, a)

    showPool :: Pool -> Double -> String
    showPool x d = unwords
        [ show (symbolVal $ Proxy @field)
        , "="
        , show (x ^. field' @field)
        , ", desirability ="
        , show d
        ]

instance Arbitrary Pool where
    arbitrary = genericArbitrary
    shrink _ = []

-- TODO: We should ideally not export the NonNegative and Positive constructors,
-- in which case we wouldn't be able to use DerivingVia.
deriving via Word64 instance (Arbitrary Lovelace)
deriving via (NonNegative Double) instance (Arbitrary (R.NonNegative Double))
deriving via (Positive Int) instance (Arbitrary (R.Positive Int))

instance Arbitrary Ratio where
    arbitrary = unsafeToRatio <$> choose (0, 1)
    shrink = map unsafeToRatio
        . filter (\a -> a >= 0 && a <= 1)
        . map getRatio
        . shrink

instance Arbitrary EpochConstants where
    arbitrary = genericArbitrary
    shrink _ = []
