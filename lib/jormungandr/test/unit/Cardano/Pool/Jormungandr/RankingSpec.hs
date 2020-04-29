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

module Cardano.Pool.Jormungandr.RankingSpec
    ( spec
    ) where

import Prelude

import Cardano.Pool.Jormungandr.Ranking
    ( EpochConstants (..)
    , Pool (..)
    , desirability
    , saturatedPoolRewards
    , saturatedPoolSize
    , unsafeMkNonNegative
    )
import Cardano.Wallet.Unsafe
    ( unsafeMkPercentage )
import Control.Exception
    ( SomeException, evaluate, try )
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
import Data.Quantity
    ( Percentage (..), Quantity (..), percentageToDouble )
import Data.Word
    ( Word64 )
import GHC.TypeLits
    ( KnownSymbol, Symbol, symbolVal )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
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
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )

import qualified Cardano.Pool.Jormungandr.Ranking as R

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

        describe "desirability" $ do
            it "desirability >= 0"
                $ property prop_desirabilityNonNegative

        describe "saturatedPoolRewards" $ do
            it "a0 == 0 => saturatedPoolRewards == p*R*z0"
                $ property prop_saturatedPoolRewardsReduces

            it "saturatedPoolRewards >= 0"
                $ property prop_saturatedPoolRewardsNonNegative

        describe "unsafeMk- newtypes" $ do
            it "unsafeMkPositive requires > 0" $
                property $ \(v :: Int) -> ioProperty $
                    prop_unsafeMk
                        (> 0)
                        R.unsafeMkPositive
                        R.getPositive
                        v
            it "unsafeMkNonNegative requires >= 0" $
                property $ \(v :: Double) -> ioProperty $
                    prop_unsafeMk
                        (>= 0)
                        R.unsafeMkNonNegative
                        R.getNonNegative
                        v

prop_unsafeMk
    :: (Eq a, Show a)
    => (a -> Bool)
    -> (a -> b)
    -> (b -> a)
    -> a
    -> IO Property
prop_unsafeMk isValid f g x = do
    r <- try . evaluate . f $ x
    let valid = isValid x
    return $
        case r of
            Left (_e::SomeException) -> valid === False
            Right x' -> g x' === x .&&. valid === True

        & classify valid "valid"
        & classify (not valid) "error"

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
            { leaderStakeInfluence = unsafeMkNonNegative 0 }
        z0 = percentageToDouble $ saturatedPoolSize constants
        _R = fromIntegral $ getQuantity $ totalRewards constants
        p  = R.getNonNegative $ recentAvgPerformance pool
    in
        saturatedPoolRewards constants pool === p*_R*z0

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
        let (higherVal, lowerVal) = sortTupleOn id (value1, value2)

        let (moreDesPool, lessDesPool) = sortTupleOn snd
                ( poolDesPairWith value1
                , poolDesPairWith value2
                )

        let poolRankDesc = unlines
                [ "Pools:"
                , showPool moreDesPool
                , showPool lessDesPool
                ]

        case des higherVal `compare` des lowerVal of
            EQ -> property True
            -- A reason we end up in this case could be that both pools are
            -- un-profitable. It would be interesting to re-run tests with
            -- a higher total reward pot. But as long we hit the other cases we
            -- should be fine, really.
            GT -> expectedEffect === PositiveEffect
            LT -> expectedEffect === NegativeEffect
          & counterexample poolRankDesc
          & classify (des higherVal == des lowerVal) "same desirability"
  where
    poolWith :: a -> Pool
    poolWith val = pool & (field' @field) .~ val

    poolDesPairWith :: a -> (Pool, Double)
    poolDesPairWith val = (poolWith val, desirability constants (poolWith val))

    sortTupleOn :: (Ord c) => (b -> c) -> (b, b) -> (b, b)
    sortTupleOn f (a, b)
        | (f a) > (f b) = (a, b)
        | otherwise     = (b, a)

    showPool :: (Pool, Double) -> String
    showPool (x, d) = unwords
        [ show (symbolVal $ Proxy @field)
        , "="
        , show (x ^. field' @field)
        , ", desirability ="
        , show d
        ]

    des val = desirability constants (poolWith val)

instance Arbitrary Pool where
    arbitrary = genericArbitrary
    shrink = genericShrink

-- TODO: We should ideally not export the NonNegative and Positive constructors,
-- in which case we wouldn't be able to use DerivingVia.
deriving via Word64 instance (Arbitrary (Quantity "lovelace" Word64))
deriving via (NonNegative Double) instance (Arbitrary (R.NonNegative Double))
deriving via (Positive Int) instance (Arbitrary (R.Positive Int))

instance Arbitrary Percentage where
    arbitrary = genPercentage
    shrink _ = [] -- TODO: Implement a shrinker that works.

instance Arbitrary EpochConstants where
    arbitrary = genericArbitrary
    shrink = genericShrink

genPercentage :: Gen Percentage
genPercentage = unsafeMkPercentage . fromRational . toRational <$> genDouble
  where
    genDouble :: Gen Double
    genDouble = choose (0, 1)
