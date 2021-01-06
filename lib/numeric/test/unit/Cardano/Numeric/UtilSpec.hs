{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Numeric.UtilSpec
    ( spec
    ) where

import Prelude

import Cardano.Numeric.Util
    ( partitionNatural )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( catMaybes )
import Data.Ratio
    ( (%) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , arbitrarySizedNatural
    , checkCoverage
    , property
    , shrink
    , shrinkIntegral
    , withMaxSuccess
    , (.&&.)
    , (===)
    )

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE

spec :: Spec
spec = do

    describe "partitionNatural" $ do

        it "prop_partitionNatural_length" $
            property prop_partitionNatural_length
        it "prop_partitionNatural_sum" $
            property prop_partitionNatural_sum
        it "prop_partitionNatural_fair" $
            withMaxSuccess 1000 $ checkCoverage prop_partitionNatural_fair

--------------------------------------------------------------------------------
-- Partitioning natural numbers
--------------------------------------------------------------------------------

prop_partitionNatural_length
    :: Natural
    -> NonEmpty Natural
    -> Property
prop_partitionNatural_length target weights =
    case partitionNatural target weights of
        Nothing -> F.sum weights === 0
        Just ps -> F.length ps === F.length weights

prop_partitionNatural_sum
    :: Natural
    -> NonEmpty Natural
    -> Property
prop_partitionNatural_sum target weights =
    case partitionNatural target weights of
        Nothing -> F.sum weights === 0
        Just ps -> F.sum ps === target

-- | Check that portions are all within unity of ideal unrounded portions.
--
prop_partitionNatural_fair
    :: Natural
    -> NonEmpty Natural
    -> Property
prop_partitionNatural_fair target weights =
    case partitionNatural target weights of
        Nothing -> F.sum weights === 0
        Just ps -> prop ps
  where
    prop portions = (.&&.)
        (F.all (uncurry (<=)) (NE.zip portions portionUpperBounds))
        (F.all (uncurry (>=)) (NE.zip portions portionLowerBounds))
      where
        portionUpperBounds = ceiling . computeIdealPortion <$> weights
        portionLowerBounds = floor   . computeIdealPortion <$> weights

        computeIdealPortion :: Natural -> Rational
        computeIdealPortion c
            = fromIntegral target
            * fromIntegral c
            % fromIntegral totalWeight

        totalWeight :: Natural
        totalWeight = F.sum weights

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (NE.NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
    shrink xs = catMaybes $ NE.nonEmpty <$> shrink (NE.toList xs)

instance Arbitrary Natural where
    arbitrary = arbitrarySizedNatural
    shrink = shrinkIntegral
