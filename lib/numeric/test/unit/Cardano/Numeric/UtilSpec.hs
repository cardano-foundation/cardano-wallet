{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Numeric.UtilSpec
    ( spec
    ) where

import Prelude

import Cardano.Numeric.Util
    ( equipartitionNatural, padCoalesce, partitionNatural, power )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( mapMaybe )
import Data.Monoid
    ( Sum (..) )
import Data.Ratio
    ( (%) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..), Property, arbitrarySizedNatural, checkCoverage, property,
    shrink, shrinkIntegral, withMaxSuccess, (.&&.), (.||.), (===) )

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE

spec :: Spec
spec = do

    describe "padCoalesce" $ do

        it "prop_padCoalesce_length" $
            property $ prop_padCoalesce_length @(Sum Int)
        it "prop_padCoalesce_sort" $
            property $ prop_padCoalesce_sort @(Sum Int)
        it "prop_padCoalesce_sum" $
            property $ prop_padCoalesce_sum @(Sum Int)

    describe "equipartitionNatural" $ do

        it "prop_equipartitionNatural_fair" $
            property prop_equipartitionNatural_fair
        it "prop_equipartitionNatural_length" $
            property prop_equipartitionNatural_length
        it "prop_equipartitionNatural_order" $
            property prop_equipartitionNatural_order
        it "prop_equipartitionNatural_sum" $
            property prop_equipartitionNatural_sum

    describe "partitionNatural" $ do

        it "prop_partitionNatural_length" $
            property prop_partitionNatural_length
        it "prop_partitionNatural_sum" $
            property prop_partitionNatural_sum
        it "prop_partitionNatural_fair" $
            withMaxSuccess 1000 $ checkCoverage prop_partitionNatural_fair

    describe "power" $ do

        it "equivalent to (^)" $ do
            2 `power` 8
                `shouldBe` (256 :: Int)
            2 `power` 8 - 1
                `shouldBe` (255 :: Int)
            2 `power` 8 + 1
                `shouldBe` (257 :: Int)

--------------------------------------------------------------------------------
-- Coalescing values
--------------------------------------------------------------------------------

prop_padCoalesce_length
    :: (Monoid a, Ord a) => NonEmpty a -> NonEmpty () -> Property
prop_padCoalesce_length source target =
    NE.length (padCoalesce source target) === NE.length target

prop_padCoalesce_sort
    :: (Monoid a, Ord a, Show a) => NonEmpty a -> NonEmpty () -> Property
prop_padCoalesce_sort source target =
    NE.sort result === result
  where
    result = padCoalesce source target

prop_padCoalesce_sum
    :: (Monoid a, Ord a, Show a) => NonEmpty a -> NonEmpty () -> Property
prop_padCoalesce_sum source target =
    F.fold source === F.fold (padCoalesce source target)

--------------------------------------------------------------------------------
-- Equipartitioning natural numbers
--------------------------------------------------------------------------------

-- Test that natural numbers are equipartitioned fairly:
--
-- Each portion must be within unity of the ideal portion.
--
prop_equipartitionNatural_fair
    :: Natural -> NonEmpty () -> Property
prop_equipartitionNatural_fair n count = (.||.)
    (difference === 0)
    (difference === 1)
  where
    difference :: Natural
    difference = F.maximum results - F.minimum results

    results :: NonEmpty Natural
    results = equipartitionNatural n count

prop_equipartitionNatural_length :: Natural -> NonEmpty () -> Property
prop_equipartitionNatural_length n count =
    NE.length (equipartitionNatural n count) === NE.length count

prop_equipartitionNatural_order :: Natural -> NonEmpty () -> Property
prop_equipartitionNatural_order n count =
    NE.sort results === results
  where
    results = equipartitionNatural n count

prop_equipartitionNatural_sum :: Natural -> NonEmpty () -> Property
prop_equipartitionNatural_sum n count =
    F.sum (equipartitionNatural n count) === n

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
    shrink xs = mapMaybe NE.nonEmpty (shrink (NE.toList xs))

instance Arbitrary Natural where
    arbitrary = arbitrarySizedNatural
    shrink = shrinkIntegral
