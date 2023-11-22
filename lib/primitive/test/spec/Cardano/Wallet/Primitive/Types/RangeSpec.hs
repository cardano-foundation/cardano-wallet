{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.Types.RangeSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Range
    ( Range (..)
    , RangeBound (..)
    , isSubrangeOf
    )
import Data.Maybe
    ( mapMaybe
    )
import Test.Hspec
    ( Spec
    , it
    , shouldNotSatisfy
    , shouldSatisfy
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , checkCoverage
    , cover
    , infiniteList
    , property
    , withMaxSuccess
    , (.&&.)
    , (=/=)
    , (===)
    )

import qualified Cardano.Wallet.Primitive.Types.Range as Range

spec :: Spec
spec = do

    it "arbitrary ranges are valid" $
        withMaxSuccess 1000 $ property $ \(r :: Range Integer) ->
            checkCoverage $
            cover 10 (Range.isFinite r) "finite range" $
            Range.isValid r .&&.
                all Range.isValid (shrink r)

    it "arbitrary non-singleton ranges are valid" $
        withMaxSuccess 1000 $ property $ \(nsr :: NonSingletonRange Int) ->
            let isValidNonSingleton (NonSingletonRange r) =
                    Range.isValid r && not (Range.isSingleton r)
            in
            checkCoverage $
            cover 10 (Range.isFinite (getNonSingletonRange nsr))
                "finite range" $
            isValidNonSingleton nsr .&&.
                all isValidNonSingleton (shrink nsr)

    it "functions is{Before,Within,After}Range are mutually exclusive" $
        withMaxSuccess 1000 $ property $ \(a :: Integer) r ->
            let options =
                    [ (Range.isBefore)
                    , (Range.isWithin)
                    , (Range.isAfter)
                    ]
            in
            checkCoverage $
            cover 10 (a `Range.isBefore` r) "Range.isBefore" $
            cover 10 (a `Range.isWithin` r) "Range.isWithin" $
            cover 10 (a `Range.isAfter`  r) "Range.isAfter"  $
            1 === length (filter (\f -> f a r) options)

    it "pred (Range.inclusiveLowerBound r) `Range.isBefore` r" $
        withMaxSuccess 1000 $ property $ \(r :: Range Integer) ->
            checkCoverage $
            cover 10 (Range.hasLowerBound r) "has lower bound" $
            ((`Range.isBefore` r) . pred <$> Range.inclusiveLowerBound r)
                =/= Just False

    it "Range.inclusiveLowerBound r `Range.isWithin` r" $
        withMaxSuccess 1000 $ property $ \(r :: Range Integer) ->
            checkCoverage $
            cover 10 (Range.hasLowerBound r) "has lower bound" $
            ((`Range.isWithin` r) <$> Range.inclusiveLowerBound r)
                =/= Just False

    it "Range.inclusiveUpperBound r `Range.isWithin` r" $
        withMaxSuccess 1000 $ property $ \(r :: Range Integer) ->
            checkCoverage $
            cover 10 (Range.hasUpperBound r) "has upper bound" $
            ((`Range.isWithin` r) <$> Range.inclusiveUpperBound r)
                =/= Just False

    it "succ (Range.inclusiveUpperBound r) `Range.isAfter` r" $
        withMaxSuccess 1000 $ property $ \(r :: Range Integer) ->
            checkCoverage $
            cover 10 (Range.hasUpperBound r) "has upper bound" $
            ((`Range.isAfter` r) . succ <$> Range.inclusiveUpperBound r)
                =/= Just False

    it "a `Range.isWithin` Range.everything == True" $
        property $ \(a :: Integer) ->
            a `Range.isWithin` Range.everything === True

    it "Range.isSingleton (Range a a)" $
        property $ \(a :: Int) ->
            Range (Just a) (Just a) `shouldSatisfy` Range.isSingleton

    it "not (Range.isSingleton (Range (pred a) a))" $
        property $ \(a :: Int) ->
            Range (Just (pred a)) (Just a)
                `shouldNotSatisfy` Range.isSingleton

    it "not (Range.isSingleton (Range a (succ a)))" $
        property $ \(a :: Int) ->
            Range (Just a) (Just (succ a))
                `shouldNotSatisfy` Range.isSingleton

    it "Range.lowerBound r = Range.upperBound r <=> Range.isSingleton r" $
        property $ \(r :: Range Bool) ->
            checkCoverage $
            cover 10 (Range.isFinite r) "is finite" $
            (Range.lowerBound r == Range.upperBound r)
                === Range.isSingleton r

    it "r `isSubrangeOf` r" $
        property $ \(r :: Range Int) ->
            r `isSubrangeOf` r

    it "r `isSubrangeOf` Range.everything" $
        property $ \(r :: Range Int) ->
            checkCoverage $
            cover 10 (Range.hasLowerBound r) "has lower bound" $
            cover 10 (Range.hasUpperBound r) "has upper bound" $
            cover 10 (Range.isFinite      r) "is finite" $
            r `isSubrangeOf` Range.everything

    it "Range (succ a) b `isSubrangeOf` Range a b" $
        withMaxSuccess 1000 $ property $ \nsr ->
            let r@(Range a b :: Range Int) = getNonSingletonRange nsr in
            checkCoverage $
            cover 10 (Range.hasLowerBound r) "has lower bound" $
            cover 10 (Range.hasUpperBound r) "has upper bound" $
            cover 10 (Range.isFinite      r) "is finite" $
            Range (succ <$> a) b `isSubrangeOf` Range a b

    it "Range a (pred b) `isSubrangeOf` Range a b" $
        withMaxSuccess 1000 $ property $ \nsr ->
            let r@(Range a b :: Range Int) = getNonSingletonRange nsr in
            checkCoverage $
            cover 10 (Range.hasLowerBound r) "has lower bound" $
            cover 10 (Range.hasUpperBound r) "has upper bound" $
            cover 10 (Range.isFinite      r) "is finite" $
            Range a (pred <$> b) `isSubrangeOf` Range a b

    it "Range a b `isSubrangeOf` Range (pred a) b" $
        property $ \r@(Range a b :: Range Int) ->
            checkCoverage $
            cover 10 (Range.hasLowerBound r) "has lower bound" $
            cover 10 (Range.hasUpperBound r) "has upper bound" $
            cover 10 (Range.isFinite      r) "is finite" $
            Range a b `isSubrangeOf` Range (pred <$> a) b

    it "Range a b `isSubrangeOf` Range a (succ b)" $
        property $ \r@(Range a b :: Range Int) ->
            checkCoverage $
            cover 10 (Range.hasLowerBound r) "has lower bound" $
            cover 10 (Range.hasUpperBound r) "has upper bound" $
            cover 10 (Range.isFinite      r) "is finite" $
            Range a b `isSubrangeOf` Range a (succ <$> b)

    it "NegativeInfinity < InclusiveBound a" $
        property $ \(a :: Int) ->
            NegativeInfinity < InclusiveBound a

    it "InclusiveBound a < PositiveInfinity" $
        property $ \(a :: Int) ->
            InclusiveBound a < PositiveInfinity

    it "compare (InclusiveBound a) (InclusiveBound b) = compare a b" $
        property $ \(a :: Int) (b :: Int) ->
            compare (InclusiveBound a) (InclusiveBound b) === compare a b

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- Ensures that the start of a range is not greater than its end.
makeRangeValid :: Ord a => Range a -> Range a
makeRangeValid = \case
    Range (Just p) (Just q) -> Range (Just $ min p q) (Just $ max p q)
    r -> r

-- Ensures that a range is not a singleton range.
makeNonSingletonRangeValid
    :: Ord a => NonSingletonRange a -> Maybe (NonSingletonRange a)
makeNonSingletonRangeValid (NonSingletonRange r)
    | Range.isSingleton r = Nothing
    | otherwise = Just $ NonSingletonRange $ makeRangeValid r

-- A range that contains more than a single element.
newtype NonSingletonRange a = NonSingletonRange
    { getNonSingletonRange :: Range a
    } deriving Show

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance (Arbitrary a, Ord a) => Arbitrary (Range a) where
    arbitrary =
        makeRangeValid . uncurry Range <$> arbitrary
    shrink (Range p q) =
        makeRangeValid . uncurry Range <$> shrink (p, q)

instance (Arbitrary a, Ord a) => Arbitrary (NonSingletonRange a) where
    arbitrary = do
        -- Iterate through the infinite list of arbitrary ranges and return
        -- the first range that is not a singleton range:
        head . mapMaybe (makeNonSingletonRangeValid . NonSingletonRange)
            <$> infiniteList
    shrink (NonSingletonRange r) = mapMaybe
        (makeNonSingletonRangeValid . NonSingletonRange) (shrink r)
