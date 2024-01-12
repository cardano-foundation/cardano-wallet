{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

-- |
-- Copyright: © 2023 Cardano Foundation
--
module Data.Map.Strict.ExtraSpec
    ( spec
    ) where

import Prelude hiding
    ( filter
    )

import Data.Function
    ( (&)
    )
import Data.Map.Strict
    ( Map
    , filter
    , fromList
    , intersectionWith
    )
import Data.Map.Strict.Extra
    ( conflicts
    )
import Data.Tuple
    ( swap
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Property
    , property
    , (===)
    )
import Test.QuickCheck.Property
    ( checkCoverage
    , cover
    )

spec :: Spec
spec = do
    describe "conflicts" $ do
        it "example_conflicts" $
            example_conflicts
                & property
        it "prop_conflicts_intersection_filter" $
            prop_conflicts_intersection_filter
                & property
        it "prop_conflicts_swap" $
            prop_conflicts_swap
                & property

example_conflicts :: Property
example_conflicts =
    conflicts
        (fromList [('a', 1), ('b', 1), ('c', 1), ('d', 1)          ])
        (fromList [          ('b', 1), ('c', 2), ('d', 1), ('e', 1)])
    ===
        (fromList [('c', (1, 2))] :: Map Char (Int, Int))

prop_conflicts_intersection_filter :: Map Int Int -> Map Int Int -> Property
prop_conflicts_intersection_filter m1 m2 =
    conflicts m1 m2 === filter (uncurry (/=)) (intersectionWith (,) m1 m2)
    & cover 10
        (conflicts m1 m2 /= mempty)
        "conflicts m1 m2 /= mempty"
    & checkCoverage

prop_conflicts_swap :: Map Int Int -> Map Int Int -> Property
prop_conflicts_swap m1 m2 =
    conflicts m1 m2 === fmap swap (conflicts m2 m1)
    & cover 10
        (conflicts m1 m2 /= mempty)
        "conflicts m1 m2 /= mempty"
    & checkCoverage
