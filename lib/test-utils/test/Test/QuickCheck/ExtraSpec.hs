{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.QuickCheck.ExtraSpec
    where

import Prelude

import Control.Monad
    ( forM_ )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Data.Set
    ( Set )
import GHC.Generics
    ( Generic )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Property, Testable, checkCoverage, cover, property, (===) )
import Test.QuickCheck.Extra
    ( Pretty (..), interleaveRoundRobin )

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Set as Set

spec :: Spec
spec = describe "Test.QuickCheck.ExtraSpec" $ do

    describe "interleaveRoundRobin" $ do
        it "prop_interleaveRoundRobin_length" $
            property prop_interleaveRoundRobin_length
        it "prop_interleaveRoundRobin_sort" $
            property prop_interleaveRoundRobin_sort
        unit_interleaveRoundRobin

--------------------------------------------------------------------------------
-- Round-robin interleaving of lists
--------------------------------------------------------------------------------

prop_interleaveRoundRobin_coverage :: Testable p => [[Int]] -> p -> Property
prop_interleaveRoundRobin_coverage xs p =
    checkCoverage $
    cover 50
        (L.length xs >= 3)
        "have at least three lists" $
    cover 50
        (Set.size listLengths > 1)
        "lists have different lengths" $
    cover 50
        (L.sort allElements /= allElements)
        "list elements not in sorted order" $
    property p
  where
    allElements :: [Int]
    allElements = F.fold xs

    listLengths :: Set Int
    listLengths = Set.fromList (L.length <$> xs)

prop_interleaveRoundRobin_length :: [[Int]] -> Property
prop_interleaveRoundRobin_length xs =
    prop_interleaveRoundRobin_coverage xs $
    L.length (interleaveRoundRobin xs) === L.length (F.fold xs)

prop_interleaveRoundRobin_sort :: [[Int]] -> Property
prop_interleaveRoundRobin_sort xs =
    prop_interleaveRoundRobin_coverage xs $
    L.sort (interleaveRoundRobin xs) === L.sort (F.fold xs)

unit_interleaveRoundRobin :: Spec
unit_interleaveRoundRobin = unitTests
    "unit_interleaveRoundRobin"
    interleaveRoundRobin
    tests
  where
    tests :: [UnitTestData [[Int]] [Int]]
    tests =
        [ UnitTestData
            { params = []
            , result = []
            }
        , UnitTestData
            { params = [[1, 2, 3, 4]]
            , result = [1, 2, 3, 4]
            }
        , UnitTestData
            { params = [[1, 2, 3, 4], []]
            , result = [1, 2, 3, 4]
            }
        , UnitTestData
            { params = [[], [1, 2, 3, 4]]
            , result = [1, 2, 3, 4]
            }
        , UnitTestData
            { params = [[1, 2], [3, 4]]
            , result = [1, 3, 2, 4]
            }
        , UnitTestData
            { params = [[1, 2, 3], [4, 5], [6]]
            , result = [1, 4, 6, 2, 5, 3]
            }
        , UnitTestData
            { params = [[1], [2, 3], [4, 5, 6]]
            , result = [1, 2, 4, 3, 5, 6]
            }
        ]

--------------------------------------------------------------------------------
-- Unit test support
--------------------------------------------------------------------------------

data UnitTestData params result = UnitTestData
    { params :: params
    , result :: result
    }
    deriving (Eq, Generic, Show)

unitTests
    :: (Eq result, Show result)
    => String
    -> (params -> result)
    -> [UnitTestData params result]
    -> Spec
unitTests title f unitTestData =
    describe title $
    forM_ (zip testNumbers unitTestData) $
        \(testNumber :: Int, test) -> do
            let subtitle = "Unit test #" <> show testNumber
            it subtitle $
                let resultExpected = view #result test in
                let resultActual = f (view #params test) in
                property $ Pretty resultExpected === Pretty resultActual
  where
    testNumbers :: [Int]
    testNumbers = [1 ..]
