{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.QuickCheck.ExtraSpec
    where

import Prelude

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Control.Monad
    ( forM_ )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Data.Set
    ( Set )
import Generics.SOP
    ( NP (Nil) )
import GHC.Generics
    ( Generic )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Gen
    , Property
    , Testable
    , checkCoverage
    , chooseInteger
    , cover
    , forAll
    , property
    , shrinkIntegral
    , (===)
    )
import Test.QuickCheck.Extra
    ( Pretty (..)
    , genericRoundRobinShrink
    , interleaveRoundRobin
    , (<:>)
    , (<@>)
    )

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.Extra as L
import qualified Data.Set as Set

spec :: Spec
spec = describe "Test.QuickCheck.ExtraSpec" $ do

    describe "genericRoundRobinShrink" $ do
        it "prop_genericRoundRobinShrink_equivalence" $
            property prop_genericRoundRobinShrink_equivalence
        it "prop_genericRoundRobinShrink_minimization" $
            property prop_genericRoundRobinShrink_minimization
        it "prop_genericRoundRobinShrink_progression" $
            property prop_genericRoundRobinShrink_progression
        it "prop_genericRoundRobinShrink_uniqueness" $
            property prop_genericRoundRobinShrink_uniqueness

    describe "interleaveRoundRobin" $ do
        it "prop_interleaveRoundRobin_length" $
            property prop_interleaveRoundRobin_length
        it "prop_interleaveRoundRobin_sort" $
            property prop_interleaveRoundRobin_sort
        unit_interleaveRoundRobin

--------------------------------------------------------------------------------
-- Generic shrinking
--------------------------------------------------------------------------------

data TestRecord = TestRecord
    { fieldA :: Integer
    , fieldB :: Integer
    , fieldC :: Integer
    }
    deriving (Eq, Generic, Show)

instance PartialOrd TestRecord where
    r1 `leq` r2 =
        fieldA r1 <= fieldA r2 &&
        fieldB r1 <= fieldB r2 &&
        fieldC r1 <= fieldC r2

genTestRecord :: Gen TestRecord
genTestRecord = TestRecord
    <$> genInteger
    <*> genInteger
    <*> genInteger
  where
    genInteger :: Gen Integer
    genInteger = chooseInteger (0, 1_000_000_000_000)

shrinkTestRecordGenerically :: TestRecord -> [TestRecord]
shrinkTestRecordGenerically = genericRoundRobinShrink
    <@> shrinkIntegral
    <:> shrinkIntegral
    <:> shrinkIntegral
    <:> Nil

shrinkTestRecordManually :: TestRecord -> [TestRecord]
shrinkTestRecordManually (TestRecord a1 a2 a3) =
    interleaveRoundRobin
        [ [ TestRecord a1' a2  a3  | a1' <- shrinkIntegral a1 ]
        , [ TestRecord a1  a2' a3  | a2' <- shrinkIntegral a2 ]
        , [ TestRecord a1  a2  a3' | a3' <- shrinkIntegral a3 ]
        ]

minimalTestRecord :: TestRecord
minimalTestRecord = TestRecord 0 0 0

prop_genericRoundRobinShrink_coverage
    :: Testable p
    => TestRecord
    -> p
    -> Property
prop_genericRoundRobinShrink_coverage r p =
    checkCoverage $
    cover 50
        (r /= minimalTestRecord)
        "record is not already minimal" $
    cover 50
        (shrinkResultsGeneric /= [])
        "shrunken list is not empty (when shrunk generically)" $
    cover 50
        (shrinkResultsManual /= [])
        "shrunken list is not empty (when shrunk manually)" $
    cover 10
        (L.length shrinkResultsGeneric >= 100)
        "shrunken list has at least 100 elements (when shrunk generically)" $
    cover 10
        (L.length shrinkResultsManual >= 100)
        "shrunken list has at least 100 elements (when shrunk manually)" $
    property p
  where
    shrinkResultsManual =
        shrinkTestRecordManually r
    shrinkResultsGeneric =
        shrinkTestRecordGenerically r

-- Verifies that generic shrinking produces the same result as manual shrinking.
--
prop_genericRoundRobinShrink_equivalence :: Property
prop_genericRoundRobinShrink_equivalence =
    forAll genTestRecord $ \r ->
    prop_genericRoundRobinShrink_coverage r $
    shrinkTestRecordGenerically r ===
    shrinkTestRecordManually r

-- Verifies that generic shrinking eventually leads to a minimal value.
--
prop_genericRoundRobinShrink_minimization :: Property
prop_genericRoundRobinShrink_minimization =
    forAll genTestRecord $ \r ->
    prop_genericRoundRobinShrink_coverage r $
    shrinkRepeatedlyUntilMinimal r === minimalTestRecord
  where
    shrinkRepeatedlyUntilMinimal :: TestRecord -> TestRecord
    shrinkRepeatedlyUntilMinimal record =
        case shrinkTestRecordManually record of
            (recordShrunk : _) -> shrinkRepeatedlyUntilMinimal recordShrunk
            [] -> record

-- Verifies that generic shrinking always makes progress.
--
prop_genericRoundRobinShrink_progression :: Property
prop_genericRoundRobinShrink_progression =
    forAll genTestRecord $ \r ->
    prop_genericRoundRobinShrink_coverage r $
    property $ all (\s -> r /= s && s `leq` r) (shrinkTestRecordGenerically r)

-- Verifies that generic shrinking never generates duplicate values.
--
prop_genericRoundRobinShrink_uniqueness :: Property
prop_genericRoundRobinShrink_uniqueness =
    forAll genTestRecord $ \r ->
    prop_genericRoundRobinShrink_coverage r $
    property $ not $ L.anySame (shrinkTestRecordGenerically r)

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
