{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{- HLINT ignore "Use null" -}

module Test.QuickCheck.ExtraSpec
    where

import Prelude

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Control.Monad
    ( forM_ )
import Data.Bifunctor
    ( first )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Data.List.Extra
    ( dropEnd )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( isJust, isNothing )
import Data.Set
    ( Set )
import Generics.SOP
    ( NP (Nil) )
import GHC.Generics
    ( Generic )
import Safe
    ( tailMay )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..), Fun (..), Gen, NonNegative (..), NonPositive (..),
    Positive (..), Property, Small (..), Testable, applyFun, checkCoverage,
    chooseInteger, conjoin, cover, forAll, frequency, genericShrink, listOf,
    oneof, property, scale, shrinkIntegral, within, (===) )
import Test.QuickCheck.Extra
    ( Pretty (..), genShrinkSequence, genericRoundRobinShrink,
    interleaveRoundRobin, partitionList, selectMapEntries, selectMapEntry,
    shrinkSpace, shrinkWhile, shrinkWhileSteps, (<:>), (<@>) )

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.Extra as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
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

    describe "partitionList" $ do
        it "prop_partitionList_coverage" $
            prop_partitionList_coverage
                @Int & property
        it "prop_partitionList_identity" $
            prop_partitionList_identity
                @Int & property
        it "prop_partitionList_mconcat" $
            prop_partitionList_mconcat
                @Int & property
        it "prop_partitionList_GE" $
            prop_partitionList_GE
                @Int & property
        it "prop_partitionList_LT" $
            prop_partitionList_LT
                @Int & property

    describe "Selecting random map entries" $ do

        describe "selectMapEntry" $ do
            it "prop_selectMapEntry_empty" $
                prop_selectMapEntry_empty
                    @Int @Int & property
            it "prop_selectMapEntry_singleton" $
                prop_selectMapEntry_singleton
                    @Int @Int & property
            it "prop_selectMapEntry_insert" $
                prop_selectMapEntry_insert
                    @Int @Int & property
            it "prop_selectMapEntry_lookup_Just" $
                prop_selectMapEntry_lookup_Just
                    @Int @Int & property
            it "prop_selectMapEntry_lookup_Nothing" $
                prop_selectMapEntry_lookup_Nothing
                    @Int @Int & property

        describe "selectMapEntries" $ do
            it "prop_selectMapEntries_empty" $
                prop_selectMapEntries_empty
                    @Int @Int & property
            it "prop_selectMapEntries_fromList" $
                prop_selectMapEntries_fromList
                    @Int @Int & property
            it "prop_selectMapEntries_length" $
                prop_selectMapEntries_length
                    @Int @Int & property
            it "prop_selectMapEntries_nonPositive" $
                prop_selectMapEntries_nonPositive
                    @Int @Int & property
            it "prop_selectMapEntries_disjoint" $
                prop_selectMapEntries_disjoint
                    @Int @Int & property
            it "prop_selectMapEntries_union" $
                prop_selectMapEntries_union
                    @Int @Int & property

    describe "Evaluating shrinkers" $ do

        describe "Generating sequences of shrunken values" $ do

            describe "genShrinkSequence" $ do

                it "prop_genShrinkSequence_length" $
                    prop_genShrinkSequence_length
                        @Integer & property
                it "prop_genShrinkSequence_empty" $
                    prop_genShrinkSequence_empty
                        @Integer & property
                it "prop_genShrinkSequence_start" $
                    prop_genShrinkSequence_start
                        @Integer & property
                it "prop_genShrinkSequence_termination" $
                    prop_genShrinkSequence_termination
                        @Integer & property
                it "prop_genShrinkSequence_validity" $
                    prop_genShrinkSequence_validity
                        @Integer & property

        describe "Evaluating the entire shrink space of a shrinker" $ do

            describe "shrinkSpace" $ do

                it "prop_shrinkSpace_complete" $
                    prop_shrinkSpace_complete
                        @Int & property
                it "prop_shrinkSpace_empty" $
                    prop_shrinkSpace_empty
                        @Int & property
                it "prop_shrinkSpace_singleton" $
                    prop_shrinkSpace_singleton
                        @Int & property

        describe "Repeatedly shrinking while a condition holds" $ do

            describe "shrinkWhile" $ do

                it "prop_shrinkWhile_coverage" $
                    prop_shrinkWhile_coverage
                        @Int & property
                it "prop_shrinkWhile_isNothing" $
                    prop_shrinkWhile_isNothing
                        @Int & property
                it "prop_shrinkWhile_satisfy" $
                    prop_shrinkWhile_satisfy
                        @Int & property

            describe "shrinkWhileSteps" $ do

                it "prop_shrinkWhileSteps_coverage" $
                    prop_shrinkWhileSteps_coverage
                        @Int & property
                it "prop_shrinkWhileSteps_satisfy" $
                    prop_shrinkWhileSteps_satisfy
                        @Int & property
                unit_shrinkWhileSteps_Int

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
    genInteger = oneof
        [ pure 0
        , chooseInteger (1, 1_000_000_000_000)
        ]

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
-- Generating list partitions
--------------------------------------------------------------------------------

data PartitionListData a = PartitionListData (Int, Int) [a]
    deriving (Eq, Generic, Show)

instance Arbitrary a => Arbitrary (PartitionListData a) where
    arbitrary = genPartitionListData
    shrink = shrinkPartitionListData

genPartitionListData :: forall a. Arbitrary a => Gen (PartitionListData a)
genPartitionListData =
    PartitionListData <$> genBounds <*> genList
  where
    genBounds :: Gen (Int, Int)
    genBounds = frequency
        [ (1, genBoundsAny)
        , (2, genBoundsValidAndDifferent)
        , (2, genBoundsValidAndIdentical)
        ]
      where
        genBoundsAny :: Gen (Int, Int)
        genBoundsAny = arbitrary

        genBoundsValidAndDifferent :: Gen (Int, Int)
        genBoundsValidAndDifferent = do
            x <- getSmall . getNonNegative <$> arbitrary
            d <- getSmall . getPositive <$> arbitrary
            pure (x, x + d)

        genBoundsValidAndIdentical :: Gen (Int, Int)
        genBoundsValidAndIdentical = do
            x <- getSmall . getPositive <$> arbitrary
            pure (x, x)

    genList :: Gen [a]
    genList = frequency
        [ (1, pure [])
        , (4, listOf arbitrary)
        , (4, listOf arbitrary & scale (* 8))
        ]

shrinkPartitionListData
    :: Arbitrary a => PartitionListData a -> [PartitionListData a]
shrinkPartitionListData = genericShrink

prop_partitionList_coverage
    :: (Eq a, Show a) => PartitionListData a -> Property
prop_partitionList_coverage (PartitionListData (x, y) as) =
    forAll (partitionList (x, y) as) $ \rs ->
        checkCoverage $
        cover 10
            (x < 0 || y < x)
            "bounds are invalid" $
        cover 10
            (x >= 1 && y == x)
            "bounds are valid and identical" $
        cover 10
            (x >= 0 && y > x)
            "bounds are valid and different" $
        cover 10
            (length rs == 0)
            "partitioned list length == 0" $
        cover 10
            (length rs == 1)
            "partitioned list length == 1" $
        cover 10
            (length rs >= 10)
            "partitioned list length >= 10" $
        property True

prop_partitionList_identity
    :: (Eq a, Show a) => [a] -> Property
prop_partitionList_identity as =
    forAll (partitionList (length as, length as) as)
        (=== [as | length as > 0])

prop_partitionList_mconcat
    :: (Eq a, Show a) => PartitionListData a -> Property
prop_partitionList_mconcat (PartitionListData (x, y) as) =
    forAll (partitionList (x, y) as)
        ((=== as) . mconcat)

prop_partitionList_GE
    :: Show a => PartitionListData a -> Property
prop_partitionList_GE (PartitionListData (x, y) as) =
    forAll (partitionList (x, y) as) $ \rs ->
        checkCoverage $
        cover 10
            (any ((> x') . length) rs)
            "at least one generated sublist has length > minimum" $
        cover 10
            (any ((== x') . length) rs)
            "at least one generated sublist has length = minimum" $
        all ((>= x') . length) $ dropEnd 1 rs
  where
    x' = max 0 x

prop_partitionList_LT
    :: Show a => PartitionListData a -> Property
prop_partitionList_LT (PartitionListData (x, y) as) =
    forAll (partitionList (x, y) as) $ \rs ->
        checkCoverage $
        cover 10
            (any ((< y') . length) rs)
            "at least one generated sublist has length < maximum" $
        cover 10
            (any ((== y') . length) rs)
            "at least one generated sublist has length = maximum" $
        all ((<= y') . length) rs
  where
    x' = max 0 x
    y' = max 1 (max y x')

--------------------------------------------------------------------------------
-- Selecting map entries (one at a time)
--------------------------------------------------------------------------------

prop_selectMapEntry_empty
    :: forall k v. (Ord k, Show k, Eq v, Show v) => Property
prop_selectMapEntry_empty =
    forAll (selectMapEntry (Map.empty @k @v)) (=== Nothing)

prop_selectMapEntry_singleton
    :: (Ord k, Show k, Eq v, Show v) => k -> v -> Property
prop_selectMapEntry_singleton k v =
    forAll (selectMapEntry (Map.singleton k v)) (=== Just ((k, v), mempty))

prop_selectMapEntry_insert
    :: (Ord k, Show k, Eq v, Show v) => Map k v -> Property
prop_selectMapEntry_insert m0 =
    forAll (selectMapEntry m0) $ \mr ->
        checkCoverage $
        cover 20 (isJust mr)
            "number of selected entries = 1" $
        cover 1 (isNothing mr)
            "number of selected entries = 0" $
        case mr of
            Nothing ->
                m0 === mempty
            Just ((k, v), m1) ->
                m0 === Map.insert k v m1

prop_selectMapEntry_lookup_Just
    :: (Ord k, Show k, Eq v, Show v) => Map k v -> Property
prop_selectMapEntry_lookup_Just m0 =
    forAll (selectMapEntry m0) $ \mr ->
        checkCoverage $
        cover 20 (isJust mr)
            "number of selected entries = 1" $
        cover 1 (isNothing mr)
            "number of selected entries = 0" $
        case mr of
            Nothing ->
                m0 === mempty
            Just ((k, v), _) ->
                Map.lookup k m0 === Just v

prop_selectMapEntry_lookup_Nothing
    :: (Ord k, Show k, Eq v, Show v) => Map k v -> Property
prop_selectMapEntry_lookup_Nothing m0 =
    forAll (selectMapEntry m0) $ \mr ->
        checkCoverage $
        cover 20 (isJust mr)
            "number of selected entries = 1" $
        cover 1 (isNothing mr)
            "number of selected entries = 0" $
        case mr of
            Nothing ->
                m0 === mempty
            Just ((k, _), m1) ->
                Map.lookup k m1 === Nothing

--------------------------------------------------------------------------------
-- Selecting map entries (many at a time)
--------------------------------------------------------------------------------

prop_selectMapEntries_empty
    :: forall k v. (Ord k, Show k, Eq v, Show v) => Int -> Property
prop_selectMapEntries_empty i =
    forAll (selectMapEntries (Map.empty @k @v) i) (=== ([], Map.empty))

prop_selectMapEntries_fromList
    :: (Ord k, Show k, Eq v, Show v) => Map k v -> Property
prop_selectMapEntries_fromList m =
    checkCoverage $
    cover 10 (Map.size m > 0)
        "number of available entries > 0" $
    cover 1 (Map.size m == 0)
        "number of available entries = 0" $
    forAll (selectMapEntries m (length m)) $
        (=== (m, mempty)) . first Map.fromList

prop_selectMapEntries_length
    :: forall k v. (Ord k, Show k, Show v)
    => Map k v
    -> Positive (Small Int)
    -> Property
prop_selectMapEntries_length m (Positive (Small i)) =
    forAll (selectMapEntries m i) $ \(kvs, _) ->
        checkCoverage $
        cover 10 (i < Map.size m)
            "number of requested entries < size of map" $
        cover 10 (i > Map.size m)
            "number of requested entries > size of map" $
        cover 1 (i == Map.size m)
            "number of requested entries = size of map" $
        length kvs === min i (Map.size m)

prop_selectMapEntries_nonPositive
    :: (Ord k, Show k, Eq v, Show v)
    => Map k v
    -> NonPositive (Small Int)
    -> Property
prop_selectMapEntries_nonPositive m (NonPositive (Small i)) =
    forAll (selectMapEntries m i) (== ([], m))

prop_selectMapEntries_disjoint
    :: (Ord k, Show k, Show v)
    => Map k v
    -> Positive (Small Int)
    -> Property
prop_selectMapEntries_disjoint m0 (Positive (Small i)) =
    forAll (selectMapEntries m0 i) $ \(kvs, m1) ->
        checkCoverage $
        cover 10 (length kvs == i && i >= 1)
            "number of selected entries = requested number" $
        cover 10 (length kvs >= 1 && length kvs < i)
            "number of selected entries < requested number" $
        cover 1 (length kvs == 0)
            "number of selected entries = 0" $
        Map.fromList kvs `Map.disjoint` m1

prop_selectMapEntries_union
    :: (Ord k, Show k, Eq v, Show v)
    => Map k v
    -> Positive (Small Int)
    -> Property
prop_selectMapEntries_union m0 (Positive (Small i)) =
    forAll (selectMapEntries m0 i) $ \(kvs, m1) ->
        checkCoverage $
        cover 10 (length kvs == i && i >= 1)
            "number of selected entries = requested number" $
        cover 10 (length kvs >= 1 && length kvs < i)
            "number of selected entries < requested number" $
        cover 1 (length kvs == 0)
            "number of selected entries = 0" $
        Map.fromList kvs `Map.union` m1 === m0

--------------------------------------------------------------------------------
-- Generating sequences of shrunken values
--------------------------------------------------------------------------------

prop_genShrinkSequence_length
    :: (Arbitrary a, Show a) => a -> Property
prop_genShrinkSequence_length a =
    forAll (genShrinkSequence shrink a) $ \as ->
        checkCoverage $
        cover 2 (length as ==  0) "length as ==  0" $
        cover 2 (length as ==  1) "length as ==  1" $
        cover 2 (length as ==  2) "length as ==  2" $
        cover 2 (length as >= 10) "length as >= 10" $
        property True

-- Verify that the resulting sequence is only empty if the starting value
-- cannot be shrunk.
--
prop_genShrinkSequence_empty
    :: (Arbitrary a, Show a) => a -> Property
prop_genShrinkSequence_empty a =
    forAll (genShrinkSequence shrink a) $ \as ->
        null as === null (shrink a)

-- Verify that the starting value is not present in the resulting sequence.
--
prop_genShrinkSequence_start
    :: (Arbitrary a, Eq a, Show a) => a -> Property
prop_genShrinkSequence_start a =
    forAll (genShrinkSequence shrink a) $ \as ->
        a `notElem` as

-- Verify that the final element in the resulting sequence cannot be shrunk
-- further.
--
prop_genShrinkSequence_termination
    :: (Arbitrary a, Eq a, Show a) => a -> Property
prop_genShrinkSequence_termination a =
    forAll (genShrinkSequence shrink a) $ \as ->
        shrink (NE.last (a :| as)) === []

-- Verify that each successive element in the resulting sequence is a member of
-- the shrink set of the preceding element.
--
prop_genShrinkSequence_validity
    :: (Arbitrary a, Eq a, Show a) => a -> Property
prop_genShrinkSequence_validity a =
    forAll (genShrinkSequence shrink a) $ \as ->
        all (\(x, y) -> y `elem` shrink x) (consecutivePairs (a :| as))

--------------------------------------------------------------------------------
-- Evaluating the entire shrink space of a shrinking function
--------------------------------------------------------------------------------

prop_shrinkSpace_complete :: (Arbitrary a, Ord a) => a -> Property
prop_shrinkSpace_complete a =
    within twoSeconds $
    conjoin
        [ -- All initial shrinks are present in the set:
          all (`Set.member` ss) (shrink a)
          -- All transitive shrinks are present in the set:
        , all (all (`Set.member` ss) . shrink) ss
        ]
  where
    ss = shrinkSpace shrink a
    twoSeconds = 2_000_000

prop_shrinkSpace_empty :: (Ord a, Show a) => a -> Property
prop_shrinkSpace_empty a =
    shrinkSpace (const []) a === mempty

prop_shrinkSpace_singleton :: (Ord a, Show a) => a -> Property
prop_shrinkSpace_singleton a =
    shrinkSpace (const [a]) a === Set.singleton a

--------------------------------------------------------------------------------
-- Repeatedly shrinking while a condition holds
--------------------------------------------------------------------------------

prop_shrinkWhile_coverage :: Arbitrary a => Fun a Bool -> a -> Property
prop_shrinkWhile_coverage (applyFun -> condition) a
    = checkCoverage
    $ cover 10
        (isJust shrinkWhileResult)
        "isJust shrinkWhileResult"
    $ cover 10
        (isNothing shrinkWhileResult)
        "isNothing shrinkWhileResult"
    $ property True
  where
    shrinkWhileResult = shrinkWhile condition shrink a

prop_shrinkWhile_isNothing :: Arbitrary a => Fun a Bool -> a -> Property
prop_shrinkWhile_isNothing (applyFun -> condition) a =
    isNothing (shrinkWhile condition shrink a)
    === (not (condition a) || null (L.find condition (shrink a)))

prop_shrinkWhile_satisfy :: Arbitrary a => Fun a Bool -> a -> Property
prop_shrinkWhile_satisfy (applyFun -> condition) a =
    all condition (shrinkWhile condition shrink a)
    === True

prop_shrinkWhileSteps_coverage :: Arbitrary a => Fun a Bool -> a -> Property
prop_shrinkWhileSteps_coverage (applyFun -> condition) a
    = checkCoverage
    $ cover 10
        (null shrinkWhileStepsResult)
        "null shrinkWhileStepsResult"
    $ cover 10
        (length shrinkWhileStepsResult == 1)
        "length shrinkWhileStepsResult == 1"
    $ cover 10
        (length shrinkWhileStepsResult >= 2)
        "length shrinkWhileStepsResult >= 2"
    $ property True
  where
    shrinkWhileStepsResult = shrinkWhileSteps condition shrink a

prop_shrinkWhileSteps_satisfy :: Arbitrary a => Fun a Bool -> a -> Property
prop_shrinkWhileSteps_satisfy (applyFun -> condition) a =
    all condition (shrinkWhileSteps condition shrink a)
    === True

unit_shrinkWhileSteps_Int :: Spec
unit_shrinkWhileSteps_Int = unitTests
    "unit_shrinkWhileSteps_Int"
    (\(a, condition) -> shrinkWhileSteps condition shrink a)
    tests
  where
    tests :: [UnitTestData (Int, (Int -> Bool)) [Int]]
    tests =
        [ UnitTestData
            { params = (1_024, (>= 0))
            , result = [0]
            }
        , UnitTestData
            { params = (1_024, (>= 1))
            , result = [512, 256, 128, 64, 32, 16, 8, 4, 2, 1]
            }
        , UnitTestData
            { params = (1_024, (>= 10))
            , result = [512, 256, 128, 64, 32, 16, 12, 11, 10]
            }
        , UnitTestData
            { params = (1_024, (>= 100))
            , result = [512, 256, 128, 112, 105, 102, 101, 100]
            }
        , UnitTestData
            { params = (1_024, (>= 1_000))
            , result = [1_008, 1_001, 1_000]
            }
        , UnitTestData
            { params = (1_024, (>= 10_000))
            , result = []
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

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

consecutivePairs :: Foldable f => f a -> [(a, a)]
consecutivePairs (F.toList -> xs) = case tailMay xs of
    Nothing -> []
    Just ys -> xs `zip` ys
