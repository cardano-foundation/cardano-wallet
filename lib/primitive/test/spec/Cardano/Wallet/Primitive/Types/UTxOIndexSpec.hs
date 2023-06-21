{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Hoist not" -}

module Cardano.Wallet.Primitive.Types.UTxOIndexSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRangePositive, shrinkTokenBundleSmallRangePositive )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetId, shrinkAssetId )
import Cardano.Wallet.Primitive.Types.UTxOIndex.Gen
    ( genUTxOIndex, shrinkUTxOIndex )
import Cardano.Wallet.Primitive.Types.UTxOIndex.Internal
    ( Asset (..), BundleCategory (..), InvariantStatus (..),
    SelectionFilter (..), UTxOIndex, categorizeTokenBundle, checkInvariant )
import Control.Monad
    ( void )
import Control.Monad.Random.Class
    ( MonadRandom (..) )
import Data.Function
    ( (&) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( isJust, isNothing )
import Data.Ratio
    ( (%) )
import Data.Word
    ( Word8 )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..), CoArbitrary (..), Confidence (..), Gen, Property,
    Testable, checkCoverage, checkCoverageWith, conjoin, counterexample, cover,
    forAll, oneof, property, stdConfidence, suchThat, withMaxSuccess, (===) )
import Test.QuickCheck.Classes
    ( eqLaws )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )
import Test.QuickCheck.Quid
    ( Hexadecimal (..), Quid, Size (..) )
import Test.Utils.Laws
    ( testLawsMany )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex.Internal as UTxOIndex
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec =
    describe "Cardano.Wallet.Primitive.Types.UTxOIndexSpec" $ do

    describe "Class instances obey laws" $ do
        testLawsMany @(UTxOIndex TestUTxO)
            [ eqLaws
            ]

    describe
        "All operations preserve the invariant:" $ do

        it "prop_arbitrary_invariant" $
            property prop_arbitrary_invariant
        it "prop_shrink_invariant" $
            property prop_shrink_invariant
        it "prop_empty_invariant" $
            property prop_empty_invariant
        it "prop_singleton_invariant" $
            property prop_singleton_invariant
        it "prop_fromMap_invariant" $
            property prop_fromMap_invariant
        it "prop_fromSequence_invariant" $
            property prop_fromSequence_invariant
        it "prop_insert_invariant" $
            property prop_insert_invariant
        it "prop_delete_invariant" $
            property prop_delete_invariant
        it "prop_selectRandom_invariant" $
            property prop_selectRandom_invariant

    describe "Construction and deconstruction" $ do

        it "prop_empty_toList" $
            property prop_empty_toList
        it "prop_fromMap_fromSequence" $
            property prop_fromMap_fromSequence
        it "prop_singleton_toList" $
            property prop_singleton_toList
        it "prop_toList_fromSequence" $
            property prop_toList_fromSequence

    describe "Modification" $ do

        it "prop_delete_balance" $
            property prop_delete_balance
        it "prop_delete_lookup" $
            property prop_delete_lookup
        it "prop_delete_size" $
            property prop_delete_size
        it "prop_insert_assets" $
            property prop_insert_assets
        it "prop_insert_balance" $
            property prop_insert_balance
        it "prop_insert_delete" $
            property prop_insert_delete
        it "prop_insert_lookup" $
            property prop_insert_lookup
        it "prop_insert_size" $
            property prop_insert_size

    describe "Filtering and partitioning" $ do

        it "prop_filter_disjoint" $
            property prop_filter_disjoint
        it "prop_filter_partition" $
            property prop_filter_partition
        it "prop_filter_toList" $
            property prop_filter_toList
        it "prop_partition_disjoint" $
            property prop_partition_disjoint

    describe "Index Selection" $ do

        it "prop_SelectionFilter_coverage" $
            property prop_SelectionFilter_coverage
        it "prop_selectRandom" $
            property prop_selectRandom
        it "prop_selectRandom_empty" $
            property prop_selectRandom_empty
        it "prop_selectRandom_all" $
            property prop_selectRandom_all
        it "prop_selectRandomWithPriority" $
            property prop_selectRandomWithPriority

    describe "Set Selection" $ do

        it "prop_selectRandomSetMember_empty" $
            property prop_selectRandomSetMember_empty
        it "prop_selectRandomSetMember_singleton" $
            property prop_selectRandomSetMember_singleton
        it "prop_selectRandomSetMember_coversRangeUniformly" $
            property prop_selectRandomSetMember_coversRangeUniformly

--------------------------------------------------------------------------------
-- Invariant properties
--------------------------------------------------------------------------------

invariantHolds :: Ord u => UTxOIndex u -> Property
invariantHolds i = checkInvariant i === InvariantHolds

prop_arbitrary_invariant :: UTxOIndex TestUTxO -> Property
prop_arbitrary_invariant = invariantHolds

prop_shrink_invariant :: u ~ Size 4 TestUTxO => UTxOIndex u -> Property
prop_shrink_invariant = conjoin . fmap invariantHolds . shrink

prop_empty_invariant :: Property
prop_empty_invariant = invariantHolds (UTxOIndex.empty @TestUTxO)

prop_singleton_invariant :: TestUTxO -> TokenBundle -> Property
prop_singleton_invariant u b = invariantHolds $ UTxOIndex.singleton u b

prop_fromMap_invariant :: Map TestUTxO TokenBundle -> Property
prop_fromMap_invariant = invariantHolds . UTxOIndex.fromMap

prop_fromSequence_invariant :: [(TestUTxO, TokenBundle)] -> Property
prop_fromSequence_invariant = invariantHolds . UTxOIndex.fromSequence

prop_insert_invariant
    :: u ~ TestUTxO => u -> TokenBundle -> UTxOIndex u -> Property
prop_insert_invariant u b i = invariantHolds $ UTxOIndex.insert u b i

prop_delete_invariant
    :: u ~ TestUTxO => u -> UTxOIndex u -> Property
prop_delete_invariant u i = invariantHolds $ UTxOIndex.delete u i

prop_selectRandom_invariant
    :: UTxOIndex TestUTxO -> SelectionFilter Asset -> Property
prop_selectRandom_invariant i f =
    monadicIO $ do
        run $ do
            result <- UTxOIndex.selectRandom i f
            pure $ prop_inner result
  where
    prop_inner :: Maybe (a, UTxOIndex TestUTxO) -> Property
    prop_inner result =
        checkCoverage $
        cover 10 (isNothing result)
            "selected nothing" $
        cover 10 (isJust result)
            "selected something" $
        case result of
            Nothing ->
                property True
            Just (_, i') ->
                checkInvariant i' === InvariantHolds

--------------------------------------------------------------------------------
-- Construction and deconstruction properties
--------------------------------------------------------------------------------

prop_empty_toList :: Property
prop_empty_toList =
    UTxOIndex.toList (UTxOIndex.empty @TestUTxO) === []

prop_fromMap_fromSequence :: Map TestUTxO TokenBundle -> Property
prop_fromMap_fromSequence m =
    UTxOIndex.fromMap m === UTxOIndex.fromSequence (Map.toList m)

prop_singleton_toList :: TestUTxO -> TokenBundle -> Property
prop_singleton_toList u b =
    UTxOIndex.toList (UTxOIndex.singleton u b) === [(u, b)]

prop_toList_fromSequence :: UTxOIndex TestUTxO -> Property
prop_toList_fromSequence i =
    UTxOIndex.fromSequence (UTxOIndex.toList i) === i

--------------------------------------------------------------------------------
-- Modification properties
--------------------------------------------------------------------------------

checkCoverage_modify
    :: (Testable prop, Ord u) => u -> UTxOIndex u -> prop -> Property
checkCoverage_modify u i
    = checkCoverage
    . cover 30 (UTxOIndex.member u i)
        "UTxO is a member of the index"
    . cover 30 (not $ UTxOIndex.member u i)
        "UTxO is not a member of the index"

prop_delete_balance
    :: u ~ Size 4 TestUTxO => u -> UTxOIndex u -> Property
prop_delete_balance u i =
    checkCoverage_modify u i $
    UTxOIndex.balance (UTxOIndex.delete u i) === expected
  where
    expected = case UTxOIndex.lookup u i of
        Nothing ->
            UTxOIndex.balance i
        Just b ->
            UTxOIndex.balance i `TokenBundle.difference` b

prop_delete_lookup
    :: u ~ Size 4 TestUTxO => u -> UTxOIndex u -> Property
prop_delete_lookup u i =
    checkCoverage_modify u i $
    UTxOIndex.lookup u (UTxOIndex.delete u i) === Nothing

prop_delete_size
    :: u ~ Size 4 TestUTxO => u -> UTxOIndex u -> Property
prop_delete_size u i =
    checkCoverage_modify u i $
    UTxOIndex.size (UTxOIndex.delete u i) === expected
  where
    expected = case UTxOIndex.lookup u i of
        Nothing ->
            UTxOIndex.size i
        Just _ ->
            UTxOIndex.size i - 1

prop_insert_assets
    :: u ~ Size 4 TestUTxO => u -> TokenBundle -> UTxOIndex u -> Property
prop_insert_assets u b i =
    checkCoverage_modify u i $
    UTxOIndex.assets (UTxOIndex.insert u b i)
        `Set.intersection` insertedAssets === insertedAssets
  where
    insertedAssets = UTxOIndex.tokenBundleAssets b

prop_insert_balance
    :: u ~ Size 4 TestUTxO => u -> TokenBundle -> UTxOIndex u -> Property
prop_insert_balance u b i =
    checkCoverage_modify u i $
    UTxOIndex.balance (UTxOIndex.insert u b i) === expected
  where
    expected = b `TokenBundle.add` case UTxOIndex.lookup u i of
        Nothing ->
            UTxOIndex.balance i
        Just b' ->
            UTxOIndex.balance i `TokenBundle.difference` b'

prop_insert_delete
    :: u ~ Size 4 TestUTxO => u -> TokenBundle -> UTxOIndex u -> Property
prop_insert_delete u b i =
    checkCoverage_modify u i $
    UTxOIndex.delete u (UTxOIndex.insert u b i) === expected
  where
    expected =
        if UTxOIndex.member u i then UTxOIndex.delete u i else i

prop_insert_lookup
    :: u ~ Size 4 TestUTxO => u -> TokenBundle -> UTxOIndex u -> Property
prop_insert_lookup u b i =
    checkCoverage_modify u i $
    UTxOIndex.lookup u (UTxOIndex.insert u b i) === Just b

prop_insert_size
    :: u ~ Size 4 TestUTxO => u -> TokenBundle -> UTxOIndex u -> Property
prop_insert_size u b i =
    checkCoverage_modify u i $
    UTxOIndex.size (UTxOIndex.insert u b i) === expected
  where
    expected = case UTxOIndex.lookup u i of
        Nothing ->
            UTxOIndex.size i + 1
        Just _ ->
            UTxOIndex.size i

--------------------------------------------------------------------------------
-- Filtering and partitioning
--------------------------------------------------------------------------------

prop_filter_disjoint
    :: (TestUTxO -> Bool) -> UTxOIndex TestUTxO -> Property
prop_filter_disjoint f i =
    checkCoverage_filter_partition f i $
    UTxOIndex.filter f i `UTxOIndex.disjoint` UTxOIndex.filter (not . f) i
        === True

prop_filter_partition
    :: (TestUTxO -> Bool) -> UTxOIndex TestUTxO -> Property
prop_filter_partition f i =
    checkCoverage_filter_partition f i $
    (UTxOIndex.filter f i, UTxOIndex.filter (not . f) i)
        === UTxOIndex.partition f i

prop_filter_toList
    :: (TestUTxO -> Bool) -> UTxOIndex TestUTxO -> Property
prop_filter_toList f i =
    checkCoverage_filter_partition f i $
    UTxOIndex.toList (UTxOIndex.filter f i)
        === L.filter (f . fst) (UTxOIndex.toList i)

prop_partition_disjoint
    :: (TestUTxO -> Bool) -> UTxOIndex TestUTxO -> Property
prop_partition_disjoint f i =
    checkCoverage_filter_partition f i $
    uncurry UTxOIndex.disjoint (UTxOIndex.partition f i) === True

checkCoverage_filter_partition
    :: (Testable prop, Ord u)
    => (u -> Bool)
    -> UTxOIndex u
    -> (prop -> Property)
checkCoverage_filter_partition f i
    = checkCoverage
    . cover 10
        (UTxOIndex.filter f i `isNonEmptyProperSubsetOf` i)
        "UTxOIndex.filter f i `isNonEmptyProperSubsetOf` i"
    . cover 10
        (UTxOIndex.filter (not . f) i `isNonEmptyProperSubsetOf` i)
        "UTxOIndex.filter (not . f) i `isNonEmptyProperSubsetOf` i"
    . cover 10
        (filterSize f i > filterSize (not . f) i)
        "filterSize f i > filterSize (not . f) i"
    . cover 10
        (filterSize f i < filterSize (not . f) i)
        "filterSize f i < filterSize (not . f) i"
  where
    i1 `isNonEmptyProperSubsetOf` i2 =
        not (UTxOIndex.null i1)
        && UTxOIndex.toMap i1 `Map.isSubmapOf` UTxOIndex.toMap i2
        && i1 /= i2

    filterSize g = UTxOIndex.size . UTxOIndex.filter g

--------------------------------------------------------------------------------
-- Index selection properties
--------------------------------------------------------------------------------

prop_SelectionFilter_coverage :: SelectionFilter Asset -> Property
prop_SelectionFilter_coverage selectionFilter = checkCoverage $ property
    $ cover 20 (category == SelectSingleton ())
        "SelectSingleton"
    $ cover 20 (category == SelectPairWith ())
        "SelectPairWith"
    $ cover 20 (category == SelectAnyWith ())
        "SelectAnyWith"
    $ cover 20 (category == SelectAny)
        "SelectAny"
    True
  where
    category = void selectionFilter

-- | Attempt to select a random entry from an empty index.
--
-- This should always return 'Nothing'.
--
prop_selectRandom_empty :: SelectionFilter Asset -> Property
prop_selectRandom_empty f = monadicIO $ do
    result <- run $ UTxOIndex.selectRandom (UTxOIndex.empty @TestUTxO) f
    assert $ isNothing result

prop_selectRandom
    :: UTxOIndex TestUTxO
    -> SelectionFilter Asset
    -> Property
prop_selectRandom index selectionFilter = monadicIO $
    prop_inner <$> run (UTxOIndex.selectRandom index selectionFilter)
  where
    prop_inner maybeSelected
        = checkCoverage
        -- We need to cover all possible selection filters, and for each
        -- selection filter we need to cover both the case where we /do/
        -- have a match /and/ the case where we /don't/ have a match.
        $ cover 4
            (matchPositive && category == SelectSingleton ())
            "matchPositive && category == SelectSingleton ()"
        $ cover 4
            (matchPositive && category == SelectPairWith ())
            "matchPositive && category == SelectPairWith ()"
        $ cover 4
            (matchPositive && category == SelectAnyWith ())
            "matchPositive && category == SelectAnyWith ()"
        $ cover 4
            (matchPositive && category == SelectAny)
            "matchPositive && category == SelectAny"
        $ cover 4
            (matchNegative && category == SelectSingleton ())
            "matchNegative && category == SelectSingleton ()"
        $ cover 4
            (matchNegative && category == SelectPairWith ())
            "matchNegative && category == SelectPairWith ()"
        $ cover 1
            (matchNegative && category == SelectAnyWith ())
            "matchNegative && category == SelectAnyWith ()"
        $ cover 0.5
            -- This case should only match if the index is completely empty,
            -- so we can't expect to match this case very often.
            (matchNegative && category == SelectAny)
            "matchNegative && category == SelectAny"
        $ maybe prop_inner_Nothing prop_inner_Just maybeSelected
      where
        category = void selectionFilter
        matchPositive = maybeSelected & isJust
        matchNegative = maybeSelected & isNothing

    prop_inner_Nothing =
        -- Given that nothing has been selected, demonstrate that nothing
        -- /could/ have been selected, by manually filtering the list of all
        -- entries in the index to check that nothing matches.
        L.filter (selectionFilterMatchesBundleCategory selectionFilter)
            bundleCategories
            === []
      where
        bundleCategories =
            categorizeTokenBundle <$> F.toList (UTxOIndex.toMap index)

    prop_inner_Just ((utxo, bundle), indexReduced) =
        -- Given that something has been selected, demonstrate that the
        -- selected token bundle is of a matching category, and that the
        -- selected UTxO entry was correctly removed from the index.
        conjoin
            [ UTxOIndex.lookup utxo index
                === Just bundle
            , UTxOIndex.lookup utxo indexReduced
                === Nothing
            , UTxOIndex.balance index
                === UTxOIndex.balance indexReduced <> bundle
            , UTxOIndex.delete utxo index
                === indexReduced
            , UTxOIndex.insert utxo bundle indexReduced
                === index
            , property
                $ UTxOIndex.member utxo index
            , property
                $ not (UTxOIndex.member utxo indexReduced)
            , property
                $ selectionFilterMatchesBundleCategory selectionFilter
                $ categorizeTokenBundle bundle
            ]

prop_selectRandom_all :: UTxOIndex TestUTxO -> SelectionFilter Asset -> Property
prop_selectRandom_all index f = monadicIO $
    prop_inner <$> run (selectAll f index)
  where
    prop_inner :: ([(TestUTxO, TokenBundle)], UTxOIndex TestUTxO) -> Property
    prop_inner (selected, indexReduced)
        = checkCoverage
        $ cover 10
            (F.length selected > 0 && F.length selected < UTxOIndex.size index)
            "F.length selected > 0 && F.length selected < UTxOIndex.size index"
        $ cover 1
            (F.length selected > 0 && F.length selected == UTxOIndex.size index)
            "F.length selected > 0 && F.length selected == UTxOIndex.size index"
        $ conjoin
            [ UTxOIndex.balance index
                === UTxOIndex.balance indexReduced <> F.fold (snd <$> selected)
            , F.foldl' (flip UTxOIndex.delete) index (fst <$> selected)
                === indexReduced
            , F.foldl' (flip (uncurry UTxOIndex.insert)) indexReduced selected
                === index
            , property
                $ all (`UTxOIndex.member` index)
                $ fst <$> selected
            , property
                $ all (not . (`UTxOIndex.member` indexReduced))
                $ fst <$> selected
            , property
                $ all (selectionFilterMatchesBundleCategory f)
                $ categorizeTokenBundle . snd <$> selected
            , property
                $ all (not . selectionFilterMatchesBundleCategory f)
                $ categorizeTokenBundle . snd <$> UTxOIndex.toList indexReduced
            ]

-- | Verify that priority order is respected when selecting with more than
--   one filter.
--
prop_selectRandomWithPriority :: UTxOIndex TestUTxO -> Property
prop_selectRandomWithPriority i =
    forAll (genAsset) $ \a1 ->
    forAll (genAsset `suchThat` (/= a1)) $ \a2 ->
    checkCoverage $ monadicIO $ do
        haveMatchForAsset1 <- isJust <$>
            run (UTxOIndex.selectRandom i $ SelectPairWith a1)
        haveMatchForAsset2 <- isJust <$>
            run (UTxOIndex.selectRandom i $ SelectPairWith a2)
        monitor $ cover 4 (haveMatchForAsset1 && not haveMatchForAsset2)
            "have match for asset 1 but not for asset 2"
        monitor $ cover 4 (not haveMatchForAsset1 && haveMatchForAsset2)
            "have match for asset 2 but not for asset 1"
        monitor $ cover 4 (haveMatchForAsset1 && haveMatchForAsset2)
            "have match for both asset 1 and asset 2"
        monitor $ cover 4 (not haveMatchForAsset1 && not haveMatchForAsset2)
            "have match for neither asset 1 nor asset 2"
        result <- run $ UTxOIndex.selectRandomWithPriority i
            [SelectPairWith a1, SelectPairWith a2]
        case result of
            Just ((_, b), _) | b `UTxOIndex.tokenBundleHasAsset` a1 -> do
                assert haveMatchForAsset1
            Just ((_, b), _) | b `UTxOIndex.tokenBundleHasAsset` a2 -> do
                assert (not haveMatchForAsset1)
                assert haveMatchForAsset2
            _ -> do
                assert (not haveMatchForAsset1)
                assert (not haveMatchForAsset2)

--------------------------------------------------------------------------------
-- Set selection properties
--------------------------------------------------------------------------------

-- | Attempt to select a random entry from an empty set.
--
-- This should always return 'Nothing'.
--
prop_selectRandomSetMember_empty :: Property
prop_selectRandomSetMember_empty = monadicIO $ do
    result <- run $ UTxOIndex.selectRandomSetMember mempty
    assert $ result == Nothing @()

-- | Attempt to select a random entry from a singleton set with entry 'i'.
--
-- This should always return 'Just i'.
--
prop_selectRandomSetMember_singleton :: Int -> Property
prop_selectRandomSetMember_singleton i = monadicIO $ do
    result <- run $ UTxOIndex.selectRandomSetMember $ Set.singleton i
    assert $ result == Just i

-- | Verify that the random selection function chooses elements uniformly
--   across the full range of a set.
--
prop_selectRandomSetMember_coversRangeUniformly :: Word8 -> Word8 -> Property
prop_selectRandomSetMember_coversRangeUniformly i j =
    withMaxSuccess 100_000 $ checkCoverageWith confidence $ monadicIO $ do
        Just selected <- run $ UTxOIndex.selectRandomSetMember set
        monitor $ cover 90 (elementCount > 1)
            "set has more than 1 element"
        monitor $ cover 50 (elementCount > 10)
            "set has more than 10 elements"
        monitor $ cover 10 (elementCount > 100)
            "set has more than 100 elements"
        monitor $ cover 5 (selected == smallest)
            "selected smallest element"
        monitor $ cover 5 (selected == greatest)
            "selected greatest element"
        monitor $ cover 50 (inLowerHalf selected)
            "selected element in lower half of range"
        monitor $ cover 50 (inUpperHalf selected)
            "selected element in upper half of range"
        monitor
            $ counterexample
            $ mconcat
            $ fmap showKeyValuePair
            [ ("smallest", show smallest)
            , ("greatest", show greatest)
            , ("midpoint", show midpoint)
            , ("selected", show selected)
            ]
        assert $ fromIntegral smallest % 1 <= midpoint
        assert $ fromIntegral greatest % 1 >= midpoint
        assert $ selected `Set.member` set
  where
    confidence = stdConfidence {tolerance = 0.99}
    elementCount = greatest - smallest + 1
    greatest = max i j
    inLowerHalf r = fromIntegral r % 1 <= midpoint
    inUpperHalf r = fromIntegral r % 1 >= midpoint
    midpoint :: Rational
    midpoint = (fromIntegral smallest + fromIntegral greatest) % 2
    set = Set.fromList [smallest .. greatest]
    showKeyValuePair (key, value) = key <> ": " <> value <> "\n"
    smallest = min i j

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Indicates whether or not a token bundle of the given category should be
--   matched by the given selection filter.
--
selectionFilterMatchesBundleCategory
    :: Ord asset
    => SelectionFilter asset
    -> BundleCategory asset
    -> Bool
selectionFilterMatchesBundleCategory selectionFilter category =
    case selectionFilter of
        SelectSingleton asset ->
            case category of
                BundleWithNoAssets ->
                    False
                BundleWithOneAsset asset1 ->
                    asset1 == asset
                BundleWithTwoAssets _ ->
                    False
                BundleWithMultipleAssets _ ->
                    False
        SelectPairWith asset ->
            case category of
                BundleWithNoAssets ->
                    False
                BundleWithOneAsset _ ->
                    False
                BundleWithTwoAssets (asset1, asset2) ->
                    asset1 == asset || asset2 == asset
                BundleWithMultipleAssets _ ->
                    False
        SelectAnyWith asset ->
            case category of
                BundleWithNoAssets ->
                    False
                BundleWithOneAsset asset1 ->
                    asset1 == asset
                BundleWithTwoAssets (asset1, asset2) ->
                    asset1 == asset || asset2 == asset
                BundleWithMultipleAssets assets ->
                    Set.member asset assets
        SelectAny ->
            True

-- | Selects all UTxO entries matching a particular filter.
--
-- Returns a list of all the entries that matched, and an updated index with
-- the selected entries removed.
--
selectAll
    :: (MonadRandom m, u ~ TestUTxO)
    => SelectionFilter Asset
    -> UTxOIndex u
    -> m ([(u, TokenBundle)], UTxOIndex u)
selectAll sf = go []
  where
    go !selectedEntries !i = do
        selected <- UTxOIndex.selectRandom i sf
        case selected of
            Nothing ->
                -- There are no more entries available. Terminate here:
                pure (selectedEntries, i)
            Just ((u, b), iReduced) ->
                go ((u, b) : selectedEntries) iReduced

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

newtype TestUTxO = TestUTxO (Hexadecimal Quid)
    deriving (Arbitrary, CoArbitrary) via Quid
    deriving stock (Eq, Ord, Read, Show)

instance Arbitrary Asset where
    arbitrary = genAsset
    shrink = shrinkAsset

instance Arbitrary AssetId where
    arbitrary = genAssetId
    shrink = shrinkAssetId

instance (Arbitrary u, Ord u) => Arbitrary (UTxOIndex u) where
    arbitrary = genUTxOIndex arbitrary
    shrink = shrinkUTxOIndex shrink

instance Arbitrary TokenBundle where
    arbitrary = genTokenBundleSmallRangePositive
    shrink = shrinkTokenBundleSmallRangePositive

instance Arbitrary (SelectionFilter Asset) where
    arbitrary = genSelectionFilter
    shrink = shrinkSelectionFilter

genAsset :: Gen Asset
genAsset = oneof
    [ AssetLovelace & pure
    , Asset <$> genAssetId
    ]

shrinkAsset :: Asset -> [Asset]
shrinkAsset = \case
    AssetLovelace -> []
    Asset assetId -> AssetLovelace : (Asset <$> shrink assetId)

genSelectionFilter :: Gen (SelectionFilter Asset)
genSelectionFilter = oneof
    [ SelectSingleton <$> genAsset
    , SelectPairWith <$> genAsset
    , SelectAnyWith <$> genAsset
    , SelectAny & pure
    ]

shrinkSelectionFilter :: SelectionFilter Asset -> [SelectionFilter Asset]
shrinkSelectionFilter = traverse shrinkAsset

--------------------------------------------------------------------------------
-- Show instances
--------------------------------------------------------------------------------

instance Show (TestUTxO -> Bool) where
    show = const "(TestUTxO -> Bool)"
