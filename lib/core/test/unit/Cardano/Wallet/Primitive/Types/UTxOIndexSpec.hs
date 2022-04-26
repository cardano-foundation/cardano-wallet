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
    ( Asset (..)
    , InvariantStatus (..)
    , SelectionFilter (..)
    , SelectionFilterNew (..)
    , UTxOIndex
    , checkInvariant
    )
import Control.Monad.Random.Class
    ( MonadRandom (..) )
import Data.Function
    ( (&) )
import Data.Maybe
    ( isJust, isNothing )
import Data.Ratio
    ( (%) )
import Data.Word
    ( Word8 )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , CoArbitrary (..)
    , Confidence (..)
    , Gen
    , Property
    , Testable
    , checkCoverage
    , checkCoverageWith
    , conjoin
    , counterexample
    , cover
    , forAll
    , oneof
    , property
    , stdConfidence
    , suchThat
    , withMaxSuccess
    , (===)
    )
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
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec =
    describe "Cardano.Wallet.Primitive.Types.UTxOIndexSpec" $ do

    parallel $ describe "Class instances obey laws" $ do
        testLawsMany @(UTxOIndex TestUTxO)
            [ eqLaws
            ]

    parallel $ describe
        "All operations preserve the invariant:" $ do

        it "prop_arbitrary_invariant" $
            property prop_arbitrary_invariant
        it "prop_shrink_invariant" $
            property prop_shrink_invariant
        it "prop_empty_invariant" $
            property prop_empty_invariant
        it "prop_singleton_invariant" $
            property prop_singleton_invariant
        it "prop_fromSequence_invariant" $
            property prop_fromSequence_invariant
        it "prop_insert_invariant" $
            property prop_insert_invariant
        it "prop_delete_invariant" $
            property prop_delete_invariant
        it "prop_selectRandom_invariant" $
            property prop_selectRandom_invariant

    parallel $ describe "Construction and deconstruction" $ do

        it "prop_empty_toList" $
            property prop_empty_toList
        it "prop_singleton_toList" $
            property prop_singleton_toList
        it "prop_toList_fromSequence" $
            property prop_toList_fromSequence

    parallel $ describe "Modification" $ do

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

    parallel $ describe "Filtering and partitioning" $ do

        it "prop_filter_disjoint" $
            property prop_filter_disjoint
        it "prop_filter_partition" $
            property prop_filter_partition
        it "prop_filter_toList" $
            property prop_filter_toList
        it "prop_partition_disjoint" $
            property prop_partition_disjoint

    parallel $ describe "Index Selection" $ do

        it "prop_SelectionFilter_coverage" $
            property prop_SelectionFilter_coverage
        it "prop_SelectionFilterNew_coverage" $
            property prop_SelectionFilterNew_coverage
        it "prop_selectRandom_empty" $
            property prop_selectRandom_empty
        it "prop_selectRandom_singleton" $
            property prop_selectRandom_singleton
        it "prop_selectRandom_one_any" $
            property prop_selectRandom_one_any
        it "prop_selectRandom_one_withAdaOnly" $
            property prop_selectRandom_one_withAdaOnly
        it "prop_selectRandom_one_withAsset" $
            property prop_selectRandom_one_withAsset
        it "prop_selectRandom_one_withAssetOnly" $
            property prop_selectRandom_one_withAssetOnly
        it "prop_selectRandom_all_any" $
            property prop_selectRandom_all_any
        it "prop_selectRandom_all_withAdaOnly" $
            property prop_selectRandom_all_withAdaOnly
        it "prop_selectRandom_all_withAsset" $
            property prop_selectRandom_all_withAsset
        it "prop_selectRandom_all_withAssetOnly" $
            property prop_selectRandom_all_withAssetOnly
        it "prop_selectRandomWithPriority" $
            property prop_selectRandomWithPriority

    parallel $ describe "Set Selection" $ do

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

prop_fromSequence_invariant :: [(TestUTxO, TokenBundle)] -> Property
prop_fromSequence_invariant = invariantHolds . UTxOIndex.fromSequence

prop_insert_invariant
    :: u ~ TestUTxO => u -> TokenBundle -> UTxOIndex u -> Property
prop_insert_invariant u b i = invariantHolds $ UTxOIndex.insert u b i

prop_delete_invariant
    :: u ~ TestUTxO => u -> UTxOIndex u -> Property
prop_delete_invariant u i = invariantHolds $ UTxOIndex.delete u i

prop_selectRandom_invariant
    :: UTxOIndex TestUTxO -> SelectionFilterNew Asset -> Property
prop_selectRandom_invariant i f =
    monadicIO $ do
        run $ do
            result <- UTxOIndex.selectRandomNew i f
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
            UTxOIndex.balance i `TokenBundle.unsafeSubtract` b

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
    insertedAssets = TokenBundle.getAssets b

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
            UTxOIndex.balance i `TokenBundle.unsafeSubtract` b'

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

-- | The set of possible 'SelectionFilter' categories.
--
data SelectionFilterCategory
    = MatchAny
    | MatchWithAdaOnly
    | MatchWithAsset
    | MatchWithAssetOnly
    deriving (Eq, Show)

-- | Categorizes a 'SelectionFilter', removing its arguments.
--
categorizeSelectionFilter :: SelectionFilter -> SelectionFilterCategory
categorizeSelectionFilter = \case
    Any             -> MatchAny
    WithAdaOnly     -> MatchWithAdaOnly
    WithAsset     _ -> MatchWithAsset
    WithAssetOnly _ -> MatchWithAssetOnly

prop_SelectionFilter_coverage :: SelectionFilter -> Property
prop_SelectionFilter_coverage selectionFilter = checkCoverage $ property
    $ cover 20 (category == MatchAny)
        "Any"
    $ cover 20 (category == MatchWithAdaOnly)
        "WithAdaOnly"
    $ cover 20 (category == MatchWithAsset)
        "WithAsset"
    $ cover 20 (category == MatchWithAssetOnly)
        "WithAssetOnly"
    True
  where
    category = categorizeSelectionFilter selectionFilter

-- TODO:
--
-- Rename this to 'prop_SelectionFilterNew_coverage' once the old property
-- has been deleted.
--
prop_SelectionFilterNew_coverage :: SelectionFilterNew Asset -> Property
prop_SelectionFilterNew_coverage selectionFilter = checkCoverage $ property
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
    category = () <$ selectionFilter

-- | Attempt to select a random entry from an empty index.
--
-- This should always return 'Nothing'.
--
prop_selectRandom_empty :: SelectionFilterNew Asset -> Property
prop_selectRandom_empty f = monadicIO $ do
    result <- run $ UTxOIndex.selectRandomNew (UTxOIndex.empty @TestUTxO) f
    assert $ isNothing result

-- | Attempt to select a random entry from a singleton index with entry 'e'.
--
-- This should always return 'Just e'.
--
prop_selectRandom_singleton
    :: SelectionFilterNew Asset
    -> TestUTxO
    -> TokenBundle
    -> Property
prop_selectRandom_singleton selectionFilter u b = monadicIO $ do
    actual <- run $ UTxOIndex.selectRandomNew index selectionFilter
    pure $ prop_inner actual

  where
    prop_inner actual =
        checkCoverage $
        cover 10 (isJust actual)
            "selected something" $
        cover 10 (isNothing actual)
            "selected nothing" $
        actual === expected

    index = UTxOIndex.singleton u b
    expected = case selectionFilter of
        SelectSingleton a
            | a `Set.member` UTxOIndex.tokenBundleAssets b
            , UTxOIndex.tokenBundleAssetCount b == 1 ->
                Just ((u, b), UTxOIndex.empty)
        SelectPairWith a
            | a `Set.member` UTxOIndex.tokenBundleAssets b
            , UTxOIndex.tokenBundleAssetCount b == 2 ->
                Just ((u, b), UTxOIndex.empty)
        SelectAnyWith a
            | a `Set.member` UTxOIndex.tokenBundleAssets b ->
                Just ((u, b), UTxOIndex.empty)
        SelectAny ->
                Just ((u, b), UTxOIndex.empty)
        _ ->
            Nothing

-- | Attempt to select a random entry with any combination of assets.
--
-- This should always succeed, provided the index is not empty.
--
prop_selectRandom_one_any :: UTxOIndex TestUTxO -> Property
prop_selectRandom_one_any i = checkCoverage $ monadicIO $ do
    result <- run $ UTxOIndex.selectRandomNew i SelectAny
    monitor $ cover 90 (isJust result)
        "selected an entry"
    case result of
        Nothing ->
            assert $ UTxOIndex.null i
        Just ((u, b), i') -> do
            assert $ UTxOIndex.delete u i == i'
            assert $ UTxOIndex.insert u b i' == i
            assert $ UTxOIndex.member u i
            assert $ not $ UTxOIndex.member u i'
            assert $ i /= i'

-- | Attempt to select a random entry with only ada.
--
prop_selectRandom_one_withAdaOnly :: UTxOIndex TestUTxO -> Property
prop_selectRandom_one_withAdaOnly i = checkCoverage $ monadicIO $ do
    result <- run $ UTxOIndex.selectRandomNew i (SelectSingleton AssetLovelace)
    monitor $ cover 50 (isJust result)
        "selected an entry"
    case result of
        Nothing ->
            assert utxoHasNoAdaOnlyEntries
        Just ((u, b), i') -> do
            assert $ UTxOIndex.delete u i == i'
            assert $ UTxOIndex.insert u b i' == i
            assert $ UTxOIndex.member u i
            assert $ not $ UTxOIndex.member u i'
            assert $ i /= i'
  where
    utxoHasNoAdaOnlyEntries =
        not (any (tokenBundleIsAdaOnly . snd) (UTxOIndex.toList i))

-- | Attempt to select a random element with a specific asset.
--
-- This should only succeed if there is at least one element with a non-zero
-- quantity of the asset.
--
prop_selectRandom_one_withAsset :: UTxOIndex TestUTxO -> Asset -> Property
prop_selectRandom_one_withAsset i a = checkCoverage $ monadicIO $ do
    result <- run $ UTxOIndex.selectRandomNew i (SelectAnyWith a)
    monitor $ cover 50 (a `Set.member` UTxOIndex.assetsNew i)
        "index has the specified asset"
    monitor $ cover 50 (Set.size (UTxOIndex.assetsNew i) > 1)
        "index has more than one asset"
    monitor $ cover 50 (isJust result)
        "selected an entry"
    case result of
        Nothing ->
            assert $ a `Set.notMember` UTxOIndex.assetsNew i
        Just ((u, b), i') -> do
            assert $ UTxOIndex.delete u i == i'
            assert $ UTxOIndex.insert u b i' == i
            assert $ UTxOIndex.member u i
            assert $ not $ UTxOIndex.member u i'
            assert $ UTxOIndex.tokenBundleHasAsset b a
            assert $ i /= i'

-- | Attempt to select a random element with a specific asset and no other
--   assets.
--
-- This should only succeed if there is at least one element with a non-zero
-- quantity of the asset and no other assets.
--
prop_selectRandom_one_withAssetOnly
    :: UTxOIndex TestUTxO -> Asset -> Property
prop_selectRandom_one_withAssetOnly i a = checkCoverage $ monadicIO $ do
    result <- run $ UTxOIndex.selectRandomNew i (SelectSingleton a)
    monitor $ cover 50 (a `Set.member` UTxOIndex.assetsNew i)
        "index has the specified asset"
    monitor $ cover 50 (Set.size (UTxOIndex.assetsNew i) > 1)
        "index has more than one asset"
    monitor $ cover 10 (isJust result)
        "selected an entry"
    case result of
        Nothing ->
            assert True
        Just ((u, b), i') -> do
            assert $ UTxOIndex.delete u i == i'
            assert $ UTxOIndex.insert u b i' == i
            assert $ UTxOIndex.member u i
            assert $ not $ UTxOIndex.member u i'
            assert $ UTxOIndex.tokenBundleHasAsset b a
            assert $ UTxOIndex.tokenBundleAssetCount b == 1
            assert $ i /= i'

-- | Attempt to select all entries from the index.
--
-- This should always succeed.
--
prop_selectRandom_all_any :: UTxOIndex TestUTxO -> Property
prop_selectRandom_all_any i = checkCoverage $ monadicIO $ do
    (selectedEntries, i') <- run $ selectAllNew SelectAny i
    monitor $ cover 90 (not (null selectedEntries))
        "selected at least one entry"
    assert $ (==)
        (L.sort $ show <$> selectedEntries)
        (L.sort $ show <$> UTxOIndex.toList i)
    assert $ UTxOIndex.assetsNew i' == mempty
    assert $ UTxOIndex.balance i' == mempty
    assert $ UTxOIndex.fromSequence selectedEntries == i
    assert $ UTxOIndex.null i'
    assert $ length selectedEntries == UTxOIndex.size i

-- | Attempt to select all entries with only ada from the index.
--
prop_selectRandom_all_withAdaOnly :: UTxOIndex TestUTxO -> Property
prop_selectRandom_all_withAdaOnly i = checkCoverage $ monadicIO $ do
    (selectedEntries, i') <- run $
        selectAllNew (SelectSingleton AssetLovelace) i
    monitor $ cover 70 (not (null selectedEntries))
        "selected at least one entry"
    assert $ L.all (\(_, b) ->
        not (tokenBundleIsAdaOnly b)) (UTxOIndex.toList i')
    assert $ L.all (\(_, b) ->
        tokenBundleIsAdaOnly b) selectedEntries
    assert $ UTxOIndex.deleteMany (fst <$> selectedEntries) i == i'
    assert $ UTxOIndex.insertMany selectedEntries i' == i

-- | Attempt to select all entries with the given asset from the index.
--
prop_selectRandom_all_withAsset :: UTxOIndex TestUTxO -> Asset -> Property
prop_selectRandom_all_withAsset i a = checkCoverage $ monadicIO $ do
    (selectedEntries, i') <- run $ selectAllNew (SelectAnyWith a) i
    monitor $ cover 50 (a `Set.member` UTxOIndex.assetsNew i)
        "index has the specified asset"
    monitor $ cover 50 (Set.size (UTxOIndex.assetsNew i) > 1)
        "index has more than one asset"
    monitor $ cover 50 (not (null selectedEntries))
        "selected at least one entry"
    assert $ L.all (\(_, b) ->
        not (UTxOIndex.tokenBundleHasAsset b a)) (UTxOIndex.toList i')
    assert $ L.all (\(_, b) ->
        UTxOIndex.tokenBundleHasAsset b a) selectedEntries
    assert $ UTxOIndex.deleteMany (fst <$> selectedEntries) i == i'
    assert $ UTxOIndex.insertMany selectedEntries i' == i
    assert $ a `Set.notMember` UTxOIndex.assetsNew i'

-- | Attempt to select all entries with only the given asset from the index.
--
prop_selectRandom_all_withAssetOnly
    :: UTxOIndex TestUTxO -> Asset -> Property
prop_selectRandom_all_withAssetOnly i a = checkCoverage $ monadicIO $ do
    (selectedEntries, i') <- run $ selectAllNew (SelectSingleton a) i
    monitor $ cover 50 (a `Set.member` UTxOIndex.assetsNew i)
        "index has the specified asset"
    monitor $ cover 50 (Set.size (UTxOIndex.assetsNew i) > 1)
        "index has more than one asset"
    monitor $ cover 10 (not (null selectedEntries))
        "selected at least one entry"
    assert $ all (\(_, b) ->
        not (tokenBundleHasAssetOnly b a)) (UTxOIndex.toList i')
    assert $ all (\(_, b) ->
        tokenBundleHasAssetOnly b a) selectedEntries
    assert $ UTxOIndex.deleteMany (fst <$> selectedEntries) i == i'
    assert $ UTxOIndex.insertMany selectedEntries i' == i

-- | Verify that priority order is respected when selecting with more than
--   one filter.
--
prop_selectRandomWithPriority :: UTxOIndex TestUTxO -> Property
prop_selectRandomWithPriority i =
    forAll (genAsset) $ \a1 ->
    forAll (genAsset `suchThat` (/= a1)) $ \a2 ->
    checkCoverage $ monadicIO $ do
        haveMatchForAsset1 <- isJust <$>
            run (UTxOIndex.selectRandomNew i $ SelectPairWith a1)
        haveMatchForAsset2 <- isJust <$>
            run (UTxOIndex.selectRandomNew i $ SelectPairWith a2)
        monitor $ cover 4 (haveMatchForAsset1 && not haveMatchForAsset2)
            "have match for asset 1 but not for asset 2"
        monitor $ cover 4 (not haveMatchForAsset1 && haveMatchForAsset2)
            "have match for asset 2 but not for asset 1"
        monitor $ cover 4 (haveMatchForAsset1 && haveMatchForAsset2)
            "have match for both asset 1 and asset 2"
        monitor $ cover 4 (not haveMatchForAsset1 && not haveMatchForAsset2)
            "have match for neither asset 1 nor asset 2"
        result <- run $ UTxOIndex.selectRandomWithPriorityNew i
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

-- | Selects all UTxO entries matching a particular filter.
--
-- Returns a list of all the entries that matched, and an updated index with
-- the selected entries removed.
--
selectAll
    :: (MonadRandom m, u ~ TestUTxO)
    => SelectionFilter
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

-- TODO:
--
-- Rename to 'selectAll' once the old function has been deleted.
--
selectAllNew
    :: (MonadRandom m, u ~ TestUTxO)
    => SelectionFilterNew Asset
    -> UTxOIndex u
    -> m ([(u, TokenBundle)], UTxOIndex u)
selectAllNew sf = go []
  where
    go !selectedEntries !i = do
        selected <- UTxOIndex.selectRandomNew i sf
        case selected of
            Nothing ->
                -- There are no more entries available. Terminate here:
                pure (selectedEntries, i)
            Just ((u, b), iReduced) ->
                go ((u, b) : selectedEntries) iReduced

-- | Returns 'True' if (and only if) the given token bundle has a non-zero
--   quantity of the given asset.
--
tokenBundleHasAsset :: TokenBundle -> AssetId -> Bool
tokenBundleHasAsset = TokenBundle.hasQuantity

-- | Returns 'True' if (and only if) the given token bundle has a non-zero
--   quantity of the given asset and no other non-ada assets.
--
tokenBundleHasAssetOnly :: TokenBundle -> Asset -> Bool
tokenBundleHasAssetOnly b a = (== [a])
    $ Set.toList $ UTxOIndex.tokenBundleAssets b

-- | Returns 'True' if (and only if) the given token bundle contains no
--   assets other than ada.
--
tokenBundleIsAdaOnly :: TokenBundle -> Bool
tokenBundleIsAdaOnly = TokenBundle.isCoin

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

instance Arbitrary SelectionFilter where
    arbitrary = genSelectionFilterSmallRange
    shrink = shrinkSelectionFilterSmallRange

genSelectionFilterSmallRange :: Gen SelectionFilter
genSelectionFilterSmallRange = oneof
    [ pure Any
    , pure WithAdaOnly
    , WithAsset <$> genAssetId
    , WithAssetOnly <$> genAssetId
    ]

shrinkSelectionFilterSmallRange :: SelectionFilter -> [SelectionFilter]
shrinkSelectionFilterSmallRange = \case
    Any -> []
    WithAdaOnly -> [Any]
    WithAsset a ->
        case WithAsset <$> shrinkAssetId a of
            [] -> [WithAdaOnly]
            xs -> xs
    WithAssetOnly a ->
        case WithAssetOnly <$> shrinkAssetId a of
            [] -> [WithAsset a]
            xs -> xs

instance Arbitrary (SelectionFilterNew Asset) where
    arbitrary = genSelectionFilterNew
    shrink = shrinkSelectionFilterNew

genAsset :: Gen Asset
genAsset = oneof
    [ AssetLovelace & pure
    , Asset <$> genAssetId
    ]

shrinkAsset :: Asset -> [Asset]
shrinkAsset = \case
    AssetLovelace -> []
    Asset assetId -> AssetLovelace : (Asset <$> shrink assetId)

genSelectionFilterNew :: Gen (SelectionFilterNew Asset)
genSelectionFilterNew = oneof
    [ SelectSingleton <$> genAsset
    , SelectPairWith <$> genAsset
    , SelectAnyWith <$> genAsset
    , SelectAny & pure
    ]

shrinkSelectionFilterNew
    :: SelectionFilterNew Asset -> [SelectionFilterNew Asset]
shrinkSelectionFilterNew = traverse shrinkAsset

--------------------------------------------------------------------------------
-- Show instances
--------------------------------------------------------------------------------

instance Show (TestUTxO -> Bool) where
    show = const "(TestUTxO -> Bool)"
