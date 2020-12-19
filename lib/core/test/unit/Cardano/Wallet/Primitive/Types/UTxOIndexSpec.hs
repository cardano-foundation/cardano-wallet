{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.Types.UTxOIndexSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetIdSmallRange, shrinkAssetIdSmallRange )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn, TxOut )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxInSmallRange
    , genTxOutSmallRange
    , shrinkTxInSmallRange
    , shrinkTxOutSmallRange
    )
import Cardano.Wallet.Primitive.Types.UTxOIndex.Internal
    ( InvariantStatus (..), SelectionFilter (..), UTxOIndex, checkInvariant )
import Control.Monad
    ( replicateM )
import Control.Monad.Random.Class
    ( MonadRandom (..) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Data.Maybe
    ( isJust, isNothing )
import Data.Ratio
    ( (%) )
import Data.Word
    ( Word8 )
import Test.Hspec
    ( Spec, describe, it, parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , Confidence (..)
    , Gen
    , Property
    , checkCoverage
    , checkCoverageWith
    , choose
    , conjoin
    , counterexample
    , cover
    , oneof
    , property
    , shrinkList
    , stdConfidence
    , withMaxSuccess
    , (===)
    )
import Test.QuickCheck.Classes
    ( eqLaws )
import Test.QuickCheck.Extra
    ( shrinkInterleaved )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )
import Test.Utils.Laws
    ( testLawsMany )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex.Internal as UTxOIndex
import qualified Data.List as L
import qualified Data.Set as Set

spec :: Spec
spec =
    describe "Indexed UTxO set properties" $ do

    parallel $ describe "Class instances obey laws" $ do
        testLawsMany @UTxOIndex
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
        it "prop_insert_lookup" $
            property prop_insert_size

    parallel $ describe "Index Selection" $ do

        it "prop_selectRandom_empty" $
            property prop_selectRandom_empty
        it "prop_selectRandom_singleton" $
            property prop_selectRandom_singleton
        it "prop_selectRandom_one_any" $
            property prop_selectRandom_one_any
        it "prop_selectRandom_one_withAsset" $
            property prop_selectRandom_one_withAsset
        it "prop_selectRandom_all_any" $
            property prop_selectRandom_all_any
        it "prop_selectRandom_all_withAsset" $
            property prop_selectRandom_all_withAsset

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

invariantHolds :: UTxOIndex -> Property
invariantHolds u = checkInvariant u === InvariantHolds

prop_arbitrary_invariant :: UTxOIndex -> Property
prop_arbitrary_invariant = invariantHolds

prop_shrink_invariant :: UTxOIndex -> Property
prop_shrink_invariant = conjoin . fmap invariantHolds . shrink

prop_empty_invariant :: Property
prop_empty_invariant = invariantHolds UTxOIndex.empty

prop_singleton_invariant :: TxIn -> TxOut -> Property
prop_singleton_invariant i o = invariantHolds $ UTxOIndex.singleton i o

prop_fromSequence_invariant :: [(TxIn, TxOut)] -> Property
prop_fromSequence_invariant = invariantHolds . UTxOIndex.fromSequence

prop_insert_invariant :: TxIn -> TxOut -> UTxOIndex -> Property
prop_insert_invariant i o u = invariantHolds $ UTxOIndex.insert i o u

prop_delete_invariant :: TxIn -> UTxOIndex -> Property
prop_delete_invariant i u = invariantHolds $ UTxOIndex.delete i u

prop_selectRandom_invariant :: UTxOIndex -> SelectionFilter -> Property
prop_selectRandom_invariant i f = monadicIO $ do
    result <- run $ UTxOIndex.selectRandom i f
    assert $ case result of
        Nothing ->
            True
        Just (_, i') ->
            checkInvariant i' == InvariantHolds

--------------------------------------------------------------------------------
-- Construction and deconstruction properties
--------------------------------------------------------------------------------

prop_empty_toList :: Property
prop_empty_toList =
    UTxOIndex.toList UTxOIndex.empty === []

prop_singleton_toList :: TxIn -> TxOut -> Property
prop_singleton_toList i o =
    UTxOIndex.toList (UTxOIndex.singleton i o) === [(i, o)]

prop_toList_fromSequence :: UTxOIndex -> Property
prop_toList_fromSequence u =
    UTxOIndex.fromSequence (UTxOIndex.toList u) === u

--------------------------------------------------------------------------------
-- Modification properties
--------------------------------------------------------------------------------

prop_delete_balance :: TxIn -> UTxOIndex -> Property
prop_delete_balance i u =
    UTxOIndex.balance (UTxOIndex.delete i u) === expected
  where
    expected = case UTxOIndex.lookup i u of
        Nothing ->
            UTxOIndex.balance u
        Just o ->
            UTxOIndex.balance u `TokenBundle.subtract` view #tokens o

prop_delete_lookup :: TxIn -> UTxOIndex -> Property
prop_delete_lookup i u =
    UTxOIndex.lookup i (UTxOIndex.delete i u) === Nothing

prop_delete_size :: TxIn -> UTxOIndex -> Property
prop_delete_size i u =
    UTxOIndex.size (UTxOIndex.delete i u) === expected
  where
    expected = case UTxOIndex.lookup i u of
        Nothing ->
            UTxOIndex.size u
        Just _ ->
            UTxOIndex.size u - 1

prop_insert_assets :: TxIn -> TxOut -> UTxOIndex -> Property
prop_insert_assets i o u =
    UTxOIndex.assets (UTxOIndex.insert i o u)
        `Set.intersection` insertedAssets === insertedAssets
  where
    insertedAssets = TokenBundle.getAssets $ view #tokens o

prop_insert_balance :: TxIn -> TxOut -> UTxOIndex -> Property
prop_insert_balance i o u =
    UTxOIndex.balance (UTxOIndex.insert i o u) === expected
  where
    expected = view #tokens o `TokenBundle.add` case UTxOIndex.lookup i u of
        Nothing ->
            UTxOIndex.balance u
        Just o' ->
            UTxOIndex.balance u `TokenBundle.subtract` view #tokens o'

prop_insert_delete :: TxIn -> TxOut -> UTxOIndex -> Property
prop_insert_delete i o u =
    UTxOIndex.delete i (UTxOIndex.insert i o u) === expected
  where
    expected =
        if UTxOIndex.member i u then UTxOIndex.delete i u else u

prop_insert_lookup :: TxIn -> TxOut -> UTxOIndex -> Property
prop_insert_lookup i o u =
    UTxOIndex.lookup i (UTxOIndex.insert i o u) === Just o

prop_insert_size :: TxIn -> TxOut -> UTxOIndex -> Property
prop_insert_size i o u =
    UTxOIndex.size (UTxOIndex.insert i o u) === expected
  where
    expected = case UTxOIndex.lookup i u of
        Nothing ->
            UTxOIndex.size u + 1
        Just _ ->
            UTxOIndex.size u

--------------------------------------------------------------------------------
-- Index selection properties
--------------------------------------------------------------------------------

-- | Attempt to select a random entry from an empty index.
--
-- This should always return 'Nothing'.
--
prop_selectRandom_empty :: SelectionFilter -> Property
prop_selectRandom_empty f = monadicIO $ do
    result <- run $ UTxOIndex.selectRandom UTxOIndex.empty f
    assert $ isNothing result

-- | Attempt to select a random entry from a singleton index with entry 'e'.
--
-- This should always return 'Just e'.
--
prop_selectRandom_singleton :: SelectionFilter -> TxIn -> TxOut -> Property
prop_selectRandom_singleton selectionFilter i o = monadicIO $ do
    actual <- run $ UTxOIndex.selectRandom index selectionFilter
    assert $ actual == expected
  where
    index = UTxOIndex.singleton i o
    expected = case selectionFilter of
        Any ->
            Just ((i, o), UTxOIndex.empty)
        WithAsset a | txOutHasAsset o a ->
            Just ((i, o), UTxOIndex.empty)
        WithAsset _ ->
            Nothing

-- | Attempt to select a random entry with any combination of assets.
--
-- This should always succeed, provided the index is not empty.
--
prop_selectRandom_one_any :: UTxOIndex -> Property
prop_selectRandom_one_any u = checkCoverage $ monadicIO $ do
    result <- run $ UTxOIndex.selectRandom u Any
    monitor $ cover 90 (isJust result)
        "selected an entry"
    case result of
        Nothing ->
            assert $ UTxOIndex.null u
        Just ((i, o), u') -> do
            assert $ UTxOIndex.delete i u == u'
            assert $ UTxOIndex.insert i o u' == u
            assert $ UTxOIndex.member i u
            assert $ not $ UTxOIndex.member i u'
            assert $ u /= u'

-- | Attempt to select a random element with a specific asset.
--
-- This should only succeed if there is at least one element with a non-zero
-- quantity of the asset.
--
prop_selectRandom_one_withAsset :: UTxOIndex -> AssetId -> Property
prop_selectRandom_one_withAsset u a = checkCoverage $ monadicIO $ do
    result <- run $ UTxOIndex.selectRandom u (WithAsset a)
    monitor $ cover 90 (a `Set.member` UTxOIndex.assets u)
        "index has the specified asset"
    monitor $ cover 90 (Set.size (UTxOIndex.assets u) > 1)
        "index has more than one asset"
    monitor $ cover 90 (isJust result)
        "selected an entry"
    case result of
        Nothing ->
            assert $ a `Set.notMember` UTxOIndex.assets u
        Just ((i, o), u') -> do
            assert $ UTxOIndex.delete i u == u'
            assert $ UTxOIndex.insert i o u' == u
            assert $ UTxOIndex.member i u
            assert $ not $ UTxOIndex.member i u'
            assert $ txOutHasAsset o a
            assert $ u /= u'

-- | Attempt to select all entries from the index.
--
-- This should always succeed.
--
prop_selectRandom_all_any :: UTxOIndex -> Property
prop_selectRandom_all_any u = checkCoverage $ monadicIO $ do
    (selectedEntries, u') <- run $ selectAll Any u
    monitor $ cover 90 (not (null selectedEntries))
        "selected at least one entry"
    assert $ L.sort selectedEntries == L.sort (UTxOIndex.toList u)
    assert $ UTxOIndex.assets u' == mempty
    assert $ UTxOIndex.balance u' == mempty
    assert $ UTxOIndex.fromSequence selectedEntries == u
    assert $ UTxOIndex.null u'
    assert $ length selectedEntries == UTxOIndex.size u

-- | Attempt to select all entries with the given asset from the index.
--
prop_selectRandom_all_withAsset :: UTxOIndex -> AssetId -> Property
prop_selectRandom_all_withAsset u a = checkCoverage $ monadicIO $ do
    (selectedEntries, u') <- run $ selectAll (WithAsset a) u
    monitor $ cover 90 (a `Set.member` UTxOIndex.assets u)
        "index has the specified asset"
    monitor $ cover 90 (Set.size (UTxOIndex.assets u) > 1)
        "index has more than one asset"
    monitor $ cover 90 (not (null selectedEntries))
        "selected at least one entry"
    assert $ L.all (\(_, o) -> not (txOutHasAsset o a)) (UTxOIndex.toList u')
    assert $ L.all (\(_, o) -> txOutHasAsset o a) selectedEntries
    assert $ UTxOIndex.deleteMany (fst <$> selectedEntries) u == u'
    assert $ UTxOIndex.insertMany selectedEntries u' == u
    assert $ a `Set.notMember` UTxOIndex.assets u'

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
    elementCount = greatest - smallest
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
    :: MonadRandom m
    => SelectionFilter
    -> UTxOIndex
    -> m ([(TxIn, TxOut)], UTxOIndex)
selectAll sf = go []
  where
    go !selectedEntries !u = do
        selected <- UTxOIndex.selectRandom u sf
        case selected of
            Nothing ->
                -- There are no more entries available. Terminate here:
                pure (selectedEntries, u)
            Just ((i, o), uReduced) ->
                go ((i, o) : selectedEntries) uReduced

-- | Returns 'True' if (and only if) the given transaction output has a non-zero
--   quantity of the given asset.
--
txOutHasAsset :: TxOut -> AssetId -> Bool
txOutHasAsset = TokenBundle.hasQuantity . view #tokens

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary AssetId where
    arbitrary = genAssetIdSmallRange
    shrink = shrinkAssetIdSmallRange

instance Arbitrary UTxOIndex where
    arbitrary = do
        entryCount <- choose (0, 64)
        UTxOIndex.fromSequence <$> replicateM entryCount genEntrySmallRange
    shrink
        = take 16
        . fmap UTxOIndex.fromSequence
        . shrinkList shrinkEntrySmallRange
        . UTxOIndex.toList

genEntrySmallRange :: Gen (TxIn, TxOut)
genEntrySmallRange = (,)
    <$> genTxInSmallRange
    <*> genTxOutSmallRange

shrinkEntrySmallRange :: (TxIn, TxOut) -> [(TxIn, TxOut)]
shrinkEntrySmallRange (i, o) = uncurry (,) <$> shrinkInterleaved
    (i, shrinkTxInSmallRange)
    (o, shrinkTxOutSmallRange)

instance Arbitrary TxIn where
    arbitrary = genTxInSmallRange
    shrink = shrinkTxInSmallRange

instance Arbitrary TxOut where
    arbitrary = genTxOutSmallRange
    shrink = shrinkTxOutSmallRange

instance Arbitrary SelectionFilter where
    arbitrary = genSelectionFilterSmallRange
    shrink = shrinkSelectionFilterSmallRange

genSelectionFilterSmallRange :: Gen SelectionFilter
genSelectionFilterSmallRange = oneof
    [ pure Any
    , WithAsset <$> genAssetIdSmallRange
    ]

shrinkSelectionFilterSmallRange :: SelectionFilter -> [SelectionFilter]
shrinkSelectionFilterSmallRange = \case
    Any -> []
    WithAsset a -> Any : (WithAsset <$> shrinkAssetIdSmallRange a)
