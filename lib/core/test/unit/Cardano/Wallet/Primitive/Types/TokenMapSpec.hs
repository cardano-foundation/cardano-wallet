{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Use camelCase" -}

module Cardano.Wallet.Primitive.Types.TokenMapSpec
    ( spec
    ) where

import Prelude

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Cardano.Numeric.Util
    ( inAscendingPartialOrder )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..), Flat (..), Nested (..), TokenMap, difference )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( AssetIdF (..)
    , genAssetIdLargeRange
    , genAssetIdSmallRange
    , genTokenMapSmallRange
    , shrinkAssetIdSmallRange
    , shrinkTokenMapSmallRange
    )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName, TokenPolicyId, mkTokenName )
import Cardano.Wallet.Primitive.Types.TokenPolicy.Gen
    ( genTokenNameSmallRange
    , genTokenPolicyIdSmallRange
    , shrinkTokenNameSmallRange
    , shrinkTokenPolicyIdSmallRange
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity.Gen
    ( genTokenQuantitySmall
    , genTokenQuantitySmallPositive
    , shrinkTokenQuantitySmall
    , shrinkTokenQuantitySmallPositive
    )
import Control.Monad
    ( replicateM )
import Data.Aeson
    ( FromJSON (..), ToJSON (..) )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.Bifunctor
    ( bimap, first, second )
import Data.ByteString
    ( ByteString )
import Data.Either
    ( fromRight )
import Data.Function
    ( (&) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( mapMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Ratio
    ( (%) )
import Data.String.QQ
    ( s )
import Data.Text
    ( Text )
import Data.Text.Class
    ( fromText, toText )
import Data.Typeable
    ( Typeable )
import Fmt
    ( pretty )
import Numeric.Natural
    ( Natural )
import System.FilePath
    ( (</>) )
import Test.Hspec
    ( Expectation, Spec, describe, it, shouldBe )
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , Blind (..)
    , Fun
    , Property
    , applyFun
    , checkCoverage
    , choose
    , conjoin
    , counterexample
    , cover
    , frequency
    , property
    , (.||.)
    , (===)
    , (==>)
    )
import Test.QuickCheck.Classes
    ( eqLaws, monoidLaws, semigroupLaws, semigroupMonoidLaws )
import Test.Utils.Laws
    ( testLawsMany )
import Test.Utils.Laws.PartialOrd
    ( partialOrdLaws )
import Test.Utils.Paths
    ( getTestData )

import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as TokenQuantity
import qualified Data.Aeson.Types as Aeson
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Test.Utils.Roundtrip as Roundtrip

spec :: Spec
spec =
    describe "Token map properties" $
    modifyMaxSuccess (const 1000) $ do

    parallel $ describe "Class instances obey laws" $ do
        testLawsMany @TokenMap
            [ eqLaws
            , monoidLaws
            , partialOrdLaws
            , semigroupLaws
            , semigroupMonoidLaws
            ]

    parallel $ describe
        "All operations preserve the invariant: \
        \all token quantities held within a map are non-zero" $ do

        it "prop_arbitrary_invariant" $
            property prop_arbitrary_invariant
        it "prop_shrink_invariant" $
            property prop_shrink_invariant
        it "prop_empty_invariant" $
            property prop_empty_invariant
        it "prop_singleton_invariant" $
            property prop_singleton_invariant
        it "prop_fromFlatList_invariant" $
            property prop_fromFlatList_invariant
        it "prop_fromNestedList_invariant" $
            property prop_fromNestedList_invariant
        it "prop_add_invariant" $
            property prop_add_invariant
        it "prop_subtract_invariant" $
            property prop_subtract_invariant
        it "prop_difference_invariant" $
            property prop_difference_invariant
        it "prop_intersection_invariant" $
            property prop_intersection_invariant
        it "prop_setQuantity_invariant" $
            property prop_setQuantity_invariant
        it "prop_adjustQuantity_invariant" $
            property prop_adjustQuantity_invariant

    parallel $ describe "Construction and deconstruction" $ do

        it "prop_fromFlatList" $
            property prop_fromFlatList
        it "prop_fromNestedList" $
            property prop_fromNestedList
        it "prop_empty_toFlatList" $
            property prop_empty_toFlatList
        it "prop_singleton_toFlatList" $
            property prop_singleton_toFlatList
        it "prop_toFlatList_fromFlatList" $
            property prop_toFlatList_fromFlatList
        it "prop_toNestedList_fromNestedList" $
            property prop_toNestedList_fromNestedList

    parallel $ describe "Filtering" $ do

        it "prop_filter_conjoin" $
            property prop_filter_conjoin
        it "prop_filter_partition" $
            property prop_filter_partition
        it "prop_filter_twice" $
            property prop_filter_twice

    parallel $ describe "Arithmetic" $ do

        it "prop_add_commutative" $
            property prop_add_commutative
        it "prop_add_associative" $
            property prop_add_associative
        it "prop_add_subtract_associative" $
            property prop_add_subtract_associative
        it "prop_subtract_null" $
            property prop_subtract_null
        it "prop_difference_zero (x - 0 = x)" $
            property prop_difference_zero
        it "prop_difference_zero2 (0 - x = 0)" $
            property prop_difference_zero2
        it "prop_difference_zero3 (x - x = 0)" $
            property prop_difference_zero3
        it "prop_difference_leq (x - y ⊆ x)" $
            property prop_difference_leq
        it "prop_difference_add ((x - y) + y ⊇ x)" $
            property prop_difference_add
        it "prop_difference_subtract" $
            property prop_difference_subtract
        it "prop_difference_equality" $
            property prop_difference_equality
        it "prop_intersection_associativity" $
            property prop_intersection_associativity
        it "prop_intersection_commutativity" $
            property prop_intersection_commutativity
        it "prop_intersection_empty" $
            property prop_intersection_empty
        it "prop_intersection_equality" $
            property prop_intersection_equality
        it "prop_intersection_identity" $
            property prop_intersection_identity
        it "prop_intersection_subset" $
            property prop_intersection_subset

    parallel $ describe "Quantities" $ do

        it "prop_removeQuantity_isEmpty" $
            property prop_removeQuantity_isEmpty
        it "prop_setQuantity_getQuantity" $
            property prop_setQuantity_getQuantity
        it "prop_setQuantity_hasQuantity" $
            property prop_setQuantity_hasQuantity
        it "prop_adjustQuantity_getQuantity" $
            property prop_adjustQuantity_getQuantity
        it "prop_adjustQuantity_hasQuantity" $
            property prop_adjustQuantity_hasQuantity
        it "prop_maximumQuantity_all" $
            property prop_maximumQuantity_all

    parallel $ describe "Partitioning assets" $ do

        it "prop_equipartitionAssets_coverage" $
            property prop_equipartitionAssets_coverage
        it "prop_equipartitionAssets_length" $
            property prop_equipartitionAssets_length
        it "prop_equipartitionAssets_sizes" $
            property prop_equipartitionAssets_sizes
        it "prop_equipartitionAssets_sum" $
            property prop_equipartitionAssets_sum

    parallel $ describe "Partitioning quantities" $ do

        it "prop_equipartitionQuantities_fair" $
            property prop_equipartitionQuantities_fair
        it "prop_equipartitionQuantities_length" $
            property prop_equipartitionQuantities_length
        it "prop_equipartitionQuantities_order" $
            property prop_equipartitionQuantities_order
        it "prop_equipartitionQuantities_sum" $
            property prop_equipartitionQuantities_sum

    parallel $ describe "Partitioning quantities with an upper bound" $ do

        it "prop_equipartitionQuantitiesWithUpperBound_coverage" $
            property prop_equipartitionQuantitiesWithUpperBound_coverage
        it "prop_equipartitionQuantitiesWithUpperBound_length" $
            property prop_equipartitionQuantitiesWithUpperBound_length
        it "prop_equipartitionQuantitiesWithUpperBound_max" $
            property prop_equipartitionQuantitiesWithUpperBound_max
        it "prop_equipartitionQuantitiesWithUpperBound_order" $
            property prop_equipartitionQuantitiesWithUpperBound_order
        it "prop_equipartitionQuantitiesWithUpperBound_sum" $
            property prop_equipartitionQuantitiesWithUpperBound_sum

    parallel $ describe "JSON serialization" $ do

        describe "Roundtrip tests" $ do
            testJson $ Proxy @(Flat TokenMap)
            testJson $ Proxy @(Nested TokenMap)

        describe "Negative tests" $ do
            it "Zero-valued token quantity (from flat representation)"
                testZeroValuedTokenQuantityFlat
            it "Zero-valued token quantity (from nested representation)"
                testZeroValuedTokenQuantityNested
            it "Empty token list"
                testEmptyTokenList

    parallel $ describe "Textual serialization" $ do
        it "Flat style" $
            property testPrettyFlat
        it "Nested style" $
            property testPrettyNested

--------------------------------------------------------------------------------
-- Invariant properties
--------------------------------------------------------------------------------

-- Tests that all quantities within the given map are non-zero.
--
invariantHolds :: TokenMap -> Bool
invariantHolds b =
    all TokenQuantity.isNonZero $ getQuantity <$> TokenMap.toFlatList b
  where
    getQuantity (_, q) = q

prop_arbitrary_invariant :: TokenMap -> Property
prop_arbitrary_invariant = property . invariantHolds

prop_shrink_invariant :: TokenMap -> Property
prop_shrink_invariant b = property $ all invariantHolds $ shrink b

prop_empty_invariant :: Property
prop_empty_invariant = property $ invariantHolds TokenMap.empty

prop_singleton_invariant :: (AssetId, TokenQuantity) -> Property
prop_singleton_invariant (asset, quantity) = property $
    invariantHolds $ TokenMap.singleton asset quantity

prop_fromFlatList_invariant :: [(AssetId, TokenQuantity)] -> Property
prop_fromFlatList_invariant entries =
    property $ invariantHolds $ TokenMap.fromFlatList entries

prop_fromNestedList_invariant
    :: [(TokenPolicyId, NonEmpty (TokenName, TokenQuantity))] -> Property
prop_fromNestedList_invariant entries =
    property $ invariantHolds $ TokenMap.fromNestedList entries

prop_add_invariant :: TokenMap -> TokenMap -> Property
prop_add_invariant b1 b2 = property $ invariantHolds $ TokenMap.add b1 b2

prop_subtract_invariant :: TokenMap -> TokenMap -> Property
prop_subtract_invariant m1 m2 = property $
    m2 `leq` m1 ==> invariantHolds result
  where
    Just result = TokenMap.subtract m1 m2

prop_difference_invariant :: TokenMap -> TokenMap -> Property
prop_difference_invariant m1 m2 =
    property $ invariantHolds $ TokenMap.difference m1 m2

prop_intersection_invariant :: TokenMap -> TokenMap -> Property
prop_intersection_invariant m1 m2 =
    property $ invariantHolds $ TokenMap.intersection m1 m2

prop_setQuantity_invariant
    :: TokenMap -> AssetId -> TokenQuantity -> Property
prop_setQuantity_invariant b asset quantity = property $
    invariantHolds $ TokenMap.setQuantity b asset quantity

prop_adjustQuantity_invariant :: TokenMap -> AssetId -> Property
prop_adjustQuantity_invariant b asset = property $
    invariantHolds $ TokenMap.adjustQuantity b asset adjust
  where
    adjust quantity
        | quantity > TokenQuantity.zero = TokenQuantity.pred quantity
        | otherwise = quantity

--------------------------------------------------------------------------------
-- Construction and deconstruction properties
--------------------------------------------------------------------------------

prop_fromFlatList :: [(AssetId, TokenQuantity)] -> Property
prop_fromFlatList assetQuantities = checkCoverage $ property $
    cover 2 (length assetQuantities == length combinedAssetQuantities)
        "Every asset has exactly one quantity" $
    cover 20 (length assetQuantities > length combinedAssetQuantities)
        "Some assets have more than one quantity" $
    -- Check that multiple quantities for the same asset are combined
    -- additively:
    F.all (\(a, q) -> TokenMap.getQuantity tokenMap a == q)
        combinedAssetQuantities
  where
    tokenMap = TokenMap.fromFlatList assetQuantities
    combinedAssetQuantities =
        Map.toList $ Map.fromListWith TokenQuantity.add assetQuantities

prop_fromNestedList
    :: [(TokenPolicyId, NonEmpty (TokenName, TokenQuantity))]
    -> Property
prop_fromNestedList assetQuantities = checkCoverage $ property $
    cover 2 (length flattenedAssetQuantities == length combinedAssetQuantities)
        "Every asset has exactly one quantity" $
    cover 20 (length flattenedAssetQuantities > length combinedAssetQuantities)
        "Some assets have more than one quantity" $
    -- Check that multiple quantities for the same asset are combined
    -- additively:
    F.all (\(a, q) -> TokenMap.getQuantity tokenMap a == q)
        combinedAssetQuantities
  where
    tokenMap = TokenMap.fromNestedList assetQuantities
    combinedAssetQuantities = Map.toList $
        Map.fromListWith TokenQuantity.add flattenedAssetQuantities
    flattenedAssetQuantities =
        [ (AssetId p t, q)
        | (p, tq) <- fmap (fmap NE.toList) assetQuantities
        , (t, q) <- tq
        ]

prop_empty_toFlatList :: Property
prop_empty_toFlatList =
    TokenMap.toFlatList TokenMap.empty === []

prop_singleton_toFlatList
    :: (AssetId, TokenQuantity) -> Property
prop_singleton_toFlatList entry@(asset, quantity) = property $
    case TokenMap.toFlatList $ TokenMap.singleton asset quantity of
        [] -> quantity === TokenQuantity.zero
        [entryRetrieved] -> entryRetrieved === entry
        _ -> error "prop_singleton_toFlatList"

prop_toFlatList_fromFlatList :: TokenMap -> Property
prop_toFlatList_fromFlatList b =
    TokenMap.fromFlatList (TokenMap.toFlatList b) === b

prop_toNestedList_fromNestedList :: TokenMap -> Property
prop_toNestedList_fromNestedList b =
    TokenMap.fromNestedList (TokenMap.toNestedList b) === b

--------------------------------------------------------------------------------
-- Filtering properties
--------------------------------------------------------------------------------

-- | Verify that all assets in the resulting filtered map satisfy the predicate.
prop_filter_conjoin :: Fun AssetIdF Bool -> TokenMap -> Property
prop_filter_conjoin f b =
    let
        as = TokenMap.getAssets $ TokenMap.filter (applyFun f . AssetIdF) b
    in
        Set.foldr ((&&) . applyFun f . AssetIdF) True as === True

-- | Verify that we can partition the token map using the predicate, and recover
-- the original map by computing the union of both partitions.
prop_filter_partition :: Fun AssetIdF Bool -> TokenMap -> Property
prop_filter_partition f b =
    let
        l = TokenMap.filter (applyFun f . AssetIdF) b
        r = TokenMap.filter (not . applyFun f . AssetIdF) b
    in
        (l <> r) === b

-- | Verify that filtering twice has the same effect as filtering once.
prop_filter_twice :: Fun AssetIdF Bool -> TokenMap -> Property
prop_filter_twice f b =
    let
        once  = TokenMap.filter (applyFun f . AssetIdF) b
        twice = TokenMap.filter (applyFun f . AssetIdF) once
    in
        once === twice

--------------------------------------------------------------------------------
-- Arithmetic properties
--------------------------------------------------------------------------------

prop_add_commutative :: TokenMap -> TokenMap -> Property
prop_add_commutative b1 b2 =
    b1 `TokenMap.add` b2 === b2 `TokenMap.add` b1

prop_add_associative :: TokenMap -> TokenMap -> TokenMap -> Property
prop_add_associative b1 b2 b3 = (===)
    ((b1 `TokenMap.add` b2) `TokenMap.add` b3)
    (b1 `TokenMap.add` (b2 `TokenMap.add` b3))

prop_add_subtract_associative
    :: TokenMap -> TokenMap -> TokenMap -> Property
prop_add_subtract_associative m1 m2 m3 =
    m3 `leq` m2 ==> (===)
        ((m1 `TokenMap.add` m2) `TokenMap.subtract` m3)
        (fmap (m1 `TokenMap.add`) (m2 `TokenMap.subtract` m3))

prop_subtract_null :: TokenMap -> Property
prop_subtract_null m =
    m `TokenMap.subtract` m === Just TokenMap.empty

prop_difference_zero :: TokenMap -> Property
prop_difference_zero x =
    x `difference` mempty === x

prop_difference_zero2 :: TokenMap-> Property
prop_difference_zero2 x =
    mempty `difference` x === mempty

prop_difference_zero3 :: TokenMap -> Property
prop_difference_zero3 x =
    x `difference` x === mempty

prop_difference_leq :: TokenMap -> TokenMap -> Property
prop_difference_leq x y = property $
    x `difference` y `leq` x

-- (x - y) + y ⊇ x
prop_difference_add :: TokenMap -> TokenMap -> Property
prop_difference_add x y =
    let
        delta = x `difference` y
        yAndDelta = delta `TokenMap.add` y
    in
        counterexample ("x - y = " <> show delta) $
        counterexample ("(x - y) + y = " <> show yAndDelta) $
        property $ x `leq` yAndDelta

prop_difference_subtract :: TokenMap -> TokenMap -> Property
prop_difference_subtract x y =
    y `leq` x ==> (===)
        (x `TokenMap.subtract` y)
        (Just $ x `TokenMap.difference` y)

prop_difference_equality :: TokenMap -> TokenMap -> Property
prop_difference_equality x y = checkCoverage $
    cover 5 (TokenMap.isNotEmpty xReduced)
        "reduced maps are not empty" $
    xReduced === yReduced
  where
    xReduced = x `TokenMap.unsafeSubtract` xExcess
    yReduced = y `TokenMap.unsafeSubtract` yExcess
    xExcess = x `TokenMap.difference` y
    yExcess = y `TokenMap.difference` x

prop_intersection_associativity :: TokenMap -> TokenMap -> TokenMap -> Property
prop_intersection_associativity x y z = (===)
    ((x `TokenMap.intersection` y) `TokenMap.intersection` z)
    (x `TokenMap.intersection` (y `TokenMap.intersection` z))

prop_intersection_commutativity :: TokenMap -> TokenMap -> Property
prop_intersection_commutativity x y =
    x `TokenMap.intersection` y === y `TokenMap.intersection` x

prop_intersection_empty :: TokenMap -> Property
prop_intersection_empty x =
    x `TokenMap.intersection` TokenMap.empty === TokenMap.empty

prop_intersection_equality :: TokenMap -> TokenMap -> Property
prop_intersection_equality x y = conjoin
    [ total `TokenMap.intersection` x === x
    , total `TokenMap.intersection` y === y
    ]
  where
    total = x <> y

prop_intersection_identity :: TokenMap -> Property
prop_intersection_identity x =
    x `TokenMap.intersection` x === x

prop_intersection_subset :: TokenMap -> TokenMap -> Property
prop_intersection_subset x y = conjoin
    [ intersection `leq` x
    , intersection `leq` y
    ]
  where
    intersection = x `TokenMap.intersection` y

--------------------------------------------------------------------------------
-- Quantity properties
--------------------------------------------------------------------------------

prop_removeQuantity_isEmpty :: TokenMap -> Property
prop_removeQuantity_isEmpty b =
    F.foldl' TokenMap.removeQuantity b assets === TokenMap.empty
  where
    assets = fst <$> TokenMap.toFlatList b

prop_setQuantity_getQuantity
    :: TokenMap -> AssetId -> TokenQuantity -> Property
prop_setQuantity_getQuantity b asset quantity =
    TokenMap.getQuantity (TokenMap.setQuantity b asset quantity) asset
        === quantity

prop_setQuantity_hasQuantity
    :: TokenMap -> AssetId -> TokenQuantity -> Property
prop_setQuantity_hasQuantity b asset quantity =
    TokenMap.hasQuantity (TokenMap.setQuantity b asset quantity) asset
        === TokenQuantity.isNonZero quantity

prop_adjustQuantity_getQuantity
    :: TokenMap -> AssetId -> Property
prop_adjustQuantity_getQuantity b asset =
    TokenMap.getQuantity (TokenMap.adjustQuantity b asset adjust) asset
        === adjust quantityOriginal
  where
    quantityOriginal = TokenMap.getQuantity b asset
    adjust quantity
        | quantity > TokenQuantity.zero = TokenQuantity.pred quantity
        | otherwise = quantity

prop_adjustQuantity_hasQuantity
    :: TokenMap -> AssetId -> Property
prop_adjustQuantity_hasQuantity b asset =
    TokenMap.hasQuantity (TokenMap.adjustQuantity b asset adjust) asset
        === TokenQuantity.isNonZero (adjust quantityOriginal)
  where
    quantityOriginal = TokenMap.getQuantity b asset
    adjust quantity
        | quantity > TokenQuantity.zero = TokenQuantity.pred quantity
        | otherwise = quantity

prop_maximumQuantity_all
    :: TokenMap -> Property
prop_maximumQuantity_all b =
    property $ all (<= maxQ) (snd <$> TokenMap.toFlatList b)
  where
    maxQ = TokenMap.maximumQuantity b

--------------------------------------------------------------------------------
-- Partitioning assets
--------------------------------------------------------------------------------

prop_equipartitionAssets_coverage
    :: Blind (Large TokenMap) -> Property
prop_equipartitionAssets_coverage m = checkCoverage $
    cover 4 (assetCount == 0)
        "asset count = 0" $
    cover 4 (assetCount == 1)
        "asset count = 1" $
    cover 20 (2 <= assetCount && assetCount <= 31)
        "2 <= asset count <= 31" $
    cover 20 (32 <= assetCount && assetCount <= 63)
        "32 <= asset count <= 63"
    True
  where
    assetCount = Set.size $ TokenMap.getAssets $ getLarge $ getBlind m

prop_equipartitionAssets_length
    :: Blind (Large TokenMap) -> NonEmpty () -> Property
prop_equipartitionAssets_length (Blind (Large m)) count =
    NE.length (TokenMap.equipartitionAssets m count) === NE.length count

prop_equipartitionAssets_sizes
    :: Blind (Large TokenMap) -> NonEmpty () -> Property
prop_equipartitionAssets_sizes (Blind (Large m)) count = (.||.)
    (assetCountDifference == 0)
    (assetCountDifference == 1)
  where
    assetCounts = Set.size . TokenMap.getAssets <$> results
    assetCountMin = F.minimum assetCounts
    assetCountMax = F.maximum assetCounts
    assetCountDifference = assetCountMax - assetCountMin
    results = TokenMap.equipartitionAssets m count

prop_equipartitionAssets_sum
    :: Blind (Large TokenMap) -> NonEmpty () -> Property
prop_equipartitionAssets_sum (Blind (Large m)) count =
    F.fold (TokenMap.equipartitionAssets m count) === m

--------------------------------------------------------------------------------
-- Partitioning quantities
--------------------------------------------------------------------------------

-- Test that token map quantities are equipartitioned fairly:
--
-- Each token quantity portion must be within unity of the ideal portion.
--
prop_equipartitionQuantities_fair :: TokenMap -> NonEmpty () -> Property
prop_equipartitionQuantities_fair m count = property $
    isZeroOrOne maximumDifference
  where
    -- Here we take advantage of the fact that the resultant maps are sorted
    -- into ascending order when compared with the 'leq' function.
    --
    -- Consequently:
    --
    --  - the head map will be the smallest;
    --  - the last map will be the greatest.
    --
    -- Therefore, subtracting the head map from the last map will produce a map
    -- where each token quantity is equal to the difference between:
    --
    --  - the smallest quantity of that token in the resulting maps;
    --  - the greatest quantity of that token in the resulting maps.
    --
    differences :: TokenMap
    differences = NE.last results `TokenMap.unsafeSubtract` NE.head results

    isZeroOrOne :: TokenQuantity -> Bool
    isZeroOrOne (TokenQuantity q) = q == 0 || q == 1

    maximumDifference :: TokenQuantity
    maximumDifference = TokenMap.maximumQuantity differences

    results = TokenMap.equipartitionQuantities m count

prop_equipartitionQuantities_length :: TokenMap -> NonEmpty () -> Property
prop_equipartitionQuantities_length m count =
    NE.length (TokenMap.equipartitionQuantities m count) === NE.length count

prop_equipartitionQuantities_order :: TokenMap -> NonEmpty () -> Property
prop_equipartitionQuantities_order m count = property $
    inAscendingPartialOrder (TokenMap.equipartitionQuantities m count)

prop_equipartitionQuantities_sum :: TokenMap -> NonEmpty () -> Property
prop_equipartitionQuantities_sum m count =
    F.fold (TokenMap.equipartitionQuantities m count) === m

--------------------------------------------------------------------------------
-- Partitioning quantities according to an upper bound
--------------------------------------------------------------------------------

-- | Computes the number of parts that 'equipartitionQuantitiesWithUpperBound'
--   should return.
--
equipartitionQuantitiesWithUpperBound_expectedLength
    :: TokenMap -> TokenQuantity -> Int
equipartitionQuantitiesWithUpperBound_expectedLength
    m (TokenQuantity maxQuantity) =
        max 1 $ ceiling $ currentMaxQuantity % maxQuantity
  where
    TokenQuantity currentMaxQuantity = TokenMap.maximumQuantity m

prop_equipartitionQuantitiesWithUpperBound_coverage
    :: TokenMap -> Positive TokenQuantity -> Property
prop_equipartitionQuantitiesWithUpperBound_coverage m (Positive maxQuantity) =
    checkCoverage $
    cover 8 (maxQuantity == TokenQuantity 1)
        "Maximum allowable quantity == 1" $
    cover 8 (maxQuantity == TokenQuantity 2)
        "Maximum allowable quantity == 2" $
    cover 8 (maxQuantity >= TokenQuantity 3)
        "Maximum allowable quantity >= 3" $
    cover 8 (expectedLength == 1)
        "Expected number of parts == 1" $
    cover 8 (expectedLength == 2)
        "Expected number of parts == 2" $
    cover 8 (expectedLength >= 3)
        "Expected number of parts >= 3" $
    property $ expectedLength > 0
  where
    expectedLength = equipartitionQuantitiesWithUpperBound_expectedLength
        m maxQuantity

prop_equipartitionQuantitiesWithUpperBound_length
    :: TokenMap -> Positive TokenQuantity -> Property
prop_equipartitionQuantitiesWithUpperBound_length m (Positive maxQuantity) =
    length (TokenMap.equipartitionQuantitiesWithUpperBound m maxQuantity)
        === equipartitionQuantitiesWithUpperBound_expectedLength m maxQuantity

prop_equipartitionQuantitiesWithUpperBound_max
    :: TokenMap -> Positive TokenQuantity -> Property
prop_equipartitionQuantitiesWithUpperBound_max m (Positive maxQuantity) =
    checkCoverage $
    cover 10 (maxResultQuantity == maxQuantity)
        "At least one resultant token map has a maximal quantity" $
    property $ maxResultQuantity <= maxQuantity
  where
    results = TokenMap.equipartitionQuantitiesWithUpperBound m maxQuantity
    maxResultQuantity = F.maximum (TokenMap.maximumQuantity <$> results)

prop_equipartitionQuantitiesWithUpperBound_order
    :: TokenMap -> Positive TokenQuantity -> Property
prop_equipartitionQuantitiesWithUpperBound_order m (Positive maxQuantity) =
    property $ inAscendingPartialOrder
        (TokenMap.equipartitionQuantitiesWithUpperBound m maxQuantity)

prop_equipartitionQuantitiesWithUpperBound_sum
    :: TokenMap -> Positive TokenQuantity -> Property
prop_equipartitionQuantitiesWithUpperBound_sum m (Positive maxQuantity) =
    F.fold (TokenMap.equipartitionQuantitiesWithUpperBound m maxQuantity) === m

--------------------------------------------------------------------------------
-- JSON serialization tests
--------------------------------------------------------------------------------

failurePreamble :: String
failurePreamble = unwords
    [ "Error in $:"
    , "Error while deserializing token map from JSON:"
    ]

testZeroValuedTokenQuantityFlat :: Expectation
testZeroValuedTokenQuantityFlat =
    Aeson.parseEither (parseJSON @(Flat TokenMap)) json `shouldBe`
        Left message
  where
    policy = dummyTokenPolicyId 'A'
    token = dummyTokenName "DUMMY-TOKEN"
    json =
        [aesonQQ|
          [ { "policy_id": #{policy}
            , "asset_name": #{token}
            , "quantity": 0
            }
          ]
        |]
    message = unwords
        [ failurePreamble
        , "Encountered zero-valued quantity for token"
        , show (toText token)
        , "within policy"
        , show (toText policy) <> "."
        ]

testZeroValuedTokenQuantityNested :: Expectation
testZeroValuedTokenQuantityNested =
    Aeson.parseEither (parseJSON @(Nested TokenMap)) json `shouldBe`
        Left message
  where
    policy = dummyTokenPolicyId 'A'
    token = dummyTokenName "DUMMY-TOKEN"
    json =
        [aesonQQ|
          [ { "policy_id": #{policy}
            , "tokens": [{"asset_name": #{token}, "quantity": 0}]
            }
          ]
        |]
    message = unwords
        [ failurePreamble
        , "Encountered zero-valued quantity for token"
        , show (toText token)
        , "within policy"
        , show (toText policy) <> "."
        ]

testEmptyTokenList :: Expectation
testEmptyTokenList =
    Aeson.parseEither (parseJSON @(Nested TokenMap)) json `shouldBe`
        Left message
  where
    policy = dummyTokenPolicyId 'A'
    json = [aesonQQ|[{"policy_id": #{policy}, "tokens": []}]|]
    message = unwords
        [ failurePreamble
        , "Encountered empty token list for policy"
        , show (toText policy) <> "."
        ]

testJson
    :: (Arbitrary a, ToJSON a, FromJSON a, Typeable a) => Proxy a -> Spec
testJson = Roundtrip.jsonRoundtripAndGolden testJsonDataDirectory

testJsonDataDirectory :: FilePath
testJsonDataDirectory =
    ($(getTestData)
        </> "Cardano"
        </> "Wallet"
        </> "Primitive"
        </> "Types"
        </> "TokenMap")

--------------------------------------------------------------------------------
-- Textual serialization
--------------------------------------------------------------------------------

testPrettyFlat :: Expectation
testPrettyFlat =
    pretty (Flat testMap) `shouldBe` testMapPrettyFlat

testPrettyNested :: Expectation
testPrettyNested =
    pretty (Nested testMap) `shouldBe` testMapPrettyNested

testMap :: TokenMap
testMap = testMapData
    & fmap (second TokenQuantity)
    & fmap (first (bimap dummyTokenPolicyId dummyTokenName))
    & fmap (first (uncurry AssetId))
    & TokenMap.fromFlatList

testMapData :: [((Char, ByteString), Natural)]
testMapData =
    [ (('A', "APPLE"    ), 1)
    , (('A', "AVOCADO"  ), 2)
    , (('B', "BANANA"   ), 3)
    , (('B', "BLUEBERRY"), 4)
    ]

testMapPrettyFlat :: Text
testMapPrettyFlat = [s|
- policy: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  token: 4150504c45
  quantity: 1
- policy: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  token: 41564f4341444f
  quantity: 2
- policy: bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
  token: 42414e414e41
  quantity: 3
- policy: bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
  token: 424c55454245525259
  quantity: 4
|]

testMapPrettyNested :: Text
testMapPrettyNested = [s|
- policy: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  tokens:
    - token: 4150504c45
      quantity: 1
    - token: 41564f4341444f
      quantity: 2
- policy: bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
  tokens:
    - token: 42414e414e41
      quantity: 3
    - token: 424c55454245525259
      quantity: 4
|]

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

dummyTokenName :: ByteString -> TokenName
dummyTokenName t = fromRight reportError $ mkTokenName t
  where
    reportError = error $
        "Unable to construct dummy token name from bytes: " <> show t

-- The input must be a character in the range [0-9] or [A-Z].
--
dummyTokenPolicyId :: Char -> TokenPolicyId
dummyTokenPolicyId c
    = fromRight reportError
    $ fromText
    $ T.pack
    $ replicate tokenPolicyIdHexStringLength c
  where
    reportError = error $
        "Unable to construct dummy token policy id from character: " <> show c

tokenPolicyIdHexStringLength :: Int
tokenPolicyIdHexStringLength = 56

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

newtype Large a = Large
    { getLarge :: a }
    deriving (Eq, Show)

newtype Positive a = Positive
    { getPositive :: a }
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Flat a) where
    arbitrary = Flat <$> arbitrary
    shrink = fmap Flat . shrink . getFlat

instance Arbitrary a => Arbitrary (Nested a) where
    arbitrary = Nested <$> arbitrary
    shrink = fmap Nested . shrink . getNested

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
    shrink = mapMaybe NE.nonEmpty . shrink . NE.toList

instance Arbitrary AssetId where
    arbitrary = genAssetIdSmallRange
    shrink = shrinkAssetIdSmallRange

instance Arbitrary TokenMap where
    arbitrary = genTokenMapSmallRange
    shrink = shrinkTokenMapSmallRange

instance Arbitrary (Large TokenMap) where
    arbitrary = Large <$> do
        assetCount <- frequency
            [ (1, pure 0)
            , (1, pure 1)
            , (8, choose (2, 63))
            ]
        TokenMap.fromFlatList <$> replicateM assetCount genAssetQuantity
      where
        genAssetQuantity = (,)
            <$> genAssetIdLargeRange
            <*> genTokenQuantitySmallPositive
    -- No shrinking

instance Arbitrary TokenName where
    arbitrary = genTokenNameSmallRange
    shrink = shrinkTokenNameSmallRange

instance Arbitrary TokenPolicyId where
    arbitrary = genTokenPolicyIdSmallRange
    shrink = shrinkTokenPolicyIdSmallRange

instance Arbitrary TokenQuantity where
    -- We generate small token quantities in order to increase the chance of
    -- generating zero-valued tokens, either directly (through the generator
    -- itself), or indirectly (as the result of operations that adjust or
    -- combine existing token maps).
    --
    -- The generation of zero-valued tokens is useful, as it allows us to
    -- verify that the token map invariant (that a map contains no
    -- zero-valued tokens) is maintained.
    arbitrary = genTokenQuantitySmall
    shrink = shrinkTokenQuantitySmall

instance Arbitrary (Positive TokenQuantity) where
    arbitrary = Positive <$> genTokenQuantitySmallPositive
    shrink = fmap Positive . shrinkTokenQuantitySmallPositive . getPositive
