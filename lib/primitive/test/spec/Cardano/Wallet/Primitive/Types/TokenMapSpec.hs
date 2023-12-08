{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Functor law" -}

module Cardano.Wallet.Primitive.Types.TokenMapSpec
    ( spec
    ) where

import Prelude

import Cardano.Numeric.Util
    ( inAscendingPartialOrder
    )
import Cardano.Wallet.Primitive.Types.AssetId
    ( AssetId (..)
    )
import Cardano.Wallet.Primitive.Types.AssetId.Gen
    ( genAssetId
    , genAssetIdLargeRange
    , shrinkAssetId
    )
import Cardano.Wallet.Primitive.Types.AssetName
    ( AssetName
    )
import Cardano.Wallet.Primitive.Types.AssetName.Gen
    ( genAssetName
    , shrinkAssetName
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( Flat (..)
    , Lexicographic (..)
    , Nested (..)
    , TokenMap
    )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genTokenMapPartition
    , genTokenMapSmallRange
    , shrinkTokenMap
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId
    ( TokenPolicyId
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId.Gen
    ( genTokenPolicyId
    , shrinkTokenPolicyId
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..)
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity.Gen
    ( genTokenQuantity
    , genTokenQuantityPositive
    , shrinkTokenQuantity
    , shrinkTokenQuantityPositive
    )
import Control.Monad
    ( replicateM
    )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    )
import Data.Aeson.QQ
    ( aesonQQ
    )
import Data.Bifunctor
    ( bimap
    , first
    , second
    )
import Data.ByteString
    ( ByteString
    )
import Data.Either
    ( fromRight
    )
import Data.Function
    ( (&)
    )
import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import Data.Maybe
    ( mapMaybe
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Ratio
    ( (%)
    )
import Data.String.QQ
    ( s
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( fromText
    , toText
    )
import Data.Typeable
    ( Typeable
    )
import Fmt
    ( pretty
    )
import Numeric.Natural
    ( Natural
    )
import System.FilePath
    ( (</>)
    )
import Test.Hspec
    ( Expectation
    , Spec
    , describe
    , it
    , shouldBe
    )
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Blind (..)
    , CoArbitrary (..)
    , Fun
    , Function (..)
    , Property
    , Testable
    , applyFun
    , checkCoverage
    , choose
    , cover
    , forAll
    , frequency
    , property
    , (.||.)
    , (===)
    )
import Test.QuickCheck.Classes
    ( eqLaws
    , monoidLaws
    , ordLaws
    , semigroupLaws
    , semigroupMonoidLaws
    )
import Test.QuickCheck.Classes.Monoid.GCD
    ( gcdMonoidLaws
    , leftGCDMonoidLaws
    , overlappingGCDMonoidLaws
    , rightGCDMonoidLaws
    )
import Test.QuickCheck.Classes.Monoid.Monus
    ( monusLaws
    )
import Test.QuickCheck.Classes.Monoid.Null
    ( monoidNullLaws
    )
import Test.QuickCheck.Classes.Semigroup.Cancellative
    ( commutativeLaws
    , leftReductiveLaws
    , reductiveLaws
    , rightReductiveLaws
    )
import Test.QuickCheck.Instances.ByteString
    ()
import Test.Utils.Laws
    ( testLawsMany
    )
import Test.Utils.Laws.PartialOrd
    ( partialOrdLaws
    )
import Test.Utils.Paths
    ( getTestData
    )

import qualified Cardano.Wallet.Primitive.Types.AssetName as AssetName
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as TokenQuantity
import qualified Data.Aeson.Types as Aeson
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Test.QuickCheck as QC
import qualified Test.Utils.Roundtrip as Roundtrip

spec :: Spec
spec =
    describe "Token map properties" $
    modifyMaxSuccess (const 1000) $ do

    describe "Class instances obey laws" $ do
        testLawsMany @TokenMap
            [ commutativeLaws
            , eqLaws
            , gcdMonoidLaws
            , leftGCDMonoidLaws
            , leftReductiveLaws
            , monoidLaws
            , monoidNullLaws
            , monusLaws
            , overlappingGCDMonoidLaws
            , partialOrdLaws
            , reductiveLaws
            , rightGCDMonoidLaws
            , rightReductiveLaws
            , semigroupLaws
            , semigroupMonoidLaws
            ]
        testLawsMany @(Lexicographic TokenMap)
            [ eqLaws
            , ordLaws
            ]

    describe "Construction and deconstruction" $ do

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

    describe "Filtering" $ do

        it "prop_filter_conjoin" $
            property prop_filter_conjoin
        it "prop_filter_partition" $
            property prop_filter_partition
        it "prop_filter_twice" $
            property prop_filter_twice

    describe "Quantities" $ do

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
        it "prop_maximumQuantity_mempty" $
            property prop_maximumQuantity_mempty

    describe "Queries" $ do

        it "prop_size_isEmpty" $ do
            property prop_size_isEmpty
        it "prop_size_toFlatList" $ do
            property prop_size_toFlatList

    describe "Transformations" $ do

        it "prop_mapAssetIds_identity" $ do
            prop_mapAssetIds_identity & property
        it "prop_mapAssetIds_composition" $ do
            prop_mapAssetIds_composition & property

    describe "Partitioning assets" $ do

        it "prop_equipartitionAssets_coverage" $
            property prop_equipartitionAssets_coverage
        it "prop_equipartitionAssets_length" $
            property prop_equipartitionAssets_length
        it "prop_equipartitionAssets_sizes" $
            property prop_equipartitionAssets_sizes
        it "prop_equipartitionAssets_sum" $
            property prop_equipartitionAssets_sum

    describe "Partitioning quantities" $ do

        it "prop_equipartitionQuantities_fair" $
            property prop_equipartitionQuantities_fair
        it "prop_equipartitionQuantities_length" $
            property prop_equipartitionQuantities_length
        it "prop_equipartitionQuantities_order" $
            property prop_equipartitionQuantities_order
        it "prop_equipartitionQuantities_sum" $
            property prop_equipartitionQuantities_sum

    describe "Partitioning quantities with an upper bound" $ do

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

    describe "Generating partitions" $ do

        it "prop_genTokenMapPartition_fold" $
            prop_genTokenMapPartition_fold & property
        it "prop_genTokenMapPartition_length" $
            prop_genTokenMapPartition_length & property
        it "prop_genTokenMapPartition_nonPositive" $
            prop_genTokenMapPartition_nonPositive & property

    describe "JSON serialization" $ do

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

    describe "Textual serialization" $ do
        it "Flat style" $
            property testPrettyFlat
        it "Nested style" $
            property testPrettyNested

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
    :: [(TokenPolicyId, NonEmpty (AssetName, TokenQuantity))]
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
prop_filter_conjoin :: Fun AssetId Bool -> TokenMap -> Property
prop_filter_conjoin f b =
    let
        as = TokenMap.getAssets $ TokenMap.filter (applyFun f) b
    in
        Set.foldr ((&&) . applyFun f) True as === True

-- | Verify that we can partition the token map using the predicate, and recover
-- the original map by computing the union of both partitions.
prop_filter_partition :: Fun AssetId Bool -> TokenMap -> Property
prop_filter_partition f b =
    let
        l = TokenMap.filter (applyFun f) b
        r = TokenMap.filter (not . applyFun f) b
    in
        (l <> r) === b

-- | Verify that filtering twice has the same effect as filtering once.
prop_filter_twice :: Fun AssetId Bool -> TokenMap -> Property
prop_filter_twice f b =
    let
        once  = TokenMap.filter (applyFun f) b
        twice = TokenMap.filter (applyFun f) once
    in
        once === twice

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
    adjust = TokenQuantity.predZero

prop_adjustQuantity_hasQuantity
    :: TokenMap -> AssetId -> Property
prop_adjustQuantity_hasQuantity b asset =
    TokenMap.hasQuantity (TokenMap.adjustQuantity b asset adjust) asset
        === TokenQuantity.isNonZero (adjust quantityOriginal)
  where
    quantityOriginal = TokenMap.getQuantity b asset
    adjust = TokenQuantity.predZero

prop_maximumQuantity_all
    :: TokenMap -> Property
prop_maximumQuantity_all b =
    property $ all ((<= maxQ) . snd) (TokenMap.toFlatList b)
  where
    maxQ = TokenMap.maximumQuantity b

prop_maximumQuantity_mempty :: Property
prop_maximumQuantity_mempty = TokenMap.maximumQuantity mempty === mempty

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

prop_size_isEmpty :: TokenMap -> Property
prop_size_isEmpty m =
    checkCoverage_size m $
    if TokenMap.isEmpty m
    then TokenMap.size m == 0
    else TokenMap.size m > 0

prop_size_toFlatList :: TokenMap -> Property
prop_size_toFlatList m =
    checkCoverage_size m $
    TokenMap.size m === length (TokenMap.toFlatList m)

checkCoverage_size :: Testable prop => TokenMap -> (prop -> Property)
checkCoverage_size m
    = checkCoverage
    . cover 2 (TokenMap.size m == 0) "size == 0"
    . cover 2 (TokenMap.size m == 1) "size == 1"
    . cover 2 (TokenMap.size m == 2) "size == 2"
    . cover 2 (TokenMap.size m >= 3) "size >= 3"

--------------------------------------------------------------------------------
-- Transformations
--------------------------------------------------------------------------------

prop_mapAssetIds_identity :: TokenMap -> Property
prop_mapAssetIds_identity m =
    TokenMap.mapAssetIds id m === m

prop_mapAssetIds_composition
    :: TokenMap -> Fun AssetId AssetId -> Fun AssetId AssetId -> Property
prop_mapAssetIds_composition m (applyFun -> f) (applyFun -> g) =
    TokenMap.mapAssetIds f (TokenMap.mapAssetIds g m) ===
    TokenMap.mapAssetIds (f . g) m

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
    assetCount = TokenMap.size $ getLarge $ getBlind m

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
    assetCounts = TokenMap.size <$> results
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
    differences = NE.last results `TokenMap.difference` NE.head results

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
    cover 4 (maxQuantity == TokenQuantity 1)
        "Maximum allowable quantity == 1" $
    cover 4 (maxQuantity == TokenQuantity 2)
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
-- Generating partitions
--------------------------------------------------------------------------------

prop_genTokenMapPartition_fold
    :: TokenMap -> QC.Positive (QC.Small Int) -> Property
prop_genTokenMapPartition_fold m (QC.Positive (QC.Small i)) =
    forAll (genTokenMapPartition m i) $ (=== m) . F.fold

prop_genTokenMapPartition_length
    :: TokenMap -> QC.Positive (QC.Small Int) -> Property
prop_genTokenMapPartition_length m (QC.Positive (QC.Small i)) =
    forAll (genTokenMapPartition m i) $ (=== i) . F.length

prop_genTokenMapPartition_nonPositive
    :: TokenMap -> QC.NonPositive (QC.Small Int) -> Property
prop_genTokenMapPartition_nonPositive m (QC.NonPositive (QC.Small i)) =
    forAll (genTokenMapPartition m i) (=== pure m)

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
    token = dummyAssetName "DUMMY-ASSET"
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
    token = dummyAssetName "DUMMY-ASSET"
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
    & fmap (first (bimap dummyTokenPolicyId dummyAssetName))
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

dummyAssetName :: ByteString -> AssetName
dummyAssetName t = fromRight reportError $ AssetName.fromByteString t
  where
    reportError = error $
        "Unable to construct dummy asset name from bytes: " <> show t

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

deriving newtype instance Arbitrary (Lexicographic TokenMap)

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
    arbitrary = genAssetId
    shrink = shrinkAssetId

deriving anyclass instance CoArbitrary AssetId
deriving anyclass instance Function AssetId

instance Arbitrary TokenMap where
    arbitrary = genTokenMapSmallRange
    shrink = shrinkTokenMap

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
            <*> genTokenQuantityPositive
    -- No shrinking

instance Arbitrary AssetName where
    arbitrary = genAssetName
    shrink = shrinkAssetName

deriving anyclass instance CoArbitrary AssetName
deriving anyclass instance Function AssetName

instance Arbitrary TokenPolicyId where
    arbitrary = genTokenPolicyId
    shrink = shrinkTokenPolicyId

deriving anyclass instance CoArbitrary TokenPolicyId
deriving anyclass instance Function TokenPolicyId

deriving anyclass instance CoArbitrary (Hash "TokenPolicy")
deriving anyclass instance Function (Hash "TokenPolicy")

instance Arbitrary TokenQuantity where
    -- We generate small token quantities in order to increase the chance of
    -- generating zero-valued tokens, either directly (through the generator
    -- itself), or indirectly (as the result of operations that adjust or
    -- combine existing token maps).
    --
    -- The generation of zero-valued tokens is useful, as it allows us to
    -- verify that the token map invariant (that a map contains no
    -- zero-valued tokens) is maintained.
    arbitrary = genTokenQuantity
    shrink = shrinkTokenQuantity

instance Arbitrary (Positive TokenQuantity) where
    arbitrary = Positive <$> genTokenQuantityPositive
    shrink = fmap Positive . shrinkTokenQuantityPositive . getPositive
