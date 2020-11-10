{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.Types.TokenBundleSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.TokenBundle
    ( AssetId (..), Flat (..), Nested (..), TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName, TokenPolicyId )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Data.Aeson
    ( FromJSON (..), ToJSON (..) )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.Bifunctor
    ( bimap, first, second )
import Data.Function
    ( (&) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( catMaybes )
import Data.Proxy
    ( Proxy (..) )
import Data.String.QQ
    ( s )
import Data.Text
    ( Text )
import Data.Text.Class
    ( toText )
import Data.Typeable
    ( Typeable )
import Fmt
    ( pretty )
import System.FilePath
    ( (</>) )
import Test.Hspec
    ( Expectation, Spec, describe, it, shouldBe )
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , Small (..)
    , checkCoverage
    , cover
    , elements
    , property
    , (===)
    )
import Test.QuickCheck.Classes
    ( eqLaws, monoidLaws, semigroupLaws, semigroupMonoidLaws )
import Test.Utils.Laws
    ( testLawsMany )
import Test.Utils.Paths
    ( getTestData )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TB
import qualified Cardano.Wallet.Primitive.Types.TokenPolicy as TP
import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as TQ
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Test.Utils.Roundtrip as Roundtrip

spec :: Spec
spec =
    describe "Token bundle properties" $
    modifyMaxSuccess (const 1000) $ do

    describe "Class instances obey laws" $ do
        testLawsMany @TokenBundle
            [ eqLaws
            , monoidLaws
            , semigroupLaws
            , semigroupMonoidLaws
            ]

    describe
        "All operations preserve the invariant: \
        \all token quantities held within a bundle are non-zero" $ do

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
        it "prop_setQuantity_invariant" $
            property prop_setQuantity_invariant
        it "prop_adjustQuantity_invariant" $
            property prop_adjustQuantity_invariant

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

    describe "Arithmetic" $ do

        it "prop_add_commutative" $
            property prop_add_commutative
        it "prop_add_associative" $
            property prop_add_associative
        it "prop_add_subtract_associative" $
            property prop_add_subtract_associative
        it "prop_subtract_null" $
            property prop_subtract_null

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

    describe "JSON serialization" $ do

        describe "Roundtrip tests" $ do
            testJson $ Proxy @(Flat TokenBundle)
            testJson $ Proxy @(Nested TokenBundle)

        describe "Negative tests" $ do
            it "Zero-valued token quantity (from flat representation)" $
                testZeroValuedTokenQuantityFlat
            it "Zero-valued token quantity (from nested representation)" $
                testZeroValuedTokenQuantityNested
            it "Empty token list" $
                testEmptyTokenList

    describe "Textual serialization" $ do
        it "Flat style" $
            property testPrettyFlat
        it "Nested style" $
            property testPrettyNested

--------------------------------------------------------------------------------
-- Invariant properties
--------------------------------------------------------------------------------

-- Tests that all quantities within the given bundle are non-zero.
--
invariantHolds :: TokenBundle -> Bool
invariantHolds b = all TQ.isNonZero $ getQuantity <$> TB.toFlatList b
  where
    getQuantity (_, q) = q

prop_arbitrary_invariant :: TokenBundle -> Property
prop_arbitrary_invariant = property . invariantHolds

prop_shrink_invariant :: TokenBundle -> Property
prop_shrink_invariant b = property $ all invariantHolds $ shrink b

prop_empty_invariant :: Property
prop_empty_invariant = property $ invariantHolds $ TB.empty

prop_singleton_invariant :: (AssetId, TokenQuantity) -> Property
prop_singleton_invariant (asset, quantity) = property $
    invariantHolds $ TB.singleton asset quantity

prop_fromFlatList_invariant :: [(AssetId, TokenQuantity)] -> Property
prop_fromFlatList_invariant entries =
    property $ invariantHolds $ TB.fromFlatList entries

prop_fromNestedList_invariant
    :: [(TokenPolicyId, NonEmpty (TokenName, TokenQuantity))] -> Property
prop_fromNestedList_invariant entries =
    property $ invariantHolds $ TB.fromNestedList entries

prop_add_invariant :: TokenBundle -> TokenBundle -> Property
prop_add_invariant b1 b2 = property $ invariantHolds $ TB.add b1 b2

prop_subtract_invariant :: TokenBundle -> TokenBundle -> Property
prop_subtract_invariant b1 b2 = property $ invariantHolds $ TB.subtract b1 b2

prop_setQuantity_invariant
    :: TokenBundle -> AssetId -> TokenQuantity -> Property
prop_setQuantity_invariant b asset quantity = property $
    invariantHolds $ TB.setQuantity b asset quantity

prop_adjustQuantity_invariant :: TokenBundle -> AssetId -> Property
prop_adjustQuantity_invariant b asset = property $
    invariantHolds $ TB.adjustQuantity b asset adjust
  where
    adjust quantity
        | TQ.isStrictlyNegative quantity = TQ.succ quantity
        | TQ.isStrictlyPositive quantity = TQ.pred quantity
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
    F.all (\(a, q) -> TB.getQuantity bundle a == q) combinedAssetQuantities
  where
    bundle = TB.fromFlatList assetQuantities
    combinedAssetQuantities =
        Map.toList $ Map.fromListWith TQ.add assetQuantities

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
    F.all (\(a, q) -> TB.getQuantity bundle a == q) combinedAssetQuantities
  where
    bundle = TB.fromNestedList assetQuantities
    combinedAssetQuantities =
        Map.toList $ Map.fromListWith TQ.add flattenedAssetQuantities
    flattenedAssetQuantities =
        [ (AssetId p t, q)
        | (p, tq) <- fmap (fmap NE.toList) assetQuantities
        , (t, q) <- tq
        ]

prop_empty_toFlatList :: Property
prop_empty_toFlatList =
    TB.toFlatList TB.empty === []

prop_singleton_toFlatList
    :: (AssetId, TokenQuantity) -> Property
prop_singleton_toFlatList entry@(asset, quantity) = property $
    case TB.toFlatList $ TB.singleton asset quantity of
        [] -> quantity === TQ.zero
        [entryRetrieved] -> entryRetrieved === entry
        _ -> error "prop_singleton_toFlatList"

prop_toFlatList_fromFlatList :: TokenBundle -> Property
prop_toFlatList_fromFlatList b =
    TB.fromFlatList (TB.toFlatList b) === b

prop_toNestedList_fromNestedList :: TokenBundle -> Property
prop_toNestedList_fromNestedList b =
    TB.fromNestedList (TB.toNestedList b) === b

--------------------------------------------------------------------------------
-- Arithmetic properties
--------------------------------------------------------------------------------

prop_add_commutative :: TokenBundle -> TokenBundle -> Property
prop_add_commutative b1 b2 =
    b1 `TB.add` b2 === b2 `TB.add` b1

prop_add_associative :: TokenBundle -> TokenBundle -> TokenBundle -> Property
prop_add_associative b1 b2 b3 = (===)
    ((b1 `TB.add` b2) `TB.add` b3)
    (b1 `TB.add` (b2 `TB.add` b3))

prop_add_subtract_associative
    :: TokenBundle -> TokenBundle -> TokenBundle -> Property
prop_add_subtract_associative b1 b2 b3 = (===)
    ((b1 `TB.add` b2) `TB.subtract` b3)
    (b1 `TB.add` (b2 `TB.subtract` b3))

prop_subtract_null :: TokenBundle -> Property
prop_subtract_null b =
    b `TB.subtract` b === TB.empty

--------------------------------------------------------------------------------
-- Quantity properties
--------------------------------------------------------------------------------

prop_removeQuantity_isEmpty :: TokenBundle -> Property
prop_removeQuantity_isEmpty b =
    F.foldl' TB.removeQuantity b assets === TB.empty
  where
    assets = fst <$> TB.toFlatList b

prop_setQuantity_getQuantity
    :: TokenBundle -> AssetId -> TokenQuantity -> Property
prop_setQuantity_getQuantity b asset quantity =
    TB.getQuantity (TB.setQuantity b asset quantity) asset
        === quantity

prop_setQuantity_hasQuantity
    :: TokenBundle -> AssetId -> TokenQuantity -> Property
prop_setQuantity_hasQuantity b asset quantity =
    TB.hasQuantity (TB.setQuantity b asset quantity) asset
        === TQ.isNonZero quantity

prop_adjustQuantity_getQuantity
    :: TokenBundle -> AssetId -> Property
prop_adjustQuantity_getQuantity b asset =
    TB.getQuantity (TB.adjustQuantity b asset adjust) asset
        === adjust quantityOriginal
  where
    quantityOriginal = TB.getQuantity b asset
    adjust quantity
        | TQ.isStrictlyNegative quantity = TQ.succ quantity
        | TQ.isStrictlyPositive quantity = TQ.pred quantity
        | otherwise = quantity

prop_adjustQuantity_hasQuantity
    :: TokenBundle -> AssetId -> Property
prop_adjustQuantity_hasQuantity b asset =
    TB.hasQuantity (TB.adjustQuantity b asset adjust) asset
        === TQ.isNonZero (adjust quantityOriginal)
  where
    quantityOriginal = TB.getQuantity b asset
    adjust quantity
        | TQ.isStrictlyNegative quantity = TQ.succ quantity
        | TQ.isStrictlyPositive quantity = TQ.pred quantity
        | otherwise = quantity

--------------------------------------------------------------------------------
-- JSON serialization tests
--------------------------------------------------------------------------------

failurePreamble :: String
failurePreamble = unwords
    [ "Error in $:"
    , "Error while deserializing token bundle from JSON:"
    ]

testZeroValuedTokenQuantityFlat :: Expectation
testZeroValuedTokenQuantityFlat =
    Aeson.parseEither (parseJSON @(Flat TokenBundle)) json `shouldBe`
        Left message
  where
    policy = dummyTokenPolicyId 'A'
    token = TP.mkTokenName "DUMMY-TOKEN"
    json =
        [aesonQQ|
          [ { "asset": {"policy": #{policy}, "token": #{token}}
            , "quantity": 0 }]
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
    Aeson.parseEither (parseJSON @(Nested TokenBundle)) json `shouldBe`
        Left message
  where
    policy = dummyTokenPolicyId 'A'
    token = TP.mkTokenName "DUMMY-TOKEN"
    json =
        [aesonQQ|
          [ { "policy": #{policy}
            , "tokens": [{"name": #{token}, "quantity": 0}]}]
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
    Aeson.parseEither (parseJSON @(Nested TokenBundle)) json `shouldBe`
        Left message
  where
    policy = dummyTokenPolicyId 'A'
    json = [aesonQQ|[{"policy": #{policy}, "tokens": []}]|]
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
    ($(getTestData) </> "Cardano" </> "Wallet" </> "Primitive" </> "Types")

--------------------------------------------------------------------------------
-- Textual serialization
--------------------------------------------------------------------------------

testPrettyFlat :: Expectation
testPrettyFlat =
    pretty (Flat testBundle) `shouldBe` testBundlePrettyFlat

testPrettyNested :: Expectation
testPrettyNested =
    pretty (Nested testBundle) `shouldBe` testBundlePrettyNested

testBundle :: TokenBundle
testBundle = testBundleData
    & fmap (second TokenQuantity)
    & fmap (first (bimap dummyTokenPolicyId TP.mkTokenName))
    & fmap (first (uncurry AssetId))
    & TB.fromFlatList

testBundleData :: [((Char, Text), Integer)]
testBundleData =
    [ (('A', "APPLE"    ), -2)
    , (('A', "AVOCADO"  ), -1)
    , (('B', "BANANA"   ),  1)
    , (('B', "BLUEBERRY"),  2)
    ]

testBundlePrettyFlat :: Text
testBundlePrettyFlat = [s|
token-bundle:
  - asset:
      policy: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      token: APPLE
    quantity: -2
  - asset:
      policy: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      token: AVOCADO
    quantity: -1
  - asset:
      policy: bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
      token: BANANA
    quantity: 1
  - asset:
      policy: bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
      token: BLUEBERRY
    quantity: 2
|]

testBundlePrettyNested :: Text
testBundlePrettyNested = [s|
token-bundle:
  - policy: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    tokens:
      - name: APPLE
        quantity: -2
      - name: AVOCADO
        quantity: -1
  - policy: bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
    tokens:
      - name: BANANA
        quantity: 1
      - name: BLUEBERRY
        quantity: 2
|]

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

dummyTokenPolicyId :: Char -> TokenPolicyId
dummyTokenPolicyId
    = TP.mkTokenPolicyId
    . unsafeFromHex
    . B8.replicate tokenPolicyIdHexStringLength

tokenPolicyIdHexStringLength :: Int
tokenPolicyIdHexStringLength = 56

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Flat a) where
    arbitrary = Flat <$> arbitrary
    shrink = fmap Flat . shrink . getFlat

instance Arbitrary a => Arbitrary (Nested a) where
    arbitrary = Nested <$> arbitrary
    shrink = fmap Nested . shrink . getNested

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
    shrink = catMaybes . fmap NE.nonEmpty . shrink . NE.toList

instance Arbitrary AssetId where
    arbitrary = AssetId <$> arbitrary <*> arbitrary

instance Arbitrary TokenBundle where
    arbitrary = TB.fromFlatList <$> arbitrary
    shrink b = TB.fromFlatList <$> shrink (TB.toFlatList b)

instance Arbitrary TokenName where
    -- We generate token names from a small range in order to increase the
    -- chance of collisions, which are useful.
    arbitrary = mkTokenName <$> elements ['A' .. 'D']
      where
        mkTokenName = TP.mkTokenName . ("Token" `T.snoc`)

instance Arbitrary TokenPolicyId where
    -- We generate token policy identifiers from a small range in order to
    -- increase the chance of collisions, which are useful.
    arbitrary = dummyTokenPolicyId <$> elements ['A' .. 'D']

instance Arbitrary TokenQuantity where

    -- We generate small token quantities in order to increase the chance of
    -- generating zero-valued tokens, either directly (through the generator
    -- itself), or indirectly (as the result of operations that adjust or
    -- combine existing token bundles).
    --
    -- The generation of zero-valued tokens is useful, as it allows us to
    -- verify that the token bundle invariant (that a bundle contains no
    -- zero-valued tokens) is maintained.

    arbitrary = TokenQuantity . getSmall <$> arbitrary
    shrink (TokenQuantity q) = TokenQuantity <$> shrink q
