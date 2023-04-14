{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides property tests for the strict implementation of a non-empty map.
--
-- In particular, we test that:
--
--   - each operation preserves the invariant.
--
--   - each operation behaves in exactly the same way as the equivalent
--     operation provided by 'Data.Map.Strict'.
--
module Data.Map.Strict.NonEmptyMapSpec
    ( spec
    ) where

import Prelude

import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict.NonEmptyMap.Internal
    ( NonEmptyMap, invariantHolds )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess )
import Test.QuickCheck
    ( Arbitrary (..), Property, genericShrink, property, (===) )
import Test.QuickCheck.Classes
    ( eqLaws, foldableLaws, functorLaws, showLaws, traversableLaws )
import Test.Utils.Laws
    ( testLawsMany )

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict.NonEmptyMap.Internal as NonEmptyMap

spec :: Spec
spec = describe "Data.Map.Strict.NonEmptyMap" $

    modifyMaxSuccess (const 1000) $ do

        describe "Class instances obey laws" $ do
            testLawsMany @(NonEmptyMap Int Int)
                [ eqLaws
                , showLaws
                ]
            testLawsMany @(NonEmptyMap Int)
                [ foldableLaws
                , functorLaws
                , traversableLaws
                ]

        describe "Construction and deconstruction" $ do

            it "prop_fromList_invariant" $
                property prop_fromList_invariant
            it "prop_fromList_toList" $
                property prop_fromList_toList
            it "prop_fromMap_invariant" $
                property prop_fromMap_invariant
            it "prop_fromMap_toMap" $
                property prop_fromMap_toMap
            it "prop_singleton_invariant" $
                property prop_singleton_invariant
            it "prop_singleton_toList" $
                property prop_singleton_toList

        describe "Modification" $ do

            it "prop_insert_invariant" $
                property prop_insert_invariant
            it "prop_insert" $
                property prop_insert
            it "prop_delete_invariant" $
                property prop_delete_invariant
            it "prop_delete" $
                property prop_delete

        describe "Lookup" $ do

            it "prop_lookup" $
                property prop_lookup

        describe "Combination" $ do

            it "prop_unionWith_invariant" $
                property prop_unionWith_invariant
            it "prop_unionWith" $
                property prop_unionWith

--------------------------------------------------------------------------------
-- Construction and deconstruction properties
--------------------------------------------------------------------------------

prop_fromList_invariant :: NonEmpty (Int, Int) -> Property
prop_fromList_invariant xs =
    property $ invariantHolds $ NonEmptyMap.fromList xs

prop_fromList_toList :: NonEmpty (Int, Int) -> Property
prop_fromList_toList xs =
    expected === actual
  where
    expected = Map.toList $ Map.fromList $ NE.toList xs
    actual = NE.toList $ NonEmptyMap.toList $ NonEmptyMap.fromList xs

prop_fromMap_invariant :: [(Int, Int)] -> Property
prop_fromMap_invariant xs = property $
    case NonEmptyMap.fromMap (Map.fromList xs) of
        Nothing -> null xs
        Just m -> invariantHolds m

prop_fromMap_toMap :: NonEmpty (Int, Int) -> Property
prop_fromMap_toMap xs =
    expected === actual
  where
    expected = Map.fromList $ NE.toList xs
    actual = NonEmptyMap.toMap $ NonEmptyMap.fromList xs

prop_singleton_invariant :: (Int, Int) -> Property
prop_singleton_invariant (k, v) = property $
    invariantHolds (NonEmptyMap.singleton k v)

prop_singleton_toList :: (Int, Int) -> Property
prop_singleton_toList (k, v) = property $
    NonEmptyMap.toList (NonEmptyMap.singleton k v) === (k, v) :| []

--------------------------------------------------------------------------------
-- Modification properties
--------------------------------------------------------------------------------

prop_insert_invariant :: NonEmptyMap Int Int -> (Int, Int) -> Property
prop_insert_invariant m (k, v) = property $
    invariantHolds $ NonEmptyMap.insert k v m

prop_insert :: NonEmptyMap Int Int -> (Int, Int) -> Property
prop_insert m (k, v) =
    expected === actual
  where
    expected = Map.insert k v $ NonEmptyMap.toMap m
    actual = NonEmptyMap.toMap $ NonEmptyMap.insert k v m

prop_delete_invariant :: NonEmptyMap Int Int -> Int -> Property
prop_delete_invariant kvs k = property $
    maybe True invariantHolds (NonEmptyMap.delete k kvs)

prop_delete :: NonEmpty (Int, Int) -> Int -> Property
prop_delete kvs k =
    expected === actual
  where
    expected = Map.delete k $ Map.fromList $ NE.toList kvs
    actual
        = maybe mempty NonEmptyMap.toMap
        $ NonEmptyMap.delete k
        $ NonEmptyMap.fromList kvs

--------------------------------------------------------------------------------
-- Lookup properties
--------------------------------------------------------------------------------

prop_lookup :: NonEmpty (Int, Int) -> Int -> Property
prop_lookup kvs k =
    expected === actual
  where
    expected = Map.lookup k $ Map.fromList $ NE.toList kvs
    actual = NonEmptyMap.lookup k $ NonEmptyMap.fromList kvs

--------------------------------------------------------------------------------
-- Combination properties
--------------------------------------------------------------------------------

prop_unionWith_invariant
    :: NonEmptyMap Int Int -> NonEmptyMap Int Int -> Property
prop_unionWith_invariant m1 m2 = property $
    invariantHolds $ NonEmptyMap.unionWith (+) m1 m2

prop_unionWith
    :: NonEmpty (Int, Int) -> NonEmpty (Int, Int) -> Property
prop_unionWith kvs1 kvs2 =
    expected === actual
  where
    expected = Map.unionWith (+)
        (Map.fromList $ NE.toList kvs1)
        (Map.fromList $ NE.toList kvs2)
    actual = NonEmptyMap.toMap $ NonEmptyMap.unionWith (+)
        (NonEmptyMap.fromList kvs1)
        (NonEmptyMap.fromList kvs2)

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
    shrink = genericShrink

instance (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (NonEmptyMap k v) where
    arbitrary = NonEmptyMap.fromList <$> arbitrary
    -- Note that we must completely avoid the use of 'genericShrink' here as
    -- NonEmptyMap has an internal invariant that must be preserved to avoid
    -- undefined behaviour:
    shrink m = NonEmptyMap.fromList <$> shrink (NonEmptyMap.toList m)
