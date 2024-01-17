{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Use camelCase" -}

module Cardano.CoinSelection.UTxOSelectionSpec
    ( spec
    ) where

import Prelude

import Cardano.CoinSelection.UTxOIndex
    ( UTxOIndex
    )
import Cardano.CoinSelection.UTxOIndex.Gen
    ( genUTxOIndex
    , shrinkUTxOIndex
    )
import Cardano.CoinSelection.UTxOSelection
    ( IsUTxOSelection
    , UTxOSelection
    , UTxOSelectionNonEmpty
    )
import Cardano.CoinSelection.UTxOSelection.Gen
    ( genUTxOSelection
    , genUTxOSelectionNonEmpty
    , shrinkUTxOSelection
    , shrinkUTxOSelectionNonEmpty
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , CoArbitrary (..)
    , Property
    , Testable
    , checkCoverage
    , conjoin
    , cover
    , forAll
    , property
    , (===)
    )
import Test.QuickCheck.Quid
    ( Hexadecimal (..)
    , Quid
    , Size (..)
    )

import qualified Cardano.CoinSelection.UTxOIndex as UTxOIndex
import qualified Cardano.CoinSelection.UTxOSelection as UTxOSelection
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map

spec :: Spec
spec =
    describe "Cardano.CoinSelection.UTxOSelectionSpec" $ do

    describe "Generators and shrinkers" $ do

        it "prop_genUTxOSelection" $
            property prop_genUTxOSelection
        it "prop_genUTxOSelectionNonEmpty" $
            property prop_genUTxOSelectionNonEmpty
        it "prop_shrinkUTxOSelection" $
            property prop_shrinkUTxOSelection
        it "prop_shrinkUTxOSelectionNonEmpty" $
            property prop_shrinkUTxOSelectionNonEmpty

    describe "Construction and deconstruction" $ do

        it "prop_fromIndex_isValid" $
            property prop_fromIndex_isValid
        it "prop_fromIndexFiltered_isValid" $
            property prop_fromIndexFiltered_isValid
        it "prop_fromIndexPair_isValid" $
            property prop_fromIndexPair_isValid
        it "prop_fromIndex_toIndexPair" $
            property prop_fromIndex_toIndexPair
        it "prop_fromIndexFiltered_toIndexPair" $
            property prop_fromIndexFiltered_toIndexPair
        it "prop_fromIndexPair_toIndexPair" $
            property prop_fromIndexPair_toIndexPair

    describe "Promotion and demotion" $ do

        it "prop_fromNonEmpty_toNonEmpty" $
            property prop_fromNonEmpty_toNonEmpty
        it "prop_toNonEmpty_fromNonEmpty" $
            property prop_toNonEmpty_fromNonEmpty

    describe "Indicator and accessor functions" $ do

        it "prop_availableBalance_availableMap" $
            property prop_availableBalance_availableMap
        it "prop_availableMap_availableSize" $
            property prop_availableMap_availableSize
        it "prop_isNonEmpty_selectedSize" $
            property prop_isNonEmpty_selectedSize
        it "prop_isNonEmpty_selectedIndex" $
            property prop_isNonEmpty_selectedIndex
        it "prop_isNonEmpty_selectedList" $
            property prop_isNonEmpty_selectedList
        it "prop_leftoverBalance_selectedBalance" $
            property prop_leftoverBalance_selectedBalance
        it "prop_leftoverSize_selectedSize" $
            property prop_leftoverSize_selectedSize

    describe "Modification" $ do

        it "prop_select_empty" $
            property prop_select_empty
        it "prop_select_isValid" $
            property prop_select_isValid
        it "prop_select_isLeftover" $
            property prop_select_isLeftover
        it "prop_select_isSelected" $
            property prop_select_isSelected
        it "prop_select_isProperSubSelectionOf" $
            property prop_select_isProperSubSelectionOf
        it "prop_select_availableBalance" $
            property prop_select_availableBalance
        it "prop_select_availableMap" $
            property prop_select_availableMap
        it "prop_select_leftoverSize" $
            property prop_select_leftoverSize
        it "prop_select_selectedSize" $
            property prop_select_selectedSize
        it "prop_selectMany_isSubSelectionOf" $
            property prop_selectMany_isSubSelectionOf
        it "prop_selectMany_leftoverSize_all" $
            property prop_selectMany_leftoverSize_all
        it "prop_selectMany_selectedSize_all" $
            property prop_selectMany_selectedSize_all

--------------------------------------------------------------------------------
-- Generators and shrinkers
--------------------------------------------------------------------------------

prop_genUTxOSelection :: Property
prop_genUTxOSelection =
    forAll (genUTxOSelection (arbitrary @TestUTxO)) $ \s ->
    checkCoverage_UTxOSelection s $
    isValidSelection s === True

prop_genUTxOSelectionNonEmpty :: Property
prop_genUTxOSelectionNonEmpty =
    forAll (genUTxOSelectionNonEmpty (arbitrary @TestUTxO)) $ \s ->
    checkCoverage_UTxOSelectionNonEmpty s $
    isValidSelectionNonEmpty s === True

prop_shrinkUTxOSelection :: Property
prop_shrinkUTxOSelection =
    forAll (genUTxOSelection (arbitrary @(Size 4 TestUTxO))) $ \s ->
    conjoin (isValidSelection <$> shrinkUTxOSelection shrink s)

prop_shrinkUTxOSelectionNonEmpty :: Property
prop_shrinkUTxOSelectionNonEmpty =
    forAll (genUTxOSelectionNonEmpty (arbitrary @(Size 4 TestUTxO))) $ \s ->
    conjoin $ isValidSelectionNonEmpty
        <$> shrinkUTxOSelectionNonEmpty shrink s

checkCoverage_UTxOSelection
    :: Testable p => IsUTxOSelection s u => s u -> (p -> Property)
checkCoverage_UTxOSelection s
    = checkCoverage_UTxOSelectionNonEmpty s
    . cover 2 (0 == ssize && ssize == lsize) "0 == lsize && lsize == ssize"
    . cover 2 (0 == ssize && ssize <  lsize) "0 == ssize && ssize <  lsize"
  where
    lsize = UTxOSelection.leftoverSize s
    ssize = UTxOSelection.selectedSize s

checkCoverage_UTxOSelectionNonEmpty
    :: Testable p => IsUTxOSelection s u => s u -> (p -> Property)
checkCoverage_UTxOSelectionNonEmpty s
    = checkCoverage
    . cover 2 (0 == lsize && lsize <  ssize) "0 == lsize && lsize <  ssize"
    . cover 2 (0 <  lsize && lsize == ssize) "0 <  lsize && lsize == ssize"
    . cover 2 (0 <  lsize && lsize <  ssize) "0 <  lsize && lsize <  ssize"
    . cover 2 (0 <  ssize && ssize == lsize) "0 <  ssize && ssize == lsize"
    . cover 2 (0 <  ssize && ssize <  lsize) "0 <  ssize && ssize <  lsize"
  where
    lsize = UTxOSelection.leftoverSize s
    ssize = UTxOSelection.selectedSize s

--------------------------------------------------------------------------------
-- Construction and deconstruction
--------------------------------------------------------------------------------

checkCoverage_filter
    :: (Arbitrary u, Show u, Testable prop) => (u -> Bool) -> prop -> Property
checkCoverage_filter f = property . flip checkCoverage_inner
  where
    checkCoverage_inner u
        = checkCoverage
        . cover 40 (f u)
            "filter matches"
        . cover 40 (not (f u))
            "filter does not match"

prop_fromIndex_isValid :: UTxOIndex TestUTxO -> Property
prop_fromIndex_isValid i =
    isValidSelection (UTxOSelection.fromIndex i)
    === True

prop_fromIndexFiltered_isValid
    :: (TestUTxO -> Bool) -> UTxOIndex TestUTxO -> Property
prop_fromIndexFiltered_isValid f i =
    checkCoverage_filter f $
    isValidSelection (UTxOSelection.fromIndexFiltered f i)
    === True

prop_fromIndexPair_isValid
    :: (UTxOIndex TestUTxO, UTxOIndex TestUTxO) -> Property
prop_fromIndexPair_isValid (i1, i2) =
    isValidSelection (UTxOSelection.fromIndexPair (i1, i2))
    === True

prop_fromIndex_toIndexPair :: UTxOIndex TestUTxO -> Property
prop_fromIndex_toIndexPair i =
    UTxOSelection.toIndexPair (UTxOSelection.fromIndex i)
    === (i, UTxOIndex.empty)

prop_fromIndexFiltered_toIndexPair
    :: (TestUTxO -> Bool)
    -> UTxOIndex TestUTxO
    -> Property
prop_fromIndexFiltered_toIndexPair f i =
    checkCoverage_filter f $
    UTxOSelection.toIndexPair (UTxOSelection.fromIndexFiltered f i)
    === (UTxOIndex.filter (not . f) i, UTxOIndex.filter f i)

prop_fromIndexPair_toIndexPair :: UTxOSelection TestUTxO -> Property
prop_fromIndexPair_toIndexPair s =
    checkCoverage_UTxOSelection s $
    UTxOSelection.fromIndexPair (UTxOSelection.toIndexPair s)
    === s

--------------------------------------------------------------------------------
-- Promotion and demotion
--------------------------------------------------------------------------------

prop_fromNonEmpty_toNonEmpty :: UTxOSelectionNonEmpty TestUTxO -> Property
prop_fromNonEmpty_toNonEmpty s =
    checkCoverage_UTxOSelectionNonEmpty s $
    UTxOSelection.toNonEmpty (UTxOSelection.fromNonEmpty s)
    === Just s

prop_toNonEmpty_fromNonEmpty :: UTxOSelection TestUTxO -> Property
prop_toNonEmpty_fromNonEmpty s =
    checkCoverage_UTxOSelection s $
    (UTxOSelection.fromNonEmpty <$> UTxOSelection.toNonEmpty s)
    === (if UTxOSelection.isNonEmpty s then Just s else Nothing)

--------------------------------------------------------------------------------
-- Indicator and accessor functions
--------------------------------------------------------------------------------

prop_availableBalance_availableMap :: UTxOSelection TestUTxO -> Property
prop_availableBalance_availableMap s =
    checkCoverage_UTxOSelection s $
    UTxOSelection.availableBalance s
    === F.fold (UTxOSelection.availableMap s)

prop_availableMap_availableSize :: UTxOSelection TestUTxO -> Property
prop_availableMap_availableSize s =
    checkCoverage_UTxOSelection s $
    UTxOSelection.availableSize s
    === Map.size (UTxOSelection.availableMap s)

prop_isNonEmpty_selectedSize :: UTxOSelection TestUTxO -> Property
prop_isNonEmpty_selectedSize s =
    checkCoverage_UTxOSelection s $
    UTxOSelection.isNonEmpty s
    === (UTxOSelection.selectedSize s > 0)

prop_isNonEmpty_selectedIndex :: UTxOSelection TestUTxO -> Property
prop_isNonEmpty_selectedIndex s =
    checkCoverage_UTxOSelection s $
    UTxOSelection.isNonEmpty s
    === not (UTxOIndex.null (UTxOSelection.selectedIndex s))

prop_isNonEmpty_selectedList :: UTxOSelection TestUTxO -> Property
prop_isNonEmpty_selectedList s =
    checkCoverage_UTxOSelection s $
    UTxOSelection.isNonEmpty s
    === not (null (UTxOSelection.selectedList s))

prop_leftoverBalance_selectedBalance :: UTxOSelection TestUTxO -> Property
prop_leftoverBalance_selectedBalance s =
    checkCoverage_UTxOSelection s $
    (UTxOSelection.leftoverBalance s <> UTxOSelection.selectedBalance s)
    ===
    (<>)
        (UTxOIndex.balance (UTxOSelection.leftoverIndex s))
        (UTxOIndex.balance (UTxOSelection.selectedIndex s))

prop_leftoverSize_selectedSize :: UTxOSelection TestUTxO -> Property
prop_leftoverSize_selectedSize s =
    checkCoverage_UTxOSelection s $
    (UTxOSelection.leftoverSize s + UTxOSelection.selectedSize s)
    ===
    (+)
        (UTxOIndex.size (UTxOSelection.leftoverIndex s))
        (UTxOIndex.size (UTxOSelection.selectedIndex s))

--------------------------------------------------------------------------------
-- Modification
--------------------------------------------------------------------------------

prop_select_empty :: TestUTxO -> Property
prop_select_empty u =
    UTxOSelection.select u UTxOSelection.empty === Nothing

prop_select_isValid
    :: u ~ Size 4 TestUTxO => u -> UTxOSelection u -> Property
prop_select_isValid u s = property $
    checkCoverage_select u s $
    maybe True isValidSelectionNonEmpty (UTxOSelection.select u s)

prop_select_isLeftover
    :: u ~ Size 4 TestUTxO => u -> UTxOSelection u -> Property
prop_select_isLeftover u s =
    checkCoverage_select u s $
    (UTxOSelection.isLeftover u <$> UTxOSelection.select u s)
    ===
    if UTxOSelection.isLeftover u s then Just False else Nothing

prop_select_isSelected
    :: u ~ Size 4 TestUTxO => u -> UTxOSelection u -> Property
prop_select_isSelected u s =
    checkCoverage_select u s $
    (UTxOSelection.isSelected u <$> UTxOSelection.select u s)
    ===
    if UTxOSelection.isLeftover u s then Just True else Nothing

prop_select_isProperSubSelectionOf
    :: u ~ Size 4 TestUTxO => u -> UTxOSelection u -> Property
prop_select_isProperSubSelectionOf u s =
    checkCoverage_select u s $
    (UTxOSelection.isProperSubSelectionOf s <$> UTxOSelection.select u s)
    ===
    if UTxOSelection.isLeftover u s then Just True else Nothing

prop_select_availableBalance
    :: u ~ Size 4 TestUTxO => u -> UTxOSelection u -> Property
prop_select_availableBalance u s =
    checkCoverage_select u s $
    (UTxOSelection.availableBalance <$> UTxOSelection.select u s)
    ===
    if UTxOSelection.isLeftover u s
    then Just (UTxOSelection.availableBalance s)
    else Nothing

prop_select_availableMap
    :: u ~ Size 4 TestUTxO => u -> UTxOSelection u -> Property
prop_select_availableMap u s =
    checkCoverage_select u s $
    (UTxOSelection.availableMap <$> UTxOSelection.select u s)
    ===
    if UTxOSelection.isLeftover u s
    then Just (UTxOSelection.availableMap s)
    else Nothing

prop_select_leftoverSize
    :: u ~ Size 4 TestUTxO => u -> UTxOSelection u -> Property
prop_select_leftoverSize u s =
    checkCoverage_select u s $
    (UTxOSelection.leftoverSize <$> UTxOSelection.select u s)
    ===
    if UTxOSelection.isLeftover u s
    then Just (UTxOSelection.leftoverSize s - 1)
    else Nothing

prop_select_selectedSize
    :: u ~ Size 4 TestUTxO => u -> UTxOSelection u -> Property
prop_select_selectedSize u s =
    checkCoverage_select u s $
    (UTxOSelection.selectedSize <$> UTxOSelection.select u s)
    ===
    if UTxOSelection.isLeftover u s
    then Just (UTxOSelection.selectedSize s + 1)
    else Nothing

prop_selectMany_isSubSelectionOf
    :: (TestUTxO -> Bool) -> UTxOSelection TestUTxO -> Property
prop_selectMany_isSubSelectionOf f s =
    checkCoverage_filter f $
    checkCoverage_UTxOSelection s $
    UTxOSelection.isSubSelectionOf s (UTxOSelection.selectMany toSelect s)
    === True
  where
    toSelect = filter f $ fst <$> UTxOSelection.leftoverList s

prop_selectMany_leftoverSize_all :: UTxOSelection TestUTxO -> Property
prop_selectMany_leftoverSize_all s =
    checkCoverage_UTxOSelection s $
    UTxOSelection.leftoverSize
        (UTxOSelection.selectMany (fst <$> UTxOSelection.leftoverList s) s)
    === 0

prop_selectMany_selectedSize_all :: UTxOSelection TestUTxO -> Property
prop_selectMany_selectedSize_all s =
    checkCoverage_UTxOSelection s $
    UTxOSelection.selectedSize
        (UTxOSelection.selectMany (fst <$> UTxOSelection.leftoverList s) s)
    === (UTxOSelection.leftoverSize s + UTxOSelection.selectedSize s)

checkCoverage_select
    :: (Testable prop, Ord u) => u -> UTxOSelection u -> (prop -> Property)
checkCoverage_select u s
    = checkCoverage
    . cover 10 (UTxOSelection.isLeftover u s)
        "in leftover set"
    . cover 10 (UTxOSelection.isSelected u s)
        "in selected set"
    . cover 10 (not (UTxOSelection.isMember u s))
        "in neither set"

--------------------------------------------------------------------------------
-- Validity
--------------------------------------------------------------------------------

isValidSelection :: Ord u => IsUTxOSelection s u => s u -> Bool
isValidSelection s = UTxOIndex.disjoint
    (UTxOSelection.selectedIndex s)
    (UTxOSelection.leftoverIndex s)

isValidSelectionNonEmpty :: Ord u => UTxOSelectionNonEmpty u -> Bool
isValidSelectionNonEmpty s =
    isValidSelection s
    && UTxOSelection.isNonEmpty s
    && UTxOSelection.selectedSize s > 0
    && UTxOSelection.selectedIndex s /= UTxOIndex.empty
    && not (null (UTxOSelection.selectedList s))

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

newtype TestUTxO = TestUTxO (Hexadecimal Quid)
    deriving (Arbitrary, CoArbitrary) via Quid
    deriving stock (Eq, Ord, Read, Show)

instance (Arbitrary u, Ord u) => Arbitrary (UTxOIndex u)
  where
    arbitrary = genUTxOIndex arbitrary
    shrink = shrinkUTxOIndex shrink

instance (Arbitrary u, Ord u, Show u) => Arbitrary (UTxOSelection u)
  where
    arbitrary = genUTxOSelection arbitrary
    shrink = shrinkUTxOSelection shrink

instance (Arbitrary u, Ord u, Show u) => Arbitrary (UTxOSelectionNonEmpty u)
  where
    arbitrary = genUTxOSelectionNonEmpty arbitrary
    shrink = shrinkUTxOSelectionNonEmpty shrink

--------------------------------------------------------------------------------
-- Show instances
--------------------------------------------------------------------------------

instance Show (TestUTxO -> Bool) where
    show = const "(TestUTxO -> Bool)"
