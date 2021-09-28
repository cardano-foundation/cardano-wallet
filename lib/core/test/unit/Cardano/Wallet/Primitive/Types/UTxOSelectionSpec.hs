{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Use camelCase" -}

module Cardano.Wallet.Primitive.Types.UTxOSelectionSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( coarbitraryTxIn, genTxIn, shrinkTxIn )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( UTxOIndex )
import Cardano.Wallet.Primitive.Types.UTxOIndex.Gen
    ( genUTxOIndex, shrinkUTxOIndex )
import Cardano.Wallet.Primitive.Types.UTxOSelection
    ( IsUTxOSelection, UTxOSelection, UTxOSelectionNonEmpty )
import Cardano.Wallet.Primitive.Types.UTxOSelection.Gen
    ( genUTxOSelection
    , genUTxOSelectionNonEmpty
    , shrinkUTxOSelection
    , shrinkUTxOSelectionNonEmpty
    )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Extra
    ( parallel )
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

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
import qualified Cardano.Wallet.Primitive.Types.UTxOSelection as UTxOSelection

spec :: Spec
spec =
    describe "Cardano.Wallet.Primitive.Types.UTxOSelectionSpec" $ do

    parallel $ describe "Generators and shrinkers" $ do

        it "prop_genUTxOSelection" $
            property prop_genUTxOSelection
        it "prop_genUTxOSelectionNonEmpty" $
            property prop_genUTxOSelectionNonEmpty
        it "prop_shrinkUTxOSelection" $
            property prop_shrinkUTxOSelection
        it "prop_shrinkUTxOSelectionNonEmpty" $
            property prop_shrinkUTxOSelectionNonEmpty

    parallel $ describe "Construction and deconstruction" $ do

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

    parallel $ describe "Promotion and demotion" $ do

        it "prop_fromNonEmpty_toNonEmpty" $
            property prop_fromNonEmpty_toNonEmpty
        it "prop_toNonEmpty_fromNonEmpty" $
            property prop_toNonEmpty_fromNonEmpty

    parallel $ describe "Indicator and accessor functions" $ do

        it "prop_availableBalance_availableUTxO" $
            property prop_availableBalance_availableUTxO
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

    parallel $ describe "Modification" $ do

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
        it "prop_select_availableUTxO" $
            property prop_select_availableUTxO
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
    forAll genUTxOSelection $ \s ->
    checkCoverage_UTxOSelection s $
    isValidSelection s === True

prop_genUTxOSelectionNonEmpty :: Property
prop_genUTxOSelectionNonEmpty =
    forAll genUTxOSelectionNonEmpty $ \s ->
    checkCoverage_UTxOSelectionNonEmpty s $
    isValidSelectionNonEmpty s === True

prop_shrinkUTxOSelection :: Property
prop_shrinkUTxOSelection =
    forAll genUTxOSelection $ \s ->
    conjoin (isValidSelection <$> shrinkUTxOSelection s)

prop_shrinkUTxOSelectionNonEmpty :: Property
prop_shrinkUTxOSelectionNonEmpty =
    forAll genUTxOSelectionNonEmpty $ \s ->
    conjoin (isValidSelectionNonEmpty <$> shrinkUTxOSelectionNonEmpty s)

checkCoverage_UTxOSelection
    :: Testable p => IsUTxOSelection s => s -> (p -> Property)
checkCoverage_UTxOSelection s
    = checkCoverage_UTxOSelectionNonEmpty s
    . cover 2 (0 == ssize && ssize == lsize) "0 == lsize && lsize == ssize"
    . cover 2 (0 == ssize && ssize <  lsize) "0 == ssize && ssize <  lsize"
  where
    lsize = UTxOSelection.leftoverSize s
    ssize = UTxOSelection.selectedSize s

checkCoverage_UTxOSelectionNonEmpty
    :: Testable p => IsUTxOSelection s => s -> (p -> Property)
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

prop_fromIndex_isValid :: UTxOIndex -> Property
prop_fromIndex_isValid u =
    isValidSelection (UTxOSelection.fromIndex u)
    === True

prop_fromIndexFiltered_isValid :: (TxIn -> Bool) -> UTxOIndex -> Property
prop_fromIndexFiltered_isValid f u =
    isValidSelection (UTxOSelection.fromIndexFiltered f u)
    === True

prop_fromIndexPair_isValid :: (UTxOIndex, UTxOIndex) -> Property
prop_fromIndexPair_isValid (u1, u2) =
    isValidSelection (UTxOSelection.fromIndexPair (u1, u2))
    === True

prop_fromIndex_toIndexPair :: UTxOIndex -> Property
prop_fromIndex_toIndexPair u =
    UTxOSelection.toIndexPair (UTxOSelection.fromIndex u)
    === (u, UTxOIndex.empty)

prop_fromIndexFiltered_toIndexPair :: (TxIn -> Bool) -> UTxOIndex -> Property
prop_fromIndexFiltered_toIndexPair f u =
    UTxOSelection.toIndexPair (UTxOSelection.fromIndexFiltered f u)
    === (UTxOIndex.filter (not . f) u, UTxOIndex.filter f u)

prop_fromIndexPair_toIndexPair :: UTxOSelection -> Property
prop_fromIndexPair_toIndexPair s =
    checkCoverage_UTxOSelection s $
    UTxOSelection.fromIndexPair (UTxOSelection.toIndexPair s)
    === s

--------------------------------------------------------------------------------
-- Promotion and demotion
--------------------------------------------------------------------------------

prop_fromNonEmpty_toNonEmpty :: UTxOSelectionNonEmpty -> Property
prop_fromNonEmpty_toNonEmpty s =
    checkCoverage_UTxOSelectionNonEmpty s $
    UTxOSelection.toNonEmpty (UTxOSelection.fromNonEmpty s)
    === Just s

prop_toNonEmpty_fromNonEmpty :: UTxOSelection -> Property
prop_toNonEmpty_fromNonEmpty s =
    checkCoverage_UTxOSelection s $
    (UTxOSelection.fromNonEmpty <$> UTxOSelection.toNonEmpty s)
    === (if UTxOSelection.isNonEmpty s then Just s else Nothing)

--------------------------------------------------------------------------------
-- Indicator and accessor functions
--------------------------------------------------------------------------------

prop_availableBalance_availableUTxO :: UTxOSelection -> Property
prop_availableBalance_availableUTxO s =
    checkCoverage_UTxOSelection s $
    UTxOSelection.availableBalance s
    === UTxO.balance (UTxOSelection.availableUTxO s)

prop_isNonEmpty_selectedSize :: UTxOSelection -> Property
prop_isNonEmpty_selectedSize s =
    checkCoverage_UTxOSelection s $
    UTxOSelection.isNonEmpty s
    === (UTxOSelection.selectedSize s > 0)

prop_isNonEmpty_selectedIndex :: UTxOSelection -> Property
prop_isNonEmpty_selectedIndex s =
    checkCoverage_UTxOSelection s $
    UTxOSelection.isNonEmpty s
    === not (UTxOIndex.null (UTxOSelection.selectedIndex s))

prop_isNonEmpty_selectedList :: UTxOSelection -> Property
prop_isNonEmpty_selectedList s =
    checkCoverage_UTxOSelection s $
    UTxOSelection.isNonEmpty s
    === not (null (UTxOSelection.selectedList s))

prop_leftoverBalance_selectedBalance :: UTxOSelection -> Property
prop_leftoverBalance_selectedBalance s =
    checkCoverage_UTxOSelection s $
    (UTxOSelection.leftoverBalance s <> UTxOSelection.selectedBalance s)
    ===
    TokenBundle.add
        (UTxOIndex.balance (UTxOSelection.leftoverIndex s))
        (UTxOIndex.balance (UTxOSelection.selectedIndex s))

prop_leftoverSize_selectedSize :: UTxOSelection -> Property
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

prop_select_empty :: TxIn -> Property
prop_select_empty i =
    UTxOSelection.select i UTxOSelection.empty === Nothing

prop_select_isValid :: TxIn -> UTxOSelection -> Property
prop_select_isValid i s = property $
    checkCoverage_select i s $
    maybe True isValidSelectionNonEmpty (UTxOSelection.select i s)

prop_select_isLeftover :: TxIn -> UTxOSelection -> Property
prop_select_isLeftover i s =
    checkCoverage_select i s $
    (UTxOSelection.isLeftover i <$> UTxOSelection.select i s)
    ===
    if UTxOSelection.isLeftover i s then Just False else Nothing

prop_select_isSelected :: TxIn -> UTxOSelection -> Property
prop_select_isSelected i s =
    checkCoverage_select i s $
    (UTxOSelection.isSelected i <$> UTxOSelection.select i s)
    ===
    if UTxOSelection.isLeftover i s then Just True else Nothing

prop_select_isProperSubSelectionOf :: TxIn -> UTxOSelection -> Property
prop_select_isProperSubSelectionOf i s =
    checkCoverage_select i s $
    (UTxOSelection.isProperSubSelectionOf s <$> UTxOSelection.select i s)
    ===
    if UTxOSelection.isLeftover i s then Just True else Nothing

prop_select_availableBalance :: TxIn -> UTxOSelection -> Property
prop_select_availableBalance i s =
    checkCoverage_select i s $
    (UTxOSelection.availableBalance <$> UTxOSelection.select i s)
    ===
    if UTxOSelection.isLeftover i s
    then Just (UTxOSelection.availableBalance s)
    else Nothing

prop_select_availableUTxO :: TxIn -> UTxOSelection -> Property
prop_select_availableUTxO i s =
    checkCoverage_select i s $
    (UTxOSelection.availableUTxO <$> UTxOSelection.select i s)
    ===
    if UTxOSelection.isLeftover i s
    then Just (UTxOSelection.availableUTxO s)
    else Nothing

prop_select_leftoverSize :: TxIn -> UTxOSelection -> Property
prop_select_leftoverSize i s =
    checkCoverage_select i s $
    (UTxOSelection.leftoverSize <$> UTxOSelection.select i s)
    ===
    if UTxOSelection.isLeftover i s
    then Just (UTxOSelection.leftoverSize s - 1)
    else Nothing

prop_select_selectedSize :: TxIn -> UTxOSelection -> Property
prop_select_selectedSize i s =
    checkCoverage_select i s $
    (UTxOSelection.selectedSize <$> UTxOSelection.select i s)
    ===
    if UTxOSelection.isLeftover i s
    then Just (UTxOSelection.selectedSize s + 1)
    else Nothing

prop_selectMany_isSubSelectionOf :: (TxIn -> Bool) -> UTxOSelection -> Property
prop_selectMany_isSubSelectionOf f s =
    checkCoverage_UTxOSelection s $
    UTxOSelection.isSubSelectionOf s (UTxOSelection.selectMany toSelect s)
    === True
  where
    toSelect = filter f $ fst <$> UTxOSelection.leftoverList s

prop_selectMany_leftoverSize_all :: UTxOSelection -> Property
prop_selectMany_leftoverSize_all s =
    checkCoverage_UTxOSelection s $
    UTxOSelection.leftoverSize
        (UTxOSelection.selectMany (fst <$> UTxOSelection.leftoverList s) s)
    === 0

prop_selectMany_selectedSize_all :: UTxOSelection -> Property
prop_selectMany_selectedSize_all s =
    checkCoverage_UTxOSelection s $
    UTxOSelection.selectedSize
        (UTxOSelection.selectMany (fst <$> UTxOSelection.leftoverList s) s)
    === (UTxOSelection.leftoverSize s + UTxOSelection.selectedSize s)

checkCoverage_select
    :: Testable prop => TxIn -> UTxOSelection -> (prop -> Property)
checkCoverage_select i s
    = checkCoverage
    . cover 10 (UTxOSelection.isLeftover i s)
        "in leftover set"
    . cover 10 (UTxOSelection.isSelected i s)
        "in selected set"
    . cover 10 (not (UTxOSelection.isMember i s))
        "in neither set"

--------------------------------------------------------------------------------
-- Validity
--------------------------------------------------------------------------------

isValidSelection :: IsUTxOSelection s => s -> Bool
isValidSelection s = UTxOIndex.disjoint
    (UTxOSelection.selectedIndex s)
    (UTxOSelection.leftoverIndex s)

isValidSelectionNonEmpty :: UTxOSelectionNonEmpty -> Bool
isValidSelectionNonEmpty s =
    isValidSelection s
    && UTxOSelection.isNonEmpty s
    && UTxOSelection.selectedSize s > 0
    && UTxOSelection.selectedIndex s /= UTxOIndex.empty
    && not (null (UTxOSelection.selectedList s))

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary TxIn where
    arbitrary = genTxIn
    shrink = shrinkTxIn

instance Arbitrary UTxOIndex where
    arbitrary = genUTxOIndex
    shrink = shrinkUTxOIndex

instance Arbitrary UTxOSelection where
    arbitrary = genUTxOSelection
    shrink = shrinkUTxOSelection

instance Arbitrary UTxOSelectionNonEmpty where
    arbitrary = genUTxOSelectionNonEmpty
    shrink = shrinkUTxOSelectionNonEmpty

--------------------------------------------------------------------------------
-- CoArbitrary instances
--------------------------------------------------------------------------------

instance CoArbitrary TxIn where
    coarbitrary = coarbitraryTxIn

--------------------------------------------------------------------------------
-- Show instances
--------------------------------------------------------------------------------

instance Show (TxIn -> Bool) where
    show = const "(TxIn -> Bool)"
