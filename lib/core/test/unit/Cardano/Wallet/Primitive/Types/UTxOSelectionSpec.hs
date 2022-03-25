{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Use camelCase" -}

module Cardano.Wallet.Primitive.Types.UTxOSelectionSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.CoinSelection
    ( WalletUTxO (..) )
import Cardano.Wallet.CoinSelection.Gen
    ( coarbitraryWalletUTxO, genWalletUTxO, shrinkWalletUTxO )
import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Cardano.Wallet.Primitive.Types.Address.Gen
    ( coarbitraryAddress )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( coarbitraryTxIn )
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
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
import qualified Cardano.Wallet.Primitive.Types.UTxOSelection as UTxOSelection
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map

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
    forAll (genUTxOSelection genWalletUTxO) $ \s ->
    checkCoverage_UTxOSelection s $
    isValidSelection s === True

prop_genUTxOSelectionNonEmpty :: Property
prop_genUTxOSelectionNonEmpty =
    forAll (genUTxOSelectionNonEmpty genWalletUTxO) $ \s ->
    checkCoverage_UTxOSelectionNonEmpty s $
    isValidSelectionNonEmpty s === True

prop_shrinkUTxOSelection :: Property
prop_shrinkUTxOSelection =
    forAll (genUTxOSelection genWalletUTxO) $ \s ->
    conjoin (isValidSelection <$> shrinkUTxOSelection shrinkWalletUTxO s)

prop_shrinkUTxOSelectionNonEmpty :: Property
prop_shrinkUTxOSelectionNonEmpty =
    forAll (genUTxOSelectionNonEmpty genWalletUTxO) $ \s ->
    conjoin $ isValidSelectionNonEmpty
        <$> shrinkUTxOSelectionNonEmpty shrinkWalletUTxO s

checkCoverage_UTxOSelection
    :: Testable p
    => IsUTxOSelection s WalletUTxO
    => s WalletUTxO
    -> (p -> Property)
checkCoverage_UTxOSelection s
    = checkCoverage_UTxOSelectionNonEmpty s
    . cover 2 (0 == ssize && ssize == lsize) "0 == lsize && lsize == ssize"
    . cover 2 (0 == ssize && ssize <  lsize) "0 == ssize && ssize <  lsize"
  where
    lsize = UTxOSelection.leftoverSize s
    ssize = UTxOSelection.selectedSize s

checkCoverage_UTxOSelectionNonEmpty
    :: Testable p
    => IsUTxOSelection s WalletUTxO
    => s WalletUTxO
    -> (p -> Property)
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

prop_fromIndex_isValid :: UTxOIndex WalletUTxO -> Property
prop_fromIndex_isValid i =
    isValidSelection (UTxOSelection.fromIndex i)
    === True

prop_fromIndexFiltered_isValid
    :: (WalletUTxO -> Bool) -> UTxOIndex WalletUTxO -> Property
prop_fromIndexFiltered_isValid f i =
    isValidSelection (UTxOSelection.fromIndexFiltered f i)
    === True

prop_fromIndexPair_isValid
    :: (UTxOIndex WalletUTxO, UTxOIndex WalletUTxO) -> Property
prop_fromIndexPair_isValid (i1, i2) =
    isValidSelection (UTxOSelection.fromIndexPair (i1, i2))
    === True

prop_fromIndex_toIndexPair :: UTxOIndex WalletUTxO-> Property
prop_fromIndex_toIndexPair i =
    UTxOSelection.toIndexPair (UTxOSelection.fromIndex i)
    === (i, UTxOIndex.empty)

prop_fromIndexFiltered_toIndexPair
    :: (WalletUTxO -> Bool)
    -> UTxOIndex WalletUTxO
    -> Property
prop_fromIndexFiltered_toIndexPair f i =
    UTxOSelection.toIndexPair (UTxOSelection.fromIndexFiltered f i)
    === (UTxOIndex.filter (not . f) i, UTxOIndex.filter f i)

prop_fromIndexPair_toIndexPair :: UTxOSelection WalletUTxO -> Property
prop_fromIndexPair_toIndexPair s =
    checkCoverage_UTxOSelection s $
    UTxOSelection.fromIndexPair (UTxOSelection.toIndexPair s)
    === s

--------------------------------------------------------------------------------
-- Promotion and demotion
--------------------------------------------------------------------------------

prop_fromNonEmpty_toNonEmpty :: UTxOSelectionNonEmpty WalletUTxO -> Property
prop_fromNonEmpty_toNonEmpty s =
    checkCoverage_UTxOSelectionNonEmpty s $
    UTxOSelection.toNonEmpty (UTxOSelection.fromNonEmpty s)
    === Just s

prop_toNonEmpty_fromNonEmpty :: UTxOSelection WalletUTxO -> Property
prop_toNonEmpty_fromNonEmpty s =
    checkCoverage_UTxOSelection s $
    (UTxOSelection.fromNonEmpty <$> UTxOSelection.toNonEmpty s)
    === (if UTxOSelection.isNonEmpty s then Just s else Nothing)

--------------------------------------------------------------------------------
-- Indicator and accessor functions
--------------------------------------------------------------------------------

prop_availableBalance_availableMap :: UTxOSelection WalletUTxO -> Property
prop_availableBalance_availableMap s =
    checkCoverage_UTxOSelection s $
    UTxOSelection.availableBalance s
    === F.fold (UTxOSelection.availableMap s)

prop_availableMap_availableSize :: UTxOSelection WalletUTxO -> Property
prop_availableMap_availableSize s =
    checkCoverage_UTxOSelection s $
    UTxOSelection.availableSize s
    === Map.size (UTxOSelection.availableMap s)

prop_isNonEmpty_selectedSize :: UTxOSelection WalletUTxO -> Property
prop_isNonEmpty_selectedSize s =
    checkCoverage_UTxOSelection s $
    UTxOSelection.isNonEmpty s
    === (UTxOSelection.selectedSize s > 0)

prop_isNonEmpty_selectedIndex :: UTxOSelection WalletUTxO -> Property
prop_isNonEmpty_selectedIndex s =
    checkCoverage_UTxOSelection s $
    UTxOSelection.isNonEmpty s
    === not (UTxOIndex.null (UTxOSelection.selectedIndex s))

prop_isNonEmpty_selectedList :: UTxOSelection WalletUTxO -> Property
prop_isNonEmpty_selectedList s =
    checkCoverage_UTxOSelection s $
    UTxOSelection.isNonEmpty s
    === not (null (UTxOSelection.selectedList s))

prop_leftoverBalance_selectedBalance :: UTxOSelection WalletUTxO -> Property
prop_leftoverBalance_selectedBalance s =
    checkCoverage_UTxOSelection s $
    (UTxOSelection.leftoverBalance s <> UTxOSelection.selectedBalance s)
    ===
    TokenBundle.add
        (UTxOIndex.balance (UTxOSelection.leftoverIndex s))
        (UTxOIndex.balance (UTxOSelection.selectedIndex s))

prop_leftoverSize_selectedSize :: UTxOSelection WalletUTxO -> Property
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

prop_select_empty :: WalletUTxO -> Property
prop_select_empty u =
    UTxOSelection.select u UTxOSelection.empty === Nothing

prop_select_isValid :: WalletUTxO -> UTxOSelection WalletUTxO -> Property
prop_select_isValid u s = property $
    checkCoverage_select u s $
    maybe True isValidSelectionNonEmpty (UTxOSelection.select u s)

prop_select_isLeftover :: WalletUTxO -> UTxOSelection WalletUTxO -> Property
prop_select_isLeftover u s =
    checkCoverage_select u s $
    (UTxOSelection.isLeftover u <$> UTxOSelection.select u s)
    ===
    if UTxOSelection.isLeftover u s then Just False else Nothing

prop_select_isSelected :: WalletUTxO -> UTxOSelection WalletUTxO -> Property
prop_select_isSelected u s =
    checkCoverage_select u s $
    (UTxOSelection.isSelected u <$> UTxOSelection.select u s)
    ===
    if UTxOSelection.isLeftover u s then Just True else Nothing

prop_select_isProperSubSelectionOf
    :: WalletUTxO -> UTxOSelection WalletUTxO -> Property
prop_select_isProperSubSelectionOf u s =
    checkCoverage_select u s $
    (UTxOSelection.isProperSubSelectionOf s <$> UTxOSelection.select u s)
    ===
    if UTxOSelection.isLeftover u s then Just True else Nothing

prop_select_availableBalance
    :: WalletUTxO -> UTxOSelection WalletUTxO -> Property
prop_select_availableBalance u s =
    checkCoverage_select u s $
    (UTxOSelection.availableBalance <$> UTxOSelection.select u s)
    ===
    if UTxOSelection.isLeftover u s
    then Just (UTxOSelection.availableBalance s)
    else Nothing

prop_select_availableMap :: WalletUTxO -> UTxOSelection WalletUTxO -> Property
prop_select_availableMap u s =
    checkCoverage_select u s $
    (UTxOSelection.availableMap <$> UTxOSelection.select u s)
    ===
    if UTxOSelection.isLeftover u s
    then Just (UTxOSelection.availableMap s)
    else Nothing

prop_select_leftoverSize :: WalletUTxO -> UTxOSelection WalletUTxO -> Property
prop_select_leftoverSize u s =
    checkCoverage_select u s $
    (UTxOSelection.leftoverSize <$> UTxOSelection.select u s)
    ===
    if UTxOSelection.isLeftover u s
    then Just (UTxOSelection.leftoverSize s - 1)
    else Nothing

prop_select_selectedSize :: WalletUTxO -> UTxOSelection WalletUTxO -> Property
prop_select_selectedSize u s =
    checkCoverage_select u s $
    (UTxOSelection.selectedSize <$> UTxOSelection.select u s)
    ===
    if UTxOSelection.isLeftover u s
    then Just (UTxOSelection.selectedSize s + 1)
    else Nothing

prop_selectMany_isSubSelectionOf
    :: (WalletUTxO -> Bool) -> UTxOSelection WalletUTxO -> Property
prop_selectMany_isSubSelectionOf f s =
    checkCoverage_UTxOSelection s $
    UTxOSelection.isSubSelectionOf s (UTxOSelection.selectMany toSelect s)
    === True
  where
    toSelect = filter f $ fst <$> UTxOSelection.leftoverList s

prop_selectMany_leftoverSize_all :: UTxOSelection WalletUTxO -> Property
prop_selectMany_leftoverSize_all s =
    checkCoverage_UTxOSelection s $
    UTxOSelection.leftoverSize
        (UTxOSelection.selectMany (fst <$> UTxOSelection.leftoverList s) s)
    === 0

prop_selectMany_selectedSize_all :: UTxOSelection WalletUTxO -> Property
prop_selectMany_selectedSize_all s =
    checkCoverage_UTxOSelection s $
    UTxOSelection.selectedSize
        (UTxOSelection.selectMany (fst <$> UTxOSelection.leftoverList s) s)
    === (UTxOSelection.leftoverSize s + UTxOSelection.selectedSize s)

checkCoverage_select
    :: Testable prop
    => WalletUTxO
    -> UTxOSelection WalletUTxO
    -> (prop -> Property)
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

isValidSelection :: IsUTxOSelection s WalletUTxO => s WalletUTxO -> Bool
isValidSelection s = UTxOIndex.disjoint
    (UTxOSelection.selectedIndex s)
    (UTxOSelection.leftoverIndex s)

isValidSelectionNonEmpty :: UTxOSelectionNonEmpty WalletUTxO -> Bool
isValidSelectionNonEmpty s =
    isValidSelection s
    && UTxOSelection.isNonEmpty s
    && UTxOSelection.selectedSize s > 0
    && UTxOSelection.selectedIndex s /= UTxOIndex.empty
    && not (null (UTxOSelection.selectedList s))

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

-- TODO: ADP-1448:
--
-- Replace this instance with one for a mock input identifier, after the type
-- of input identifier has been made into a type parameter.
--
instance Arbitrary WalletUTxO where
    arbitrary = genWalletUTxO
    shrink = shrinkWalletUTxO

instance Arbitrary (UTxOIndex WalletUTxO) where
    arbitrary = genUTxOIndex genWalletUTxO
    shrink = shrinkUTxOIndex shrinkWalletUTxO

instance Arbitrary (UTxOSelection WalletUTxO) where
    arbitrary = genUTxOSelection genWalletUTxO
    shrink = shrinkUTxOSelection shrinkWalletUTxO

instance Arbitrary (UTxOSelectionNonEmpty WalletUTxO) where
    arbitrary = genUTxOSelectionNonEmpty genWalletUTxO
    shrink = shrinkUTxOSelectionNonEmpty shrinkWalletUTxO

--------------------------------------------------------------------------------
-- CoArbitrary instances
--------------------------------------------------------------------------------

instance CoArbitrary Address where
    coarbitrary = coarbitraryAddress

instance CoArbitrary TxIn where
    coarbitrary = coarbitraryTxIn

instance CoArbitrary WalletUTxO where
    coarbitrary = coarbitraryWalletUTxO

--------------------------------------------------------------------------------
-- Show instances
--------------------------------------------------------------------------------

instance Show (WalletUTxO -> Bool) where
    show = const "(WalletUTxO -> Bool)"
