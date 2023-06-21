{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Use camelCase" -}

module Cardano.Wallet.Primitive.Types.TokenBundleSpec
    ( spec
    ) where

import Prelude hiding
    ( subtract )

import Algebra.PartialOrd
    ( leq )
import Cardano.Numeric.Util
    ( inAscendingPartialOrder )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( Lexicographic (..), TokenBundle (..), add, difference, isCoin, subtract )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundlePartition, genTokenBundleSmallRange,
    shrinkTokenBundleSmallRange )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity.Gen
    ( genTokenQuantityPositive, shrinkTokenQuantityPositive )
import Data.Function
    ( (&) )
import Data.Ratio
    ( (%) )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess )
import Test.QuickCheck
    ( Arbitrary (..), Property, checkCoverage, counterexample, cover, forAll,
    property, (===), (==>) )
import Test.QuickCheck.Classes
    ( eqLaws, monoidLaws, ordLaws, semigroupLaws, semigroupMonoidLaws )
import Test.Utils.Laws
    ( testLawsMany )
import Test.Utils.Laws.PartialOrd
    ( partialOrdLaws )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as TokenQuantity
import qualified Data.Foldable as F
import qualified Test.QuickCheck as QC

spec :: Spec
spec =
    describe "Token bundle properties" $
    modifyMaxSuccess (const 1000) $ do

    describe "Class instances obey laws" $ do
        testLawsMany @TokenBundle
            [ eqLaws
            , monoidLaws
            , partialOrdLaws
            , semigroupLaws
            , semigroupMonoidLaws
            ]
        testLawsMany @(Lexicographic TokenBundle)
            [ eqLaws
            , ordLaws
            ]

    describe "Arithmetic" $ do
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

    describe "Partitioning quantities with an upper bound" $ do
        it "prop_equipartitionQuantitiesWithUpperBound_length" $
            property prop_equipartitionQuantitiesWithUpperBound_length
        it "prop_equipartitionQuantitiesWithUpperBound_order" $
            property prop_equipartitionQuantitiesWithUpperBound_order
        it "prop_equipartitionQuantitiesWithUpperBound_sum" $
            property prop_equipartitionQuantitiesWithUpperBound_sum

    describe "Generating partitions" $ do

        it "prop_genTokenBundlePartition_fold" $
            prop_genTokenBundlePartition_fold & property
        it "prop_genTokenBundlePartition_length" $
            prop_genTokenBundlePartition_length & property
        it "prop_genTokenBundlePartition_nonPositive" $
            prop_genTokenBundlePartition_nonPositive & property

    describe "Behaviour" $
        it "toCoin only returns when token bundle has only ADA" $
            property prop_toCoin_onlyAda

--------------------------------------------------------------------------------
-- Arithmetic properties
--------------------------------------------------------------------------------

prop_difference_zero :: TokenBundle -> Property
prop_difference_zero x =
    x `difference` mempty === x

prop_difference_zero2 :: TokenBundle -> Property
prop_difference_zero2 x =
    mempty `difference` x === mempty

prop_difference_zero3 :: TokenBundle -> Property
prop_difference_zero3 x =
    x `difference` x === mempty

prop_difference_leq :: TokenBundle -> TokenBundle -> Property
prop_difference_leq x y = do
    let delta = x `difference` y
    counterexample ("x - y = " <> show delta) $ property $ delta `leq` x

-- (x - y) + y ⊇ x
prop_difference_add :: TokenBundle -> TokenBundle -> Property
prop_difference_add x y =
    let
        delta = x `difference` y
        yAndDelta = delta `add` y
    in
        counterexample ("x - y = " <> show delta) $
        counterexample ("(x - y) + y = " <> show yAndDelta) $
        property $ x `leq` yAndDelta

prop_difference_subtract :: TokenBundle -> TokenBundle -> Property
prop_difference_subtract x y =
    y `leq` x ==> (===)
        (x `subtract` y)
        (Just $ x `difference` y)

prop_difference_equality :: TokenBundle -> TokenBundle -> Property
prop_difference_equality x y = checkCoverage $
    cover 5 (not (isCoin xReduced))
        "reduced bundles are not coins" $
    xReduced === yReduced
  where
    xReduced = x `difference` xExcess
    yReduced = y `difference` yExcess
    xExcess = x `difference` y
    yExcess = y `difference` x

--------------------------------------------------------------------------------
-- Partitioning quantities according to an upper bound
--------------------------------------------------------------------------------

-- | Computes the number of parts that 'equipartitionQuantitiesWithUpperBound'
--   should return.
--
equipartitionQuantitiesWithUpperBound_expectedLength
    :: TokenBundle -> TokenQuantity -> Int
equipartitionQuantitiesWithUpperBound_expectedLength
    (TokenBundle _ m) (TokenQuantity maxQuantity) =
        max 1 $ ceiling $ currentMaxQuantity % maxQuantity
  where
    TokenQuantity currentMaxQuantity = TokenMap.maximumQuantity m

prop_equipartitionQuantitiesWithUpperBound_length
    :: TokenBundle -> TokenQuantity -> Property
prop_equipartitionQuantitiesWithUpperBound_length m maxQuantity =
    maxQuantity > TokenQuantity.zero ==>
        length (TokenBundle.equipartitionQuantitiesWithUpperBound m maxQuantity)
            === equipartitionQuantitiesWithUpperBound_expectedLength
                m maxQuantity

prop_equipartitionQuantitiesWithUpperBound_order
    :: TokenBundle -> TokenQuantity -> Property
prop_equipartitionQuantitiesWithUpperBound_order m maxQuantity =
    maxQuantity > TokenQuantity.zero ==>
        inAscendingPartialOrder
            (TokenBundle.equipartitionQuantitiesWithUpperBound m maxQuantity)

prop_equipartitionQuantitiesWithUpperBound_sum
    :: TokenBundle -> TokenQuantity -> Property
prop_equipartitionQuantitiesWithUpperBound_sum m maxQuantity =
    maxQuantity > TokenQuantity.zero ==>
        F.fold (TokenBundle.equipartitionQuantitiesWithUpperBound m maxQuantity)
            === m

--------------------------------------------------------------------------------
-- Generating partitions
--------------------------------------------------------------------------------

prop_genTokenBundlePartition_fold
    :: TokenBundle -> QC.Positive (QC.Small Int) -> Property
prop_genTokenBundlePartition_fold m (QC.Positive (QC.Small i)) =
    forAll (genTokenBundlePartition m i) $ (=== m) . F.fold

prop_genTokenBundlePartition_length
    :: TokenBundle -> QC.Positive (QC.Small Int) -> Property
prop_genTokenBundlePartition_length m (QC.Positive (QC.Small i)) =
    forAll (genTokenBundlePartition m i) $ (=== i) . F.length

prop_genTokenBundlePartition_nonPositive
    :: TokenBundle -> QC.NonPositive (QC.Small Int) -> Property
prop_genTokenBundlePartition_nonPositive m (QC.NonPositive (QC.Small i)) =
    forAll (genTokenBundlePartition m i) (=== pure m)

--------------------------------------------------------------------------------
-- Behavioural properties
-------------------------------------------------------------------------------

prop_toCoin_onlyAda :: TokenBundle -> Property
prop_toCoin_onlyAda bundle =
    let
        result = TokenBundle.toCoin bundle
    in
        checkCoverage $
        cover 30 (not (TokenBundle.isCoin bundle))
            "Token bundle has at least 1 non-ada asset" $
        cover 30 (TokenBundle.isCoin bundle)
            "Token bundle has no non-ada assets" $
        if TokenBundle.isCoin bundle
        then result === Just (TokenBundle.coin bundle)
        else result === Nothing

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary TokenBundle where
    arbitrary = genTokenBundleSmallRange
    shrink = shrinkTokenBundleSmallRange

instance Arbitrary TokenQuantity where
    arbitrary = genTokenQuantityPositive
    shrink = shrinkTokenQuantityPositive

deriving instance Arbitrary (Lexicographic TokenBundle)
