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
    ( TokenBundle (..), add, difference, isCoin, subtract, unsafeSubtract )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRange, shrinkTokenBundleSmallRange )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity.Gen
    ( genTokenQuantitySmallPositive, shrinkTokenQuantitySmallPositive )
import Data.Ratio
    ( (%) )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , checkCoverage
    , counterexample
    , cover
    , property
    , (===)
    , (==>)
    )
import Test.QuickCheck.Classes
    ( eqLaws, monoidLaws, semigroupLaws, semigroupMonoidLaws )
import Test.Utils.Laws
    ( testLawsMany )
import Test.Utils.Laws.PartialOrd
    ( partialOrdLaws )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as TokenQuantity
import qualified Data.Foldable as F

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
    xReduced = x `unsafeSubtract` xExcess
    yReduced = y `unsafeSubtract` yExcess
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
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary TokenBundle where
    arbitrary = genTokenBundleSmallRange
    shrink = shrinkTokenBundleSmallRange

instance Arbitrary TokenQuantity where
    arbitrary = genTokenQuantitySmallPositive
    shrink = shrinkTokenQuantitySmallPositive
