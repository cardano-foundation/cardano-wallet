{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.Types.TokenBundleSpec
    ( spec
    ) where

import Prelude

import Algebra.PartialOrd
    ( leq )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle, add, difference )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRange, shrinkTokenBundleSmallRange )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess )
import Test.QuickCheck
    ( Arbitrary (..), Property, counterexample, property, (===) )
import Test.QuickCheck.Classes
    ( eqLaws, monoidLaws, semigroupLaws, semigroupMonoidLaws )
import Test.Utils.Laws
    ( testLawsMany )
import Test.Utils.Laws.PartialOrd
    ( partialOrdLaws )

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

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary TokenBundle where
    arbitrary = genTokenBundleSmallRange
    shrink = shrinkTokenBundleSmallRange
