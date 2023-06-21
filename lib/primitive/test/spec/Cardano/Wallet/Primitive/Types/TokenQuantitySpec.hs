{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.Types.TokenQuantitySpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity.Gen
    ( genTokenQuantityFullRange, genTokenQuantityPartition,
    shrinkTokenQuantityFullRange )
import Data.Aeson
    ( FromJSON (..), ToJSON (..) )
import Data.Function
    ( (&) )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( ToText (..) )
import Data.Typeable
    ( Typeable )
import System.FilePath
    ( (</>) )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess )
import Test.QuickCheck
    ( Arbitrary (..), Property, checkCoverage, conjoin, counterexample, cover,
    forAll, property, (===), (==>) )
import Test.QuickCheck.Classes
    ( eqLaws, monoidLaws, ordLaws, semigroupLaws, semigroupMonoidLaws,
    showReadLaws )
import Test.QuickCheck.Extra
    ( genNonEmpty, shrinkNonEmpty )
import Test.Text.Roundtrip
    ( textRoundtrip )
import Test.Utils.Laws
    ( testLawsMany )
import Test.Utils.Paths
    ( getTestData )

import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as TokenQuantity
import qualified Data.Char as Char
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Test.QuickCheck as QC
import qualified Test.Utils.Roundtrip as JsonRoundtrip

spec :: Spec
spec =
    describe "Token quantity properties" $
    modifyMaxSuccess (const 1000) $ do

    describe "Class instances obey laws" $ do
        testLawsMany @TokenQuantity
            [ eqLaws
            , monoidLaws
            , ordLaws
            , semigroupLaws
            , semigroupMonoidLaws
            , showReadLaws
            ]

    describe "Operations" $ do

        it "prop_pred_succ" $
            property prop_pred_succ
        it "prop_succ_pred" $
            property prop_succ_pred
        it "prop_succ_predZero" $
            property prop_succ_predZero
        it "prop_predZero_difference" $
            property prop_predZero_difference
        it "prop_predZero_pred" $
            property prop_predZero_pred
        it "prop_difference_zero (x - 0 = x)" $
            property prop_difference_zero
        it "prop_difference_zero2 (0 - x = 0)" $
            property prop_difference_zero2
        it "prop_difference_zero3 (x - x = 0)" $
            property prop_difference_zero3
        it "prop_difference_leq (x - y <= x)" $
            property prop_difference_leq
        it "prop_difference_add ((x - y) + y >= x)" $
            property prop_difference_add
        it "prop_add_difference ((x + y) - y = x)" $
            property prop_add_difference

    describe "Partitioning" $ do

        it "prop_partitionDefault_fold" $
            prop_partitionDefault_fold & property
        it "prop_partitionDefault_length" $
            prop_partitionDefault_length & property
        it "prop_partitionDefault_zeroWeightSum" $
            prop_partitionDefault_zeroWeightSum & property

    describe "Generating partitions" $ do

        it "prop_genTokenQuantityPartition_fold" $
            prop_genTokenQuantityPartition_fold & property
        it "prop_genTokenQuantityPartition_length" $
            prop_genTokenQuantityPartition_length & property
        it "prop_genTokenQuantityPartition_nonPositive" $
            prop_genTokenQuantityPartition_nonPositive & property

    describe "JSON serialization" $ do

        describe "Roundtrip tests" $ do
            testJson $ Proxy @TokenQuantity

    describe "Text serialization" $ do

        describe "Roundtrip tests" $ do
            textRoundtrip $ Proxy @TokenQuantity
        it "prop_toText_noQuotes" $ do
            property prop_toText_noQuotes

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

prop_pred_succ :: TokenQuantity -> Property
prop_pred_succ q = q > TokenQuantity.zero ==>
    (TokenQuantity.succ <$> TokenQuantity.pred q) === Just q

prop_succ_pred :: TokenQuantity -> Property
prop_succ_pred q =
    TokenQuantity.pred (TokenQuantity.succ q) === Just q

prop_succ_predZero :: TokenQuantity -> Property
prop_succ_predZero q =
    TokenQuantity.predZero (TokenQuantity.succ q) === q

prop_predZero_difference :: TokenQuantity -> Property
prop_predZero_difference q =
    checkCoverage $
    cover  1 (q == TokenQuantity 0) "q == 0" $
    cover 10 (q >= TokenQuantity 1) "q >= 1" $
    TokenQuantity.predZero q === q `TokenQuantity.difference` TokenQuantity 1

prop_predZero_pred :: TokenQuantity -> Property
prop_predZero_pred q =
    checkCoverage $
    cover  1 (q == TokenQuantity 0) "q == 0" $
    cover 10 (q >= TokenQuantity 1) "q >= 1" $
    if q == TokenQuantity.zero
    then TokenQuantity.predZero q === TokenQuantity.zero
    else Just (TokenQuantity.predZero q) === TokenQuantity.pred q

prop_difference_zero :: TokenQuantity -> Property
prop_difference_zero x =
    x `TokenQuantity.difference` TokenQuantity.zero === x

prop_difference_zero2 :: TokenQuantity -> Property
prop_difference_zero2 x =
    TokenQuantity.zero `TokenQuantity.difference` x === TokenQuantity.zero

prop_difference_zero3 :: TokenQuantity -> Property
prop_difference_zero3 x =
    x `TokenQuantity.difference` x === TokenQuantity.zero

prop_difference_leq :: TokenQuantity -> TokenQuantity -> Property
prop_difference_leq x y =
    let
        delta = x `TokenQuantity.difference` y
    in
      counterexample ("x = " <> show x) $
      counterexample ("y = " <> show y) $
      counterexample ("x - y = " <> show delta) $
      counterexample ("x - y is not <= " <> show x) $
      property $ delta <= x

prop_difference_add :: TokenQuantity -> TokenQuantity -> Property
prop_difference_add x y =
    let
        delta = x `TokenQuantity.difference` y
        yAndDelta = delta `TokenQuantity.add` y
    in
        counterexample ("x - y = " <> show delta) $
        counterexample ("(x - y) + y = " <> show yAndDelta) $
        counterexample ("x is not <= " <> show yAndDelta) $
        property $ x <= yAndDelta

prop_add_difference :: TokenQuantity -> TokenQuantity -> Property
prop_add_difference x y =
    conjoin
        [ (x `TokenQuantity.add` y) `TokenQuantity.difference` y === x
        , (x `TokenQuantity.add` y) `TokenQuantity.difference` x === y
        ]

--------------------------------------------------------------------------------
-- Partitioning
--------------------------------------------------------------------------------

prop_partitionDefault_fold
    :: TokenQuantity -> NonEmpty TokenQuantity -> Property
prop_partitionDefault_fold c cs =
    F.fold (TokenQuantity.partitionDefault c cs) === c

prop_partitionDefault_length
    :: TokenQuantity -> NonEmpty TokenQuantity -> Property
prop_partitionDefault_length c cs =
    length (TokenQuantity.partitionDefault c cs) === length cs

prop_partitionDefault_zeroWeightSum
    :: TokenQuantity -> NonEmpty () -> Property
prop_partitionDefault_zeroWeightSum c cs =
    TokenQuantity.partitionDefault c (TokenQuantity 0 <$ cs)
        === TokenQuantity.equipartition c cs

--------------------------------------------------------------------------------
-- Generating partitions
--------------------------------------------------------------------------------

prop_genTokenQuantityPartition_fold
    :: TokenQuantity -> QC.Positive (QC.Small Int) -> Property
prop_genTokenQuantityPartition_fold m (QC.Positive (QC.Small i)) =
    forAll (genTokenQuantityPartition m i) $ (=== m) . F.fold

prop_genTokenQuantityPartition_length
    :: TokenQuantity -> QC.Positive (QC.Small Int) -> Property
prop_genTokenQuantityPartition_length m (QC.Positive (QC.Small i)) =
    forAll (genTokenQuantityPartition m i) $ (=== i) . F.length

prop_genTokenQuantityPartition_nonPositive
    :: TokenQuantity -> QC.NonPositive (QC.Small Int) -> Property
prop_genTokenQuantityPartition_nonPositive m (QC.NonPositive (QC.Small i)) =
    forAll (genTokenQuantityPartition m i) (=== pure m)

--------------------------------------------------------------------------------
-- JSON serialization
--------------------------------------------------------------------------------

testJson
    :: (Arbitrary a, ToJSON a, FromJSON a, Typeable a) => Proxy a -> Spec
testJson = JsonRoundtrip.jsonRoundtripAndGolden testJsonDataDirectory

testJsonDataDirectory :: FilePath
testJsonDataDirectory =
    ($(getTestData) </> "Cardano" </> "Wallet" </> "Primitive" </> "Types")

--------------------------------------------------------------------------------
-- Text serialization
--------------------------------------------------------------------------------

prop_toText_noQuotes :: TokenQuantity -> Property
prop_toText_noQuotes q = property $ case text of
    c : cs ->
        Char.isDigit c || c == '-' && F.all Char.isDigit cs
    [] ->
        error "Unexpected empty string."
  where
    text = T.unpack $ toText q

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = genNonEmpty arbitrary
    shrink = shrinkNonEmpty shrink

instance Arbitrary TokenQuantity where
    -- We test with token quantities of a variety of magnitudes to ensure that
    -- roundtrip serialization works even with large values, both positive and
    -- negative.
    arbitrary = genTokenQuantityFullRange
    shrink = shrinkTokenQuantityFullRange
