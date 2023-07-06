{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.Types.CoinSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoin
    , genCoinPartition
    , genCoinPositive
    , shrinkCoin
    , shrinkCoinPositive
    )
import Data.Function
    ( (&) )
import Data.List.NonEmpty
    ( NonEmpty )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , Testable
    , checkCoverage
    , conjoin
    , cover
    , forAll
    , property
    , (===)
    )
import Test.QuickCheck.Extra
    ( genNonEmpty, shrinkNonEmpty )

import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Data.Foldable as F
import qualified Test.QuickCheck as QC

spec :: Spec
spec = describe "Cardano.Wallet.Primitive.Types.CoinSpec" $ do

    describe "Partitioning" $ do

        it "prop_partitionDefault_fold" $
            prop_partitionDefault_fold & property
        it "prop_partitionDefault_length" $
            prop_partitionDefault_length & property
        it "prop_partitionDefault_zeroWeightSum" $
            prop_partitionDefault_zeroWeightSum & property

    describe "Generators and shrinkers" $ do

        describe "Coins that can be zero" $ do
            it "genCoin_coverage" $
                property prop_genCoin_coverage
            it "shrinkCoin" $
                property prop_shrinkCoin

        describe "Coins that are strictly positive" $ do
            it "genCoinPositive" $
                property prop_genCoinPositive
            it "shrinkCoinPositive" $
                property prop_shrinkCoinPositive

    describe "Generating partitions" $ do

        it "prop_genCoinPartition_fold" $
            prop_genCoinPartition_fold & property
        it "prop_genCoinPartition_length" $
            prop_genCoinPartition_length & property
        it "prop_genCoinPartition_nonPositive" $
            prop_genCoinPartition_nonPositive & property

--------------------------------------------------------------------------------
-- Partitioning
--------------------------------------------------------------------------------

prop_partitionDefault_fold :: Coin -> NonEmpty Coin -> Property
prop_partitionDefault_fold c cs =
    F.fold (Coin.partitionDefault c cs) === c

prop_partitionDefault_length :: Coin -> NonEmpty Coin -> Property
prop_partitionDefault_length c cs =
    length (Coin.partitionDefault c cs) === length cs

prop_partitionDefault_zeroWeightSum :: Coin -> NonEmpty () -> Property
prop_partitionDefault_zeroWeightSum c cs =
    Coin.partitionDefault c (Coin 0 <$ cs) === Coin.equipartition c cs

--------------------------------------------------------------------------------
-- Coins that can be zero
--------------------------------------------------------------------------------

prop_genCoin_coverage :: Coin -> Coin -> Property
prop_genCoin_coverage a b =
    checkCoverageCoin a b True

checkCoverageCoin :: Testable prop => Coin -> Coin -> (prop -> Property)
checkCoverageCoin a b
    = checkCoverage
    . cover  1 (a == Coin 0 && b == Coin 0) "a == 0 && b == 0"
    . cover  2 (a == Coin 0 && b /= Coin 0) "a == 0 && b /= 0"
    . cover  2 (a /= Coin 0 && b == Coin 0) "a /= 0 && b == 0"
    . cover 10 (a /= Coin 0 && b /= Coin 0) "a /= 0 && b /= 0"
    . cover 10 (a <  b) "a < b"
    . cover  2 (a == b) "a = b"
    . cover 10 (a >  b) "a > b"

prop_shrinkCoin :: Property
prop_shrinkCoin = forAll genCoin $ \c ->
    let shrunken = shrinkCoin c in
    all (< c) shrunken

--------------------------------------------------------------------------------
-- Coins that are strictly positive
--------------------------------------------------------------------------------

prop_genCoinPositive :: Property
prop_genCoinPositive = forAll genCoinPositive isValidCoinPositive

prop_shrinkCoinPositive :: Property
prop_shrinkCoinPositive = forAll genCoinPositive $ \c ->
    let shrunken = shrinkCoinPositive c in
    conjoin $ ($ shrunken) <$>
        [ all (< c)
        , all isValidCoinPositive
        ]

isValidCoinPositive :: Coin -> Bool
isValidCoinPositive c = c > Coin 0

--------------------------------------------------------------------------------
-- Generating partitions
--------------------------------------------------------------------------------

prop_genCoinPartition_fold
    :: Coin -> QC.Positive (QC.Small Int) -> Property
prop_genCoinPartition_fold m (QC.Positive (QC.Small i)) =
    forAll (genCoinPartition m i) $ (=== m) . F.fold

prop_genCoinPartition_length
    :: Coin -> QC.Positive (QC.Small Int) -> Property
prop_genCoinPartition_length m (QC.Positive (QC.Small i)) =
    forAll (genCoinPartition m i) $ (=== i) . F.length

prop_genCoinPartition_nonPositive
    :: Coin -> QC.NonPositive (QC.Small Int) -> Property
prop_genCoinPartition_nonPositive m (QC.NonPositive (QC.Small i)) =
    forAll (genCoinPartition m i) (=== pure m)

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary Coin where
    arbitrary = genCoin
    shrink = shrinkCoin

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = genNonEmpty arbitrary
    shrink = shrinkNonEmpty shrink
