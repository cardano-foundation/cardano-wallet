{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.Types.CoinSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoin, genCoinPositive, shrinkCoin, shrinkCoinPositive )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Extra
    ( parallel )
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

import qualified Cardano.Wallet.Primitive.Types.Coin as Coin

spec :: Spec
spec = describe "Cardano.Wallet.Primitive.Types.CoinSpec" $ do

    parallel $ describe "Arithmetic operations" $ do

        it "prop_add_toNatural" $ do
            property prop_add_toNatural
        it "prop_add_subtract" $ do
            property prop_add_subtract
        it "prop_difference_distance" $ do
            property prop_difference_distance
        it "prop_difference_subtract" $ do
            property prop_difference_subtract
        it "prop_distance_commutative" $ do
            property prop_distance_commutative
        it "prop_subtract_toNatural" $ do
            property prop_subtract_toNatural

    parallel $ describe "Generators and shrinkers" $ do

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

--------------------------------------------------------------------------------
-- Arithmetic operations
--------------------------------------------------------------------------------

prop_add_subtract :: Coin -> Coin -> Property
prop_add_subtract a b =
    checkCoverageCoin a b $
    conjoin
    [ (a `Coin.addCoin` b) `Coin.subtractCoin` b === Just a
    , (b `Coin.addCoin` a) `Coin.subtractCoin` a === Just b
    ]

prop_add_toNatural :: Coin -> Coin -> Property
prop_add_toNatural a b =
    checkCoverageCoin a b $
    (===)
        (Coin.toNatural (a `Coin.addCoin` b))
        (Coin.toNatural a + Coin.toNatural b)

prop_difference_distance :: Coin -> Coin -> Property
prop_difference_distance a b =
    checkCoverageCoin a b $
    if (a >= b)
    then a `Coin.distance` b == a `Coin.difference` b
    else a `Coin.distance` b == b `Coin.difference` a

prop_difference_subtract :: Coin -> Coin -> Property
prop_difference_subtract a b =
    checkCoverageCoin a b $
    if (a >= b)
    then a `Coin.subtractCoin` b === Just (a `Coin.difference` b)
    else a `Coin.subtractCoin` b === Nothing

prop_distance_commutative :: Coin -> Coin -> Property
prop_distance_commutative a b =
    checkCoverageCoin a b $
    a `Coin.distance` b === b `Coin.distance` a

prop_subtract_toNatural :: Coin -> Coin -> Property
prop_subtract_toNatural a b =
    checkCoverageCoin a b $
    if (a >= b)
    then
        (Coin.toNatural <$> (a `Coin.subtractCoin` b))
        ===
        (Just (Coin.toNatural a - Coin.toNatural b))
    else
        (Coin.toNatural <$> (b `Coin.subtractCoin` a))
        ===
        (Just (Coin.toNatural b - Coin.toNatural a))

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
isValidCoinPositive c = c > Coin 0 && c <= maxBound

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary Coin where
    arbitrary = genCoin
    shrink = shrinkCoin
