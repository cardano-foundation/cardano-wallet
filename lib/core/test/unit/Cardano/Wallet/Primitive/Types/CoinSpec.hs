{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
module Cardano.Wallet.Primitive.Types.CoinSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..), isValidCoin, scaleCoin, sumCoins )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoin, genCoinPositive, shrinkCoin, shrinkCoinPositive )
import Cardano.Wallet.Util
    ( tryInternalError )
import Data.List
    ( isInfixOf )
import GHC.Generics
    ( Generic )
import Test.Hspec
    ( Spec, describe )
import Test.Hspec.Extra
    ( parallel )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , conjoin
    , counterexample
    , forAll
    , frequency
    , genericShrink
    , getNonNegative
    , label
    , property
    , (===)
    )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )

spec :: Spec
spec = parallel $ do
    describe "Generators and shrinkers" $ do
        describe "Coins that can be zero" $ do
            prop "genCoin" prop_genCoin
            prop "shrinkCoin" prop_shrinkCoin

        describe "Coins that are strictly positive" $ do
            prop "genCoinPositive" prop_genCoinPositive
            prop "shrinkCoinPositive" prop_shrinkCoinPositive

    describe "Operations" $ do
        prop "scaleCoin" prop_scaleCoin

{-------------------------------------------------------------------------------
                             Coins that can be zero
-------------------------------------------------------------------------------}

prop_genCoin :: Coin -> Property
prop_genCoin = property . isValidCoin

prop_shrinkCoin :: Coin -> Property
prop_shrinkCoin c = conjoin $
    ($ shrinkCoin c) <$>
        [ all (< c)
        , all isValidCoin
        ]

{-------------------------------------------------------------------------------
                        Coins that are strictly positive
-------------------------------------------------------------------------------}

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

{-------------------------------------------------------------------------------
                                   Operations
-------------------------------------------------------------------------------}

data ScaleCoin = ScaleCoin
    { scale :: Integer
    , coin :: Coin
    } deriving (Generic, Show, Eq)

prop_scaleCoin :: ScaleCoin -> Property
prop_scaleCoin (ScaleCoin s c)
    | overflow  = label "overflow error" $ catchError "overflow"
    | underflow = label "underflow error" $ catchError "underflow"
    | otherwise = result === expected
  where
    result = scaleCoin s c
    expected = sumCoins (replicate (fromIntegral s) c)

    overflow = fromIntegral (unCoin c) * s > fromIntegral (unCoin maxBound)
    underflow = s < 0

    catchError e = monadicIO $ do
        res <- run $ tryInternalError result
        monitor (counterexample ("res = " ++ show res))
        case res of
            Right (Coin 0) -> pure ()
            Right _ -> fail "expected scaleCoin to crash but it didn't"
            Left msg -> assert $ e `isInfixOf` msg

{-------------------------------------------------------------------------------
                              Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary Coin where
    arbitrary = genCoin
    shrink = shrinkCoin

instance Arbitrary ScaleCoin where
    arbitrary = ScaleCoin
        <$> frequency
            [ (75, getNonNegative <$> arbitrary)
            , (20, arbitrary)
            , (5, (* 1000000) <$> arbitrary) ]
        <*> frequency
            [ (60, arbitrary)
            , (40, Coin . (* 45000000) <$> arbitrary) ]

    shrink = genericShrink
