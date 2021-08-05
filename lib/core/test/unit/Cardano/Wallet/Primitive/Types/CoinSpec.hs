module Cardano.Wallet.Primitive.Types.CoinSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..), isValidCoin )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoin, genCoinPositive, shrinkCoin, shrinkCoinPositive )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Property, conjoin, forAll, property )

spec :: Spec
spec = do

    parallel $ describe "Generators and shrinkers" $ do

        describe "Coins that can be zero" $ do
            it "genCoin" $
                property prop_genCoin
            it "shrinkCoin" $
                property prop_shrinkCoin

        describe "Coins that are strictly positive" $ do
            it "genCoinPositive" $
                property prop_genCoinPositive
            it "shrinkCoinPositive" $
                property prop_shrinkCoinPositive

--------------------------------------------------------------------------------
-- Coins that can be zero
--------------------------------------------------------------------------------

prop_genCoin :: Property
prop_genCoin = forAll genCoin isValidCoin

prop_shrinkCoin :: Property
prop_shrinkCoin = forAll genCoin $ \c ->
    let shrunken = shrinkCoin c in
    conjoin $ ($ shrunken) <$>
        [ all (< c)
        , all isValidCoin
        ]

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
