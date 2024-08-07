{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Read.EraValueSpec (spec) where

import Prelude

import Cardano.Wallet.Read.Eras
    ( Era (..)
    , EraValue (..)
    , IsEra
    , K (..)
    , eraValueSerialize
    )
import Control.Lens
    ( Prism'
    , preview
    , review
    )
import Test.Hspec
    ( Expectation
    , Spec
    , describe
    , it
    , shouldBe
    , shouldNotBe
    , shouldSatisfy
    )
import Test.QuickCheck
    ( Gen
    , Testable (..)
    , elements
    , forAll
    )

generate :: Gen (EraValue (K Int))
generate = do
    era <- elements [0 :: Int .. 6]
    case era of
        _ -> error "todo"

inject :: forall era a. IsEra era => Era era -> a -> EraValue (K a)
inject _ x = EraValue (K x :: K a era)

injectInt :: forall era. IsEra era => Era era -> Int -> EraValue (K Int)
injectInt = inject

spec :: Spec
spec =
    describe "EraValue" $ do
        it "respects equality" $ do
            injectInt Byron 1 `shouldBe` injectInt Byron 1
            injectInt Byron 1 `shouldNotBe` injectInt Byron 2
            injectInt Byron 1 `shouldNotBe` injectInt Shelley 1
        it "respects ord" $ do
            injectInt Byron 1 `shouldSatisfy` (< injectInt Byron 2)
            injectInt Byron 1 `shouldSatisfy` (<= injectInt Byron 1)
            injectInt Byron 1 `shouldSatisfy` (>= injectInt Byron 1)
            injectInt Byron 1 `shouldSatisfy` (> injectInt Byron 0)
            injectInt Byron 1 `shouldSatisfy` (< injectInt Shelley 1)
            injectInt Shelley 1 `shouldSatisfy` (< injectInt Allegra 1)
            injectInt Allegra 1 `shouldSatisfy` (< injectInt Mary 1)
            injectInt Mary 1 `shouldSatisfy` (< injectInt Alonzo 1)
            injectInt Alonzo 1 `shouldSatisfy` (< injectInt Babbage 1)
            injectInt Babbage 1 `shouldSatisfy` (< injectInt Conway 1)
        it "roundrips serialization" $ do
            property $ forAll generate $ prismLaw eraValueSerialize

prismLaw :: (Eq a, Show a) => Prism' s a -> a -> Expectation
prismLaw l b = preview l (review l b) `shouldBe` Just b
