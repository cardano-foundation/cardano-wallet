{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Read.EraValueSpec (spec) where

import Prelude

import Cardano.Wallet.Read.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , EraValue (..)
    , K (..)
    , Mary
    , Shelley
    , eraValue
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
    ( Arbitrary (..)
    , Gen
    , Testable (..)
    , elements
    , forAll
    )

k :: Int -> K Int era
k = K

generate :: Gen (EraValue (K Int))
generate = do
    era <- elements [0 :: Int .. 6]
    case era of
        0 -> eraValue @Byron . k <$> arbitrary
        1 -> eraValue @Shelley . k <$> arbitrary
        2 -> eraValue @Allegra . k <$> arbitrary
        3 -> eraValue @Mary . k <$> arbitrary
        4 -> eraValue @Alonzo . k <$> arbitrary
        5 -> eraValue @Babbage . k <$> arbitrary
        6 -> eraValue @Conway . k <$> arbitrary
        _ -> error "impossible"

spec :: Spec
spec =
    describe "EraValue" $ do
        it "respects equality" $ do
            eraValue @Byron (k 1) `shouldBe` eraValue @Byron (k 1)
            eraValue @Byron (k 1) `shouldNotBe` eraValue @Byron (k 2)
            eraValue @Byron (k 1) `shouldNotBe` eraValue @Shelley (k 1)
        it "respects ord" $ do
            eraValue @Byron (k 1) `shouldSatisfy` (< eraValue @Byron (k 2))
            eraValue @Byron (k 1) `shouldSatisfy` (<= eraValue @Byron (k 1))
            eraValue @Byron (k 1) `shouldSatisfy` (>= eraValue @Byron (k 1))
            eraValue @Byron (k 1) `shouldSatisfy` (> eraValue @Byron (k 0))
            eraValue @Byron (k 1) `shouldSatisfy` (< eraValue @Shelley (k 1))
            eraValue @Shelley (k 1) `shouldSatisfy` (< eraValue @Allegra (k 1))
            eraValue @Allegra (k 1) `shouldSatisfy` (< eraValue @Mary (k 1))
            eraValue @Mary (k 1) `shouldSatisfy` (< eraValue @Alonzo (k 1))
            eraValue @Alonzo (k 1) `shouldSatisfy` (< eraValue @Babbage (k 1))
            eraValue @Babbage (k 1) `shouldSatisfy` (< eraValue @Conway (k 1))
        it "roundrips serialization" $ do
            property $ forAll generate $ prismLaw eraValueSerialize

prismLaw :: (Eq a, Show a) => Prism' s a -> a -> Expectation
prismLaw l b = preview l (review l b) `shouldBe` Just b
