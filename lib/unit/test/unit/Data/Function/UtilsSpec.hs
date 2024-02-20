{-# LANGUAGE ScopedTypeVariables #-}

module Data.Function.UtilsSpec
    ( spec
    ) where

import Prelude

import Data.Function.Utils
    ( applyN
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( NonNegative (..)
    , property
    , withMaxSuccess
    , (===)
    )

spec :: Spec
spec = describe "Function utilities" $ do

    describe "applyN" $ do

        it "forall m n . n >= 0 : applyN n (+ 1) m == m + n" $
            withMaxSuccess 10000 $
                property $ \(NonNegative (n :: Int)) (m :: Int) ->
                    applyN n (+ 1) m === n + m

        it "forall m n . n <= 0 : applyN n (+ 1) m == m" $
            withMaxSuccess 10000 $
                property $ \(NonNegative (n :: Int)) (m :: Int) ->
                    applyN (negate n) (+ 1) m === m
