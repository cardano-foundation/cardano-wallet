module Main where

import Prelude

import Test.Hspec.Core.Spec
    ( sequential
    )

import qualified Test.Integration.Run
    ( mainWith
    )

main :: IO ()
main = Test.Integration.Run.mainWith sequential
