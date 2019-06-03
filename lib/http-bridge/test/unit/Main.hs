module Main where

import Prelude

import Test.Hspec.Runner
    ( hspecWith )

import qualified Spec
import qualified Test.Hspec.Runner as Hspec

main :: IO ()
main = hspecWith Hspec.defaultConfig Spec.spec
