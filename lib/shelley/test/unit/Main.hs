module Main where

import Prelude

import Test.Hspec.Core.Runner
    ( defaultConfig, hspecWith )
import Test.Hspec.Extra
    ( configWithExecutionTimes )

import qualified Spec

main :: IO ()
main = hspecWith (configWithExecutionTimes defaultConfig) Spec.spec
