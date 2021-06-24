module Main where

import Prelude

import Cardano.Startup
    ( withUtf8Encoding )
import Test.Hspec.Core.Runner
    ( defaultConfig, hspecWith )
import Test.Hspec.Extra
    ( configWithExecutionTimes )
import Test.Utils.Startup
    ( withLineBuffering )

import qualified Spec

main :: IO ()
main = withLineBuffering
    $ withUtf8Encoding
    $ hspecWith (configWithExecutionTimes defaultConfig) Spec.spec
