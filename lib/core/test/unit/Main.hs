module Main where

import Prelude

import Cardano.Startup
    ( withUtf8Encoding )
import Test.Hspec.Runner
    ( defaultConfig, hspecWith )
import Test.Utils.Startup
    ( withLineBuffering )

import qualified Spec

main :: IO ()
main = withLineBuffering $ withUtf8Encoding $ hspecWith defaultConfig Spec.spec
