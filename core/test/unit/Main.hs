module Main where

import Cardano.Startup
    ( withUtf8Encoding )
import Prelude
import qualified Spec
import Test.Hspec.Runner

main :: IO ()
main = withUtf8Encoding $ hspecWith defaultConfig Spec.spec
