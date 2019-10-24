module Main where

import Cardano.Launcher
    ( setUtf8Encoding )
import Prelude
import qualified Spec
import Test.Hspec.Runner

main :: IO ()
main = do
    setUtf8Encoding
    hspecWith defaultConfig Spec.spec
