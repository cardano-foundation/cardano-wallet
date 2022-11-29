module Main where

import Prelude

import Cardano.Startup
    ( withUtf8Encoding )
import Test.Hspec.Extra
    ( hspecMain )

import qualified Spec

main :: IO ()
main = withUtf8Encoding $ hspecMain Spec.spec
