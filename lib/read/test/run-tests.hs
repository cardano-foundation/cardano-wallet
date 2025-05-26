module Main where

import Prelude

import Main.Utf8
    ( withUtf8
    )
import Test.Hspec
    ( hspec
    )

import qualified Spec

main :: IO ()
main = withUtf8 $ hspec Spec.spec
