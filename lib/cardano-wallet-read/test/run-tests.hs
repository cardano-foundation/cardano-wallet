module Main where

import Main.Utf8
    ( withUtf8
    )
import Test.Hspec
    ( hspec
    )
import Prelude

import Spec qualified

main :: IO ()
main = withUtf8 $ hspec Spec.spec
