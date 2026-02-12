module Main where

import Main.Utf8
    ( withUtf8
    )
import Test.Hspec.Extra
    ( hspecMain
    )
import Prelude

import qualified Spec

main :: IO ()
main = withUtf8 $ hspecMain Spec.spec
