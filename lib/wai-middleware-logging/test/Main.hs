module Main where

import Prelude

import Main.Utf8
    ( withUtf8
    )
import Test.Hspec.Extra
    ( hspecMain
    )

import qualified Network.Wai.Middleware.LoggingSpec as LoggingSpec

main :: IO ()
main = withUtf8 $ hspecMain LoggingSpec.spec
