module Main where

import Main.Utf8
  ( withUtf8
  )
import Network.Wai.Middleware.LoggingSpec qualified as LoggingSpec
import Test.Hspec.Extra
  ( hspecMain
  )
import Prelude

main :: IO ()
main = withUtf8 $ hspecMain LoggingSpec.spec
