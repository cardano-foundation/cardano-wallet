module Main where

import Cardano.Startup
  ( withUtf8Encoding
  )
import Spec qualified
import Test.Hspec.Extra
  ( hspecMain
  )
import Prelude

main :: IO ()
main = withUtf8Encoding $ hspecMain Spec.spec
