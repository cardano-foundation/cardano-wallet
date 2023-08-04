module Main where

import Main.Utf8
  ( withUtf8
  )
import qualified Spec
import Test.Hspec.Extra
  ( hspecMain
  )
import Prelude

main :: IO ()
main = withUtf8 $ hspecMain Spec.spec
