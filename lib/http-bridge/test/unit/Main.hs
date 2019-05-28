module Main where

import Prelude

import Cardano.Wallet.HttpBridge.Environment
    ( network )
import Test.Hspec.Runner
    ( hspecWith )

import qualified Spec
import qualified Test.Hspec.Runner as Hspec

main :: IO ()
main = do
    network `seq` (return ())
    hspecWith Hspec.defaultConfig Spec.spec
