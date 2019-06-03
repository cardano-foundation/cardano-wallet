module Main where

import Prelude

import Test.Hspec
    ( describe, hspec )

import qualified Cardano.LauncherSpec as Launcher

main :: IO ()
main = hspec $ do
    describe "Cardano.LauncherSpec" Launcher.spec
