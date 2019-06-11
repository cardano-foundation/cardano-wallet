module Main where

import Prelude

import Test.Hspec
    ( describe, hspec )

import qualified Cardano.LauncherSpec as Launcher
import qualified Cardano.Wallet.Jormungandr.NetworkSpec as Network

main :: IO ()
main = hspec $ do
    describe "Cardano.LauncherSpec" Launcher.spec
    describe "Cardano.Wallet.NetworkSpec" Network.spec
