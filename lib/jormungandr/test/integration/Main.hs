module Main where

import Prelude

import Cardano.Environment.Jormungandr
    ( Network (..), network )

import Test.Hspec
    ( describe, hspec )

import qualified Cardano.LauncherSpec as Launcher

main :: IO ()
main = do
    case network of
        Testnet ->
            return ()
        _ ->
            fail $ "unsupported integration environment: " <> show network

    hspec $ do
        describe "Cardano.LauncherSpec" Launcher.spec
