module Cardano.LauncherSpec
    ( spec
    ) where

import Prelude

import Cardano.Launcher
    ( Command (..), StdStream (..), launch )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( async, cancel, race, wait )
import Control.Monad
    ( void )
import System.Directory
    ( removePathForcibly )
import Test.Hspec
    ( Spec, describe, expectationFailure, it )

spec :: Spec
spec = describe "cardano-wallet-launcher" $ do
    it "Can start launcher against testnet" $ do
        removePathForcibly "/tmp/cardano-wallet-jormungandr"
        let jormungandrLauncher = Command
                "jormungandr"
                [ "--genesis-block", dir ++ "/block-0.bin"
                , "--config", dir ++ "/config.yaml"
                , "--secret", dir ++ "/secret.yaml"
                ] (return ())
                Inherit
        handle <- async $ void $ launch [jormungandrLauncher]
        let fiveSeconds = 5000000
        winner <- race (threadDelay fiveSeconds) (wait handle)
        case winner of
            Left _ ->
                cancel handle
            Right _ ->
                expectationFailure
                    "jormungandr isn't supposed to terminate. \
                    \Something went wrong."
  where
    dir = "test/data/jormungandr"
