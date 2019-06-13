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
import Test.Hspec
    ( Spec, describe, expectationFailure, it )

spec :: Spec
spec = describe "cardano-wallet-launcher" $ do
    it "Can start launcher against testnet" $ do
        let cardanoWalletLauncher = Command
                "cardano-wallet-launcher"
                [ "--http-bridge-port", "8080"
                ] (return ())
                Inherit
        handle <- async $ void $ launch [cardanoWalletLauncher]
        let fiveSeconds = 5000000
        winner <- race (threadDelay fiveSeconds) (wait handle)
        case winner of
            Left _ -> do
                cancel handle
                threadDelay 1000000
            Right _ ->
                expectationFailure
                    "cardano-wallet-launcher isn't supposed to terminate. \
                    \Something went wrong."
