{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.HttpBridge.Scenario.CLI.Server
    ( spec
    , specNoBackend
    ) where

import Prelude

import Control.Concurrent
    ( threadDelay )
import System.IO.Temp
    ( withSystemTempDirectory )
import System.Process
    ( terminateProcess, withCreateProcess )
import Test.Hspec
    ( Spec, SpecWith, describe, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe )
import Test.Integration.Framework.DSL
    ( Context (..)
    , KnownCommand (..)
    , collectStreams
    , expectPathEventuallyExist
    , proc'
    , shouldContainT
    , shouldNotContainT
    )
import Test.Integration.Framework.TestData
    ( versionLine )

spec :: forall t. KnownCommand t => SpecWith (Context t)
spec = do
    describe "SERVER - cardano-wallet serve" $ do
        it "SERVER - Can start cardano-wallet serve --database" $ \_ -> do
            withTempDir $ \d -> do
                let db = d ++ "/db-file"
                let args = ["serve", "--database", db]
                let process = proc' (commandName @t) args
                withCreateProcess process $ \_ _ _ ph -> do
                    expectPathEventuallyExist db
                    terminateProcess ph
            threadDelay oneSecond

    describe "LOGGING - cardano-wallet serve logging" $ do
        it "LOGGING - Launch can log --verbose" $ \_ -> do
            let args = ["serve", "--random-port", "--verbose"]
            let process = proc' (commandName @t) args
            (out, _) <- collectStreams (20, 0) process
            out `shouldContainT` versionLine
            out `shouldContainT` "Debug"
            out `shouldContainT` "Info"
            out `shouldContainT` "Notice"

        it "LOGGING - Serve --quiet logs Error only" $ \_ -> do
            let args = ["serve", "--random-port", "--quiet"]
            let process = proc' (commandName @t) args
            (out, err) <- collectStreams (10, 10) process
            out `shouldBe` mempty
            err `shouldBe` mempty

        it "LOGGING - Serve default logs Info" $ \_ -> do
            let args = ["serve", "--random-port"]
            let process = proc' (commandName @t) args
            (out, _) <- collectStreams (5, 0) process
            out `shouldNotContainT` "Debug"
            out `shouldContainT` versionLine
            out `shouldContainT` "Info"
            out `shouldContainT` "Notice"

specNoBackend :: forall t. KnownCommand t => Spec
specNoBackend = do
    it "TIMEOUT - Times out gracefully after 60 seconds" $ do
        let args = ["serve"]
        let process = proc' (commandName @t) args
        (out, err) <- collectStreams (61, 61) process
        out `shouldContainT` "Waited too long for http-bridge to become available.\
            \ Giving up!"
        err `shouldContainT` "Hint (1): If you're launching the wallet server\
            \ on your own, double-check that http-bridge is up-and-running and\
            \ listening on the same port given to '--node-port' (i.e. tcp/8080)."
        err `shouldContainT` "Hint (2): Should you be starting from scratch,\
            \ make sure to have a good-enough network connection to synchronize\
            \ the first blocks in a timely manner."

oneSecond :: Int
oneSecond = 1000000

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "integration-state"
