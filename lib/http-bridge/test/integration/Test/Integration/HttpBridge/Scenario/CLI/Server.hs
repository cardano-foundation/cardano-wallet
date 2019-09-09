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
import Control.Exception
    ( finally )
import Control.Monad
    ( forM_ )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import System.Command
    ( Exit (..), Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import System.IO.Temp
    ( withSystemTempDirectory )
import System.Process
    ( createProcess
    , proc
    , terminateProcess
    , waitForProcess
    , withCreateProcess
    )
import Test.Hspec
    ( Spec, SpecWith, describe, it, pendingWith )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain, shouldReturn )
import Test.Integration.Framework.DSL
    ( Context (..)
    , KnownCommand (..)
    , cardanoWalletCLI
    , collectStreams
    , expectPathEventuallyExist
    , jormungandrBaseUrl
    , proc'
    , shouldContainT
    , shouldNotContainT
    )
import Test.Integration.Framework.TestData
    ( versionLine )
import Test.Utils.Ports
    ( findPort )

import qualified Data.Text as T

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
                  `finally` do
                    terminateProcess ph
            threadDelay oneSecond

        it "SERVER - Stops gracefully on wrong network connection" $ \ctx -> do
            let faultyNetwork = "mainnet"
            let args = [ "serve", "--network", faultyNetwork
                       , "--node-port", T.unpack (ctx ^. jormungandrBaseUrl) ]
            (Exit c, Stdout out, Stderr err) <- cardanoWalletCLI @t args
            out `shouldContain` "The node backend is not running on the\
                \ \"" ++ faultyNetwork ++ "\" network. Please start the\
                \ wallet server and the node backend on the same network.\
                \ Exiting now."
            err `shouldBe` mempty
            c `shouldBe` ExitFailure 1

    describe "DaedalusIPC" $ do
        let defaultArgs nodePort =
                [ commandName @t, "serve", "--node-port", T.unpack nodePort ]
        let tests =
                [ const ["--random-port"]
                , \fixedPort -> ["--port", fixedPort]
                ]
        forM_ tests $ \args -> do
            let title = "should reply with the port when asked "
                    <> show (args "FIXED")
            it title $ \ctx -> do
                fixedPort <- findPort
                let filepath = "test/integration/js/mock-daedalus.js"
                let scriptArgs = defaultArgs (ctx ^. jormungandrBaseUrl)
                        ++ args (show fixedPort)
                (_, _, _, ph) <- createProcess (proc filepath scriptArgs)
                waitForProcess ph `shouldReturn` ExitSuccess

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
            pendingWith "The assertion in this test case is wrong."
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
        let args = ["serve", "--random-port"]
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
