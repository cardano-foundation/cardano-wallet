{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.HttpBridge.Scenario.CLI.Server
    ( spec
    , specNoBackend
    ) where

import Prelude

import Cardano.CLI
    ( Port )
import Control.Concurrent
    ( threadDelay )
import Control.Exception
    ( finally )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Product.Typed
    ( typed )
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
    ( Spec, SpecWith, describe, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain, shouldReturn )
import Test.Integration.Framework.DSL
    ( Context (..)
    , KnownCommand (..)
    , cardanoWalletCLI
    , collectStreams
    , expectPathEventuallyExist
    , proc'
    , shouldContainT
    , shouldNotContainT
    )
import Test.Integration.Framework.TestData
    ( versionLine )
import Test.Utils.Ports
    ( findPort )

spec :: forall t. KnownCommand t => SpecWith (Context t)
spec = do
    describe "SERVER - cardano-wallet serve" $ do
        it "SERVER - Can start cardano-wallet serve --database" $ \ctx -> do
            withTempDir $ \d -> do
                let db = d ++ "/db-file"
                let args = ["serve", "--database", db
                           , "--node-port", show (ctx ^. typed @(Port "node"))
                           ]
                let process = proc' (commandName @t) args
                withCreateProcess process $ \_ _ _ ph -> do
                    expectPathEventuallyExist db
                  `finally` do
                    terminateProcess ph
            threadDelay oneSecond

        it "SERVER - Stops gracefully on wrong network connection" $ \ctx -> do
            let faultyNetwork = "mainnet"
            let args =
                    [ "serve", "--network", faultyNetwork
                    , "--node-port", show (ctx ^. typed @(Port "node"))
                    ]
            (Exit c, Stdout out, Stderr err) <- cardanoWalletCLI @t args
            out `shouldContain` "The node backend is not running on the\
                \ \"" ++ faultyNetwork ++ "\" network. Please start the\
                \ wallet server and the node backend on the same network.\
                \ Exiting now."
            err `shouldBe` mempty
            c `shouldBe` ExitFailure 1

    describe "DaedalusIPC" $ do
        let defaultArgs nodePort =
                [ commandName @t
                , "serve"
                , "--node-port"
                , show nodePort
                ]

        let filepath = "test/integration/js/mock-daedalus.js"

        it "Should reply with the port --random" $ \ctx -> do
            let scriptArgs = defaultArgs (ctx ^. typed @(Port "node"))
                    ++ ["--random-port"]
            (_, _, _, ph) <- createProcess (proc filepath scriptArgs)
            waitForProcess ph `shouldReturn` ExitSuccess

        it "Should reply with the port --random" $ \ctx -> do
            walletPort <- findPort
            let scriptArgs = defaultArgs (ctx ^. typed @(Port "node"))
                    ++ ["--port", show walletPort]
            (_, _, _, ph) <- createProcess (proc filepath scriptArgs)
            waitForProcess ph `shouldReturn` ExitSuccess

    describe "LOGGING - cardano-wallet serve logging" $ do
        it "LOGGING - Launch can log --verbose" $ \ctx -> do
            let args = ["serve", "--random-port", "--verbose"
                       , "--node-port", show (ctx ^. typed @(Port "node"))]
            let process = proc' (commandName @t) args
            (out, _) <- collectStreams (20, 0) process
            out `shouldContainT` versionLine
            out `shouldContainT` "Debug"
            out `shouldContainT` "Info"
            out `shouldContainT` "Notice"

        it "LOGGING - Serve --quiet logs Error only" $ \ctx -> do
            let args = ["serve", "--random-port", "--quiet"
                       , "--node-port", show (ctx ^. typed @(Port "node"))]
            let process = proc' (commandName @t) args
            (out, err) <- collectStreams (10, 10) process
            out `shouldBe` mempty
            err `shouldBe` mempty

        it "LOGGING - Serve default logs Info" $ \ctx -> do
            let args = ["serve", "--random-port"
                       , "--node-port", show (ctx ^. typed @(Port "node"))]
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
        (out, _) <- collectStreams (61, 61) process
        out `shouldContainT` "Waited too long for http-bridge to become available.\
            \ Giving up!"

oneSecond :: Int
oneSecond = 1000000

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "integration-state"
