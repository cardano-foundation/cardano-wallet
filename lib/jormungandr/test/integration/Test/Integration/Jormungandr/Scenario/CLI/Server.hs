{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Jormungandr.Scenario.CLI.Server
    ( spec
    ) where

import Prelude

import Cardano.BM.Trace
    ( nullTracer )
import Cardano.CLI
    ( Port (..) )
import Cardano.Faucet
    ( getBlock0HText )
import Cardano.Launcher
    ( Command (..), StdStream (..), withBackendProcess )
import Control.Concurrent
    ( threadDelay )
import Control.Exception
    ( finally )
import Control.Monad
    ( forM_, void )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Product.Typed
    ( typed )
import System.Command
    ( Exit (..), Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import System.FilePath
    ( (</>) )
import System.IO
    ( Handle, hClose )
import System.IO.Temp
    ( withSystemTempDirectory, withSystemTempFile )
import System.Process
    ( createProcess
    , proc
    , terminateProcess
    , waitForProcess
    , withCreateProcess
    )
import Test.Hspec
    ( SpecWith, describe, it, runIO )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain, shouldNotContain, shouldReturn )
import Test.Integration.Framework.DSL
    ( Context (..)
    , KnownCommand (..)
    , cardanoWalletCLI
    , expectPathEventuallyExist
    , proc'
    )
import Test.Utils.Ports
    ( findPort )

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

spec :: forall t. KnownCommand t => SpecWith (Context t)
spec = do
    block0H <- runIO $ T.unpack <$> getBlock0HText
    describe "SERVER - cardano-wallet serve [SERIAL]" $ do
        it "SERVER - Can start cardano-wallet serve --database" $ \_ -> do
            withTempDir $ \d -> do
                let db = d </> "db-file"
                let args =
                        [ "serve"
                        , "--database", db
                        , "--genesis-block-hash", block0H
                        ]
                let process = proc' (commandName @t) args
                withCreateProcess process $ \_ _ _ ph -> do
                    expectPathEventuallyExist db
                  `finally` do
                    terminateProcess ph
            threadDelay oneSecond

    describe "DaedalusIPC [SERIAL]" $ do
        let scriptPath = "test" </> "integration" </> "js" </> "mock-daedalus.js"
        let mockProc testCase nodePort extra = proc "node" $
                [ scriptPath
                , testCase
                , commandName @t
                , "serve"
                , "--node-port"
                , show nodePort
                , "--genesis-block-hash"
                , block0H
                ] ++ extra

        it "Should reply with the port --random" $ \ctx -> do
            let script = mockProc "test1" (ctx ^. typed @(Port "node"))
                    ["--random-port"]
            (_, _, _, ph) <- createProcess script
            waitForProcess ph `shouldReturn` ExitSuccess

        it "Should reply with the port --port" $ \ctx -> do
            walletPort <- findPort
            let script = mockProc "test1" (ctx ^. typed @(Port "node"))
                    ["--port", show walletPort]
            (_, _, _, ph) <- createProcess script
            waitForProcess ph `shouldReturn` ExitSuccess

        it "Regression test for #1036" $ \ctx -> do
            let script = mockProc "test2" (ctx ^. typed @(Port "node"))
                    ["--random-port"]
            (_, _, _, ph) <- createProcess script
            waitForProcess ph `shouldReturn` ExitSuccess

    describe "LOGGING - cardano-wallet serve logging [SERIAL]" $ do
        it "LOGGING - Serve --quiet logs Error only" $ \ctx -> do
            withTempFile $ \logs hLogs -> do
                let cmd = Command
                        (commandName @t)
                        ["serve"
                        , "--node-port", show (ctx ^. typed @(Port "node"))
                        , "--random-port"
                        , "--quiet"
                        , "--genesis-block-hash", block0H
                        ]
                        (pure ())
                        (UseHandle hLogs)
                void $ withBackendProcess nullTracer cmd $ do
                    threadDelay (10 * oneSecond)
                hClose hLogs
                TIO.readFile logs `shouldReturn` mempty

        it "LOGGING - Serve default logs Info" $ \ctx -> do
            withTempFile $ \logs hLogs -> do
                let cmd = Command
                        (commandName @t)
                        ["serve"
                        , "--node-port", show (ctx ^. typed @(Port "node"))
                        , "--random-port"
                        , "--genesis-block-hash", block0H
                        ]
                        (pure ())
                        (UseHandle hLogs)
                void $ withBackendProcess nullTracer cmd $ do
                    threadDelay (10 * oneSecond)
                hClose hLogs
                logged <- T.unpack <$> TIO.readFile logs
                logged `shouldNotContain` "TRACE"
                logged `shouldNotContain` "DEBUG"

        it "LOGGING - Serve shuts down logging correctly" $ \ctx -> do
            withTempFile $ \logs hLogs -> do
                let cmd = Command
                        (commandName @t)
                        ["serve"
                        , "--database", "/does-not-exist"
                        , "--node-port", show (ctx ^. typed @(Port "node"))
                        , "--random-port"
                        , "--verbose"
                        , "--genesis-block-hash", block0H
                        ]
                        (pure ())
                        (UseHandle hLogs)
                void $ withBackendProcess nullTracer cmd $ do
                    threadDelay (10 * oneSecond)
                hClose hLogs
                logged <- T.unpack <$> TIO.readFile logs
                putStrLn logged
                logged `shouldContain` "Logging shutdown"

        describe "LOGGING - Exits nicely on wrong genesis hash" $  do
            let hashes =
                    [ replicate 40 '1'
                    , replicate 38 '1'
                    , replicate 42 '1'
                    ]
            forM_ hashes $ \hash -> it hash $ \ctx -> do
                let args =
                        ["serve"
                        , "--node-port"
                        , show (ctx ^. typed @(Port "node"))
                        , "--random-port"
                        , "--genesis-block-hash"
                        , hash
                        ]
                (Exit c, Stdout o, Stderr e) <- cardanoWalletCLI @t args
                c `shouldBe` ExitFailure 1
                o `shouldBe` mempty
                e `shouldContain`
                    "Invalid genesis hash: expecting a hex-encoded \
                    \value that is 32 bytes in length"

        it "LOGGING - Non hex-encoded genesis hash shows error" $ \_ -> do
            let args =
                    ["serve"
                    , "--genesis-block-hash"
                    , replicate 37 '1'
                    ]
            (Exit c, Stdout o, Stderr e) <- cardanoWalletCLI @t args
            c `shouldBe` ExitFailure 1
            o `shouldBe` mempty
            e `shouldContain`
                "Invalid genesis hash: expecting a hex-encoded \
                \value that is 32 bytes in length"

oneSecond :: Int
oneSecond = 1000000

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "integration-state"

withTempFile :: (FilePath -> Handle -> IO a) -> IO a
withTempFile = withSystemTempFile "temp-file"
