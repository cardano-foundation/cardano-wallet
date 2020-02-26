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
import Cardano.Launcher
    ( Command (..)
    , ProcessHasExited (..)
    , StdStream (..)
    , withBackendProcess
    , withBackendProcessHandle
    )
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
import Data.Text
    ( Text )
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
    ( shouldBe, shouldContain, shouldNotBe, shouldReturn )
import Test.Integration.Framework.DSL
    ( Context (..)
    , KnownCommand (..)
    , cardanoWalletCLI
    , expectPathEventuallyExist
    , proc'
    )
import Test.Integration.Jcli
    ( argHex, getBlock0H )
import Test.Utils.Ports
    ( findPort )

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

spec :: forall t. KnownCommand t => SpecWith (Context t)
spec = describe "PATATE" $ do
    block0H <- runIO $ argHex <$> getBlock0H
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
                        Inherit
                        (UseHandle hLogs)
                void $ withBackendProcess nullTracer cmd $ do
                    threadDelay (10 * oneSecond)
                hClose hLogs
                logged <- T.lines <$> TIO.readFile logs
                let loggedNotMain = grepNot "cardano-wallet.main:" logged
                grep "Debug" loggedNotMain `shouldBe` []
                grep "Info" loggedNotMain `shouldNotBe` []

        it "LOGGING - Serve debug logs for one component" $ \ctx -> do
            withTempFile $ \logs hLogs -> do
                let cmd = Command
                        (commandName @t)
                        ["serve"
                        , "--node-port", show (ctx ^. typed @(Port "node"))
                        , "--random-port"
                        , "--genesis-block-hash", block0H
                        , "--trace-pools-db", "debug"
                        ]
                        (pure ())
                        Inherit
                        (UseHandle hLogs)
                void $ withBackendProcess nullTracer cmd $ do
                    threadDelay (5 * oneSecond)
                hClose hLogs
                logged <- T.lines <$> TIO.readFile logs
                let poolsDebugLogs = grep "cardano-wallet.pools-db:Debug" logged
                let netDebugLogs = grep "cardano-wallet.network:Debug" logged
                length poolsDebugLogs `shouldNotBe` 0
                length netDebugLogs `shouldBe` 0

        it "LOGGING - Serve disable logs for one component" $ \ctx -> do
            withTempFile $ \logs hLogs -> do
                let cmd = Command
                        (commandName @t)
                        ["serve"
                        , "--node-port", show (ctx ^. typed @(Port "node"))
                        , "--random-port"
                        , "--genesis-block-hash", block0H
                        , "--trace-network", "off"
                        ]
                        (pure ())
                        Inherit
                        (UseHandle hLogs)
                void $ withBackendProcess nullTracer cmd $ do
                    threadDelay (5 * oneSecond)
                hClose hLogs
                logged <- T.lines <$> TIO.readFile logs
                let countLogs comp = length $
                        filter (T.isInfixOf $ "cardano-wallet." <> comp) logged
                countLogs "network" `shouldBe` 0
                countLogs "application" `shouldNotBe` 0

        it "LOGGING - Serve shuts down logging correctly" $ \ctx -> do
            withTempFile $ \logs hLogs -> do
                -- This starts the server with a database directory that should
                -- fail to create, thus causing the program to exit.
                -- On Linux/macOS, the user should not have permission to create
                -- a directory in /.
                -- On Windows, * is an invalid filename character.
                let dirShouldFailToCreate = "/*"
                let cmd = Command
                        (commandName @t)
                        ["serve"
                        , "--database", dirShouldFailToCreate
                        , "--node-port", show (ctx ^. typed @(Port "node"))
                        , "--random-port"
                        , "--genesis-block-hash", block0H
                        ]
                        (pure ())
                        Inherit
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

    describe "SERVER - Clean shutdown" $ do
        it "SERVER - shuts down on command" $ \ctx -> do
            logged <- withLogCollection $ \stream -> do
                let cmd = Command
                        (commandName @t)
                        ["serve"
                        , "--node-port", show (ctx ^. typed @(Port "node"))
                        , "--random-port"
                        , "--genesis-block-hash", block0H
                        ]
                        (pure ())
                        CreatePipe
                        stream

                res <- withBackendProcessHandle nullTracer cmd $ \(Just hStdin) _ -> do
                    threadDelay oneSecond
                    hClose hStdin
                    threadDelay oneSecond -- give handler a chance to run

                res `shouldBe` Left (ProcessHasExited "cardano-wallet-jormungandr" ExitSuccess)

            let enabledLogs = grep "shutdown handler is enabled" logged
            let shutdownLogs = grep "Starting clean shutdown" logged
            length enabledLogs `shouldBe` 1
            length shutdownLogs `shouldBe` 1

oneSecond :: Int
oneSecond = 1000000

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "integration-state"

withTempFile :: (FilePath -> Handle -> IO a) -> IO a
withTempFile = withSystemTempFile "temp-file"

withLogCollection :: (StdStream -> IO a) -> IO [Text]
withLogCollection action = withTempFile $ \logs hLogs -> do
    _ <- action (UseHandle hLogs)
    hClose hLogs
    T.lines <$> TIO.readFile logs

grep :: Text -> [Text] -> [Text]
grep str = filter (T.isInfixOf str)

grepNot :: Text -> [Text] -> [Text]
grepNot str = filter (not . T.isInfixOf str)
