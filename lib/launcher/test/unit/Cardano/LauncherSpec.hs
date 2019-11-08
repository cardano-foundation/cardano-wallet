{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.LauncherSpec
    ( spec
    ) where

import Prelude

import Cardano.BM.Trace
    ( Trace, nullTracer )
import Cardano.Launcher
    ( Command (..)
    , LauncherLog
    , ProcessHasExited (..)
    , StdStream (..)
    , withBackendProcessHandle
    )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( async, race_, waitAnyCancel )
import Control.Concurrent.MVar
    ( modifyMVar_
    , newEmptyMVar
    , newMVar
    , putMVar
    , readMVar
    , takeMVar
    , tryReadMVar
    )
import Control.Monad
    ( forever )
import Control.Retry
    ( constantDelay, limitRetriesByCumulativeDelay, recoverAll )
import Data.Maybe
    ( isJust )
import Data.Text
    ( Text )
import Fmt
    ( pretty )
import System.Exit
    ( ExitCode (..) )
import System.Info
    ( os )
import System.Process
    ( ProcessHandle, getProcessExitCode )
import Test.Hspec
    ( Spec, it, shouldBe, shouldContain, shouldReturn, shouldSatisfy )

{-# ANN spec ("HLint: ignore Use head" :: String) #-}
spec :: Spec
spec = do
    it "Buildable Command" $ do
        let command = Command "server"
                [ "start"
                , "--port", "8080"
                , "--template", "mainnet"
                ] (pure ())
                Inherit

        pretty @_ @Text command `shouldBe`
            "server\n\
            \     start\n\
            \     --port 8080\n\
            \     --template mainnet\n"

    it "1st process exits with 0, others are cancelled" $ do
        let commands =
              [ mockCommand True (pure ())
              , foreverCommand
              ]
        (phs, ProcessHasExited name code) <- launch nullTracer commands
        name `shouldBe` cmdName (commands !! 0)
        code `shouldBe` ExitSuccess
        assertProcessesExited phs

    it "2nd process exits with 0, others are cancelled" $ do
        let commands =
              [ foreverCommand
              , mockCommand True (pure ())
              ]
        (phs, ProcessHasExited name code) <- launch nullTracer commands
        name `shouldBe` cmdName (commands !! 1)
        code `shouldBe` ExitSuccess
        assertProcessesExited phs

    it "1st process exits with 3, others are cancelled" $ do
        let commands =
              [ mockCommand False (pure ())
              , foreverCommand
              ]
        (phs, ProcessHasExited name code) <- launch nullTracer commands
        name `shouldBe` cmdName (commands !! 0)
        code `shouldBe` (ExitFailure 3)
        assertProcessesExited phs

    it "2nd process exits with 3, others are cancelled" $ do
        let commands =
              [ foreverCommand
              , mockCommand False (pure ())
              ]
        (phs, ProcessHasExited name code) <- launch nullTracer commands
        name `shouldBe` cmdName (commands !! 1)
        code `shouldBe` (ExitFailure 3)
        assertProcessesExited phs

    it "Process executes a command before they start" $ do
        mvar <- newEmptyMVar
        let before = putMVar mvar "executed"
        let commands =
                [ mockCommand True before
                ]
        (phs, ProcessHasExited _ code) <- launch nullTracer commands
        code `shouldBe` ExitSuccess
        tryReadMVar mvar `shouldReturn` (Just @String "executed")
        assertProcessesExited phs

    it "Handles command not found" $ do
        let commands =
                [ Command "foobar" [] (pure ()) Inherit
                ]
        (phs, ProcessDidNotStart name _exc) <- launch nullTracer commands
        name `shouldBe` "foobar"
        assertProcessesExited phs

    it "Backend process is terminated when Async thread is cancelled" $ do
        mvar <- newEmptyMVar
        let backend =  withBackendProcessHandle nullTracer foreverCommand $ \ph -> do
                putMVar mvar ph
                forever $ threadDelay maxBound
        race_ backend (threadDelay 1000000)
        ph <- takeMVar mvar
        assertProcessesExited [ph]

    it "Sanity check System.Info.os" $
        ["linux", "darwin", "mingw32"] `shouldContain` [os]

-- | A command that will run for a short time.
mockCommand :: Bool -> IO () -> Command
mockCommand success before
    | isWindows && success =
        Command "TIMEOUT" ["1"] before Inherit
    | isWindows && not success =
        Command "CHOICE" ["/T", "1", "/C", "wat", "/D", "t"] before Inherit
    | otherwise =
        Command "sh" ["-c", "sleep 1; exit " ++ show exitStatus] before Inherit
        where exitStatus = if success then 0 else 3 :: Int

-- | A command that will run for longer than the other commands.
foreverCommand :: Command
foreverCommand
    | isWindows = Command "TIMEOUT" ["30"] (pure ()) Inherit
    | otherwise = Command "sleep" ["30"] (pure ()) Inherit

isWindows :: Bool
isWindows = os == "mingw32"

-- | Run a bunch of command in separate processes. Note that, this operation is
-- blocking and will throw when one of the given commands terminates.
-- It records the PID of all processes which started (in undefined order).
launch :: Trace IO LauncherLog -> [Command] -> IO ([ProcessHandle], ProcessHasExited)
launch tr cmds = do
    phsVar <- newMVar []
    let
        waitForOthers ph = do
            modifyMVar_ phsVar (pure . (ph:))
            forever $ threadDelay maxBound
        start = async . flip (withBackendProcessHandle tr) waitForOthers

    mapM start cmds >>= waitAnyCancel >>= \case
        (_, Left e) -> do
            phs <- readMVar phsVar
            return (phs, e)
        (_, Right _) -> error $
                "Unreachable. Supervising threads should never finish. " <>
                "They should stay running or throw @ProcessHasExited@."

-- | Check that all processes eventually exit somehow. This will wait for up to
-- 10 seconds for that to happen.
assertProcessesExited :: [ProcessHandle] -> IO ()
assertProcessesExited phs = recoverAll policy test
  where
    policy = limitRetriesByCumulativeDelay 10000 (constantDelay 100)
    test _ = do
        statuses <- mapM getProcessExitCode phs
        statuses `shouldSatisfy` all isJust
