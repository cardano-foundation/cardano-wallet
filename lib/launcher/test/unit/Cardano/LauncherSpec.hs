{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.LauncherSpec
    ( spec
    ) where

import Prelude

import Cardano.BM.Configuration.Model
    ( setMinSeverity )
import Cardano.BM.Configuration.Static
    ( defaultConfigStdout )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Setup
    ( setupTrace_, shutdown )
import Cardano.BM.Trace
    ( Trace, logDebug )
import Cardano.Launcher
    ( Command (..)
    , LauncherLog
    , ProcessHasExited (..)
    , StdStream (..)
    , transformLauncherTrace
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
import Control.Exception
    ( IOException, bracket, handle )
import Control.Monad
    ( forever )
import Control.Retry
    ( constantDelay, limitRetriesByCumulativeDelay, recoverAll )
import Data.Maybe
    ( isJust )
import Data.Text
    ( Text )
import Data.Time.Clock
    ( diffUTCTime, getCurrentTime )
import Fmt
    ( pretty )
import System.Exit
    ( ExitCode (..) )
import System.Info
    ( os )
import System.Process
    ( ProcessHandle, getProcessExitCode, readProcessWithExitCode )
import Test.Hspec
    ( Spec
    , beforeAll
    , it
    , shouldBe
    , shouldContain
    , shouldReturn
    , shouldSatisfy
    )
import Test.Utils.Windows
    ( isWindows )

{-# ANN spec ("HLint: ignore Use head" :: String) #-}
spec :: Spec
spec = beforeAll setupMockCommands $ do
    it "Buildable Command" $ \MockCommands{..} -> do
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

    it "1st process exits with 0, others are cancelled" $ \MockCommands{..} -> withTestLogging $ \tr -> do
        let commands =
              [ mockCommand True (pure ())
              , foreverCommand
              ]
        (phs, ProcessHasExited name code) <- launch tr commands
        name `shouldBe` cmdName (commands !! 0)
        code `shouldBe` ExitSuccess
        assertProcessesExited phs

    it "2nd process exits with 0, others are cancelled" $ \MockCommands{..} -> withTestLogging $ \tr -> do
        let commands =
              [ foreverCommand
              , mockCommand True (pure ())
              ]
        (phs, ProcessHasExited name code) <- launch tr commands
        name `shouldBe` cmdName (commands !! 1)
        code `shouldBe` ExitSuccess
        assertProcessesExited phs

    it "1st process exits with 1, others are cancelled" $ \MockCommands{..} -> withTestLogging $ \tr -> do
        let commands =
              [ mockCommand False (pure ())
              , foreverCommand
              ]
        (phs, ProcessHasExited name code) <- launch tr commands
        name `shouldBe` cmdName (commands !! 0)
        code `shouldBe` (ExitFailure 1)
        assertProcessesExited phs

    it "2nd process exits with 1, others are cancelled" $ \MockCommands{..} -> withTestLogging $ \tr -> do
        let commands =
              [ foreverCommand
              , mockCommand False (pure ())
              ]
        (phs, ProcessHasExited name code) <- launch tr commands
        name `shouldBe` cmdName (commands !! 1)
        code `shouldBe` (ExitFailure 1)
        assertProcessesExited phs

    it "Process executes a command before they start" $ \MockCommands{..} -> withTestLogging $ \tr -> do
        mvar <- newEmptyMVar
        let before = putMVar mvar "executed"
        let commands =
                [ mockCommand True before
                ]
        (phs, ProcessHasExited _ code) <- launch tr commands
        code `shouldBe` ExitSuccess
        tryReadMVar mvar `shouldReturn` (Just @String "executed")
        assertProcessesExited phs

    it "Handles command not found" $ \MockCommands{..} -> withTestLogging $ \tr -> do
        let commands =
                [ Command "foobar" [] (pure ()) Inherit
                ]
        (phs, ProcessDidNotStart name _exc) <- launch tr commands
        name `shouldBe` "foobar"
        assertProcessesExited phs

    it "Backend process is terminated when Async thread is cancelled" $ \MockCommands{..} -> withTestLogging $ \tr -> do
        mvar <- newEmptyMVar
        let backend = withBackendProcessHandle tr foreverCommand $ \ph -> do
                putMVar mvar ph
                forever $ threadDelay maxBound
        before <- getCurrentTime
        race_ backend (threadDelay 1000000)
        after <- getCurrentTime
        ph <- takeMVar mvar
        assertProcessesExited [ph]
        -- the total time taken should be about 1 second (the delay), definitely
        -- never more that 2 seconds.
        diffUTCTime after before `shouldSatisfy` (< 2)

    it "Sanity check System.Info.os" $ \_ ->
        ["linux", "darwin", "mingw32"] `shouldContain` [os]

data MockCommands = MockCommands
    { mockCommand :: Bool -> IO () -> Command
    -- ^ A command that will run for a short time.
    , foreverCommand :: Command
    -- ^ A command that will run for longer than the other commands.
    }

setupMockCommands :: IO MockCommands
setupMockCommands
    | isWindows = setupWin <$> getIsWine
    | otherwise = pure mockCommandsShell
  where
    mockCommandsShell = MockCommands
        { mockCommand = \success before ->
                let exitStatus = if success then 0 else 1 :: Int
                in Command "sh" ["-c", "sleep 1; exit " ++ show exitStatus] before Inherit
        , foreverCommand = Command "sleep" ["20"] (pure ()) Inherit
        }
    setupWin False = MockCommands
        { mockCommand = \success before -> if success
            then Command "TIMEOUT" ["1"] before Inherit
            else Command "CHOICE" ["/T", "1", "/C", "wat", "/D", "w"] before Inherit
        , foreverCommand = Command "TIMEOUT" ["20"] (pure ()) Inherit
        }
    setupWin True = MockCommands
        { mockCommand = \success before -> if success
                then Command "PING" ["127.0.0.1", "-n", "1", "-w", "1000"] before Inherit
                else Command "START" ["/wait", "xyzzy"] before Inherit
        , foreverCommand = Command "ping" ["127.0.0.1", "-n", "20", "-w", "1000"] (pure ()) Inherit
        }

-- | Use the presence of @winepath.exe@ to detect when running tests under Wine.
getIsWine :: IO Bool
getIsWine = handle (\(_ :: IOException) -> pure False) $ do
    (code, _, _) <- readProcessWithExitCode "winepath" ["--version"] mempty
    pure (code == ExitSuccess)

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

withTestLogging :: (Trace IO LauncherLog -> IO a) -> IO a
withTestLogging action =
    bracket before after (action . transformLauncherTrace . fst)
  where
    before = do
        cfg <- defaultConfigStdout
        setMinSeverity cfg Debug
        setupTrace_ cfg "tests"
    after (tr, sb) = do
        logDebug tr "Logging shutdown."
        shutdown sb
