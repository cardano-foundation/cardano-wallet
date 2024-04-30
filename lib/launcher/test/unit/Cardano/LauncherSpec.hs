{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.LauncherSpec
    ( spec
    ) where

import Prelude

import Cardano.BM.Configuration.Model
    ( setMinSeverity
    )
import Cardano.BM.Configuration.Static
    ( defaultConfigStdout
    )
import Cardano.BM.Data.LogItem
    ( LOContent (LogMessage)
    , LogObject (..)
    , LoggerName
    , mkLOMeta
    )
import Cardano.BM.Data.Severity
    ( Severity (..)
    )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..)
    , HasSeverityAnnotation (..)
    )
import Cardano.BM.Setup
    ( setupTrace_
    , shutdown
    )
import Cardano.BM.Trace
    ( logDebug
    )
import Cardano.Launcher
    ( Command (..)
    , LauncherLog
    , ProcessHandles (..)
    , ProcessHasExited (..)
    , StdStream (..)
    , withBackendProcess
    )
import Control.Monad
    ( forever
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Retry
    ( constantDelay
    , limitRetriesByCumulativeDelay
    , recoverAll
    )
import Control.Tracer
    ( Tracer (..)
    , nullTracer
    , traceWith
    )
import Data.Maybe
    ( isJust
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText (..)
    )
import Data.Time.Clock
    ( diffUTCTime
    , getCurrentTime
    )
import Fmt
    ( pretty
    )
import System.Exit
    ( ExitCode (..)
    )
import System.Info
    ( os
    )
import Test.Hspec
    ( Spec
    , beforeAll
    , it
    , shouldBe
    , shouldContain
    , shouldReturn
    , shouldSatisfy
    )
import Test.Utils.Platform
    ( isWindows
    , pendingOnWine
    , skipOnWindows
    )
import UnliftIO.Async
    ( async
    , race_
    , waitAnyCancel
    )
import UnliftIO.Concurrent
    ( threadDelay
    )
import UnliftIO.Exception
    ( bracket
    , try
    )
import UnliftIO.MVar
    ( modifyMVar_
    , newEmptyMVar
    , newMVar
    , putMVar
    , readMVar
    , takeMVar
    , tryReadMVar
    )
import UnliftIO.Process
    ( ProcessHandle
    , getProcessExitCode
    )

{- HLINT ignore spec "Use head" -}

spec :: Spec
spec = beforeAll setupMockCommands $ do
    it "Buildable Command" $ \_ -> do
        let command = Command "server"
                [ "start"
                , "--port", "8080"
                , "--template", "mainnet"
                ] (pure ())
                Inherit
                Inherit

        pretty @_ @Text command `shouldBe`
            "server\n\
            \     start\n\
            \     --port 8080\n\
            \     --template mainnet\n"

    it "1st process exits with 0, others are cancelled" $ \MockCommands{..} -> withTestLogging $ \tr -> do
        pendingOnWine "SYSTEM32 commands not available under wine"
        let commands =
              [ mockCommand True (pure ())
              , foreverCommand
              ]
        (phs, ProcessHasExited name code) <- launch tr commands
        name `shouldBe` cmdName (commands !! 0)
        code `shouldBe` ExitSuccess
        assertProcessesExited phs

    it "2nd process exits with 0, others are cancelled" $ \MockCommands{..} -> withTestLogging $ \tr -> do
        pendingOnWine "SYSTEM32 commands not available under wine"
        let commands =
              [ foreverCommand
              , mockCommand True (pure ())
              ]
        (phs, ProcessHasExited name code) <- launch tr commands
        name `shouldBe` cmdName (commands !! 1)
        code `shouldBe` ExitSuccess
        assertProcessesExited phs

    it "1st process exits with 1, others are cancelled" $ \MockCommands{..} -> withTestLogging $ \tr -> do
        pendingOnWine "SYSTEM32 commands not available under wine"
        let commands =
              [ mockCommand False (pure ())
              , foreverCommand
              ]
        (phs, ProcessHasExited name code) <- launch tr commands
        name `shouldBe` cmdName (commands !! 0)
        code `shouldBe` (ExitFailure 1)
        assertProcessesExited phs

    it "2nd process exits with 1, others are cancelled" $ \MockCommands{..} -> withTestLogging $ \tr -> do
        pendingOnWine "SYSTEM32 commands not available under wine"
        let commands =
              [ foreverCommand
              , mockCommand False (pure ())
              ]
        (phs, ProcessHasExited name code) <- launch tr commands
        name `shouldBe` cmdName (commands !! 1)
        code `shouldBe` (ExitFailure 1)
        assertProcessesExited phs

    it "Process executes a command before they start" $ \MockCommands{..} -> withTestLogging $ \tr -> do
        pendingOnWine "SYSTEM32 commands not available under wine"
        mvar <- newEmptyMVar
        let before = putMVar mvar "executed"
        let commands =
                [ mockCommand True before
                ]
        (phs, ProcessHasExited _ code) <- launch tr commands
        code `shouldBe` ExitSuccess
        tryReadMVar mvar `shouldReturn` (Just @String "executed")
        assertProcessesExited phs

    it "Handles command not found" $ \_ -> withTestLogging $ \tr -> do
        let commands =
                [ Command "foobar" [] (pure ()) Inherit Inherit
                ]
        (phs, ProcessDidNotStart name _exc) <- launch tr commands
        name `shouldBe` "foobar"
        assertProcessesExited phs

    it "Backend process is terminated when Async thread is cancelled" $ \MockCommands{..} -> withTestLogging $ \tr -> do
        pendingOnWine "SYSTEM32 commands not available under wine"
        mvar <- newEmptyMVar
        let backend = withBackendProcess tr foreverCommand
                $ \(ProcessHandles _ _ _ ph) -> do
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

    it "Misbehaving backend process is killed when Async thread is cancelled" $ \_ -> withTestLogging $ \tr -> do
        skipOnWindows "Not applicable"
        mvar <- newEmptyMVar
        let backend = withBackendProcess tr unkillableCommand
                $ \(ProcessHandles _ _ _ ph) -> do
                    putMVar mvar ph
                    forever $ threadDelay maxBound
        before <- getCurrentTime
        race_ backend (threadDelay 1000000)
        after <- getCurrentTime
        ph <- takeMVar mvar
        assertProcessesExited [ph]
        -- the total time taken should be about 6 seconds
        -- (the delay plus kill timeout)
        diffUTCTime after before `shouldSatisfy` (\dt -> dt > 3 && dt < 7)

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
    | isWindows = pure mockCommandsWin
    | otherwise = pure mockCommandsShell
  where
    mockCommandsShell = MockCommands
        { mockCommand = \success before ->
                let exitStatus = if success then 0 else 1 :: Int
                in Command "sh" ["-c", "sleep 1; exit " ++ show exitStatus] before Inherit Inherit
        , foreverCommand = Command "sleep" ["20"] (pure ()) Inherit Inherit
        }
    mockCommandsWin = MockCommands
        { mockCommand = \success before -> if success
            then Command "TIMEOUT" ["1"] before Inherit Inherit
            else Command "CHOICE" ["/T", "1", "/C", "wat", "/D", "w"] before Inherit Inherit
        , foreverCommand = Command "TIMEOUT" ["20"] (pure ()) Inherit Inherit
        }

-- | A command that ignores SIGTERM (POSIX only)
unkillableCommand :: Command
unkillableCommand = Command "sh" ["-c", "trap \" \" TERM; sleep 20"] (pure ()) Inherit Inherit

-- | Run a bunch of command in separate processes. Note that, this operation is
-- blocking and will throw when one of the given commands terminates.
-- It records the PID of all processes which started (in undefined order).
launch :: Tracer IO LauncherLog -> [Command] -> IO ([ProcessHandle], ProcessHasExited)
launch tr cmds = do
    phsVar <- newMVar []
    let
        waitForOthers (ProcessHandles _ _ _ ph) = do
            modifyMVar_ phsVar (pure . (ph:))
            forever $ threadDelay maxBound
        start = async . try . flip (withBackendProcess tr) waitForOthers

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

withTestLogging :: (Tracer IO LauncherLog -> IO a) -> IO a
withTestLogging action =
    bracket before after (action . trMessageText . fst)
  where
    before = do
        cfg <- defaultConfigStdout
        setMinSeverity cfg Debug
        setupTrace_ cfg "tests"
    after (tr, sb) = do
        logDebug tr "Logging shutdown."
        shutdown sb

trMessageText
    :: (MonadIO m, ToText a, HasPrivacyAnnotation a, HasSeverityAnnotation a)
    => Tracer m (LoggerName, LogObject Text)
    -> Tracer m a
trMessageText tr = Tracer $ \arg -> do
   let msg = toText arg
       tracer = if msg == mempty then nullTracer else tr
   meta <- mkLOMeta (getSeverityAnnotation arg) (getPrivacyAnnotation arg)
   traceWith tracer (mempty, LogObject mempty meta (LogMessage msg))
