{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Unused LANGUAGE pragma" -}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module contains a mechanism for launching external processes, ensuring
-- that they are terminated on exceptions.

module Cardano.Launcher
    ( Command (..)
    , ProcessRun (..)
    , StdStream(..)
    , ProcessHasExited(..)
    , withBackendProcess
    , withBackendCreateProcess

    -- * Logging
    , LauncherLog(..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..)
    )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..)
    , HasSeverityAnnotation (..)
    )
import Cardano.Startup
    ( killProcess
    )
import Control.Monad
    ( join
    , void
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Monad.IO.Unlift
    ( MonadUnliftIO (..)
    )
import Control.Tracer
    ( Tracer
    , contramap
    , traceWith
    )
import Data.Either.Combinators
    ( leftToMaybe
    )
import Data.List
    ( isPrefixOf
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText (..)
    )
import Fmt
    ( Buildable (..)
    , Builder
    , blockListF'
    , fmt
    , indentF
    , (+|)
    , (+||)
    , (|+)
    , (||+)
    )
import GHC.Generics
    ( Generic
    )
import System.Exit
    ( ExitCode (..)
    )
import System.IO
    ( Handle
    )
import System.Process
    ( cleanupProcess
    , getPid
    )
import UnliftIO.Async
    ( race
    )
import UnliftIO.Concurrent
    ( forkIO
    , forkIOWithUnmask
    , killThread
    , threadDelay
    )
import UnliftIO.Exception
    ( Exception
    , IOException
    , bracket
    , bracket_
    , finally
    , onException
    , throwIO
    , tryJust
    )
import UnliftIO.MVar
    ( newEmptyMVar
    , putMVar
    , readMVar
    )
import UnliftIO.Process
    ( CmdSpec (..)
    , CreateProcess (..)
    , ProcessHandle
    , StdStream (..)
    , createProcess
    , proc
    , waitForProcess
    )

import qualified Data.Text as T

-- | Represent a command to execute. Args are provided as a list where options
-- are expected to be prefixed with `--` or `-`. For example:
--
-- @
-- Command "cardano-wallet"
--     [ "server"
--     , "--port", "8080"
--     , "--network", "mainnet"
--     ] (return ())
--     Inherit
--     Inherit
-- @
data Command = Command
    { cmdName :: String
    , cmdArgs :: [String]
    , cmdSetup :: IO ()
        -- ^ An extra action to run _before_ the command
    , cmdInput :: StdStream
        -- ^ Input to supply to command
    , cmdOutput :: StdStream
        -- ^ What to do with stdout & stderr
    } deriving (Generic)

instance Show Command where
    show = show . build

instance Eq Command where
    a == b = build a == build b

-- | Format a command nicely with one argument / option per line.
--
-- e.g.
--
-- >>> fmt $ buildCommand "cardano-wallet-server" ["--port", "8080", "--network", "mainnet"] (return ())
-- cardano-wallet-server
--     --port 8080
--     --network mainnet
buildCommand :: String -> [String] -> Builder
buildCommand name args = mconcat [build name, "\n", indentF 4 argsBuilder]
  where
    argsBuilder = blockListF' "" build $ snd $ foldl buildOptions ("", []) args
    buildOptions :: (String, [String]) -> String -> (String, [String])
    buildOptions ("", grp) arg =
        (arg, grp)
    buildOptions (partial, grp) arg =
        if ("--" `isPrefixOf` partial) && not ("--" `isPrefixOf` arg) then
            ("", grp ++ [partial <> " " <> arg])
        else
            (arg, grp ++ [partial])

instance Buildable Command where
    build (Command name args _ _ _) = buildCommand name args

-- | ProcessHasExited is used by a monitoring thread to signal that the process
-- has exited.
data ProcessHasExited
    = ProcessDidNotStart String IOException
    | ProcessHasExited String ExitCode
    deriving (Show, Eq)

instance Exception ProcessHasExited

newtype ProcessRun m a = ProcessRun
    (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> m a)
-- | Starts a command in the background and then runs an action. If the action
-- finishes (through an exception or otherwise) then the process is terminated
-- (see 'withCreateProcess') for details. If the process exits, the action is
-- cancelled. The return type reflects those two cases.
--
-- The action receives the 'ProcessHandle' and stdin 'Handle' as arguments.
withBackendProcess
    :: MonadUnliftIO m
    => Tracer m LauncherLog
    -- ^ Logging
    -> Command
    -- ^ 'Command' description
    -> ProcessRun m a
    -- ^ Action to execute while process is running.
    -> m a
withBackendProcess tr (Command name args before std_in std_out) action =
    liftIO before >> withBackendCreateProcess tr process action
  where
    process = (proc name args) { std_in, std_out, std_err = std_out }

-- | A variant of 'withBackendProcess' which accepts a general 'CreateProcess'
-- object. This version also has nicer async properties than
-- 'System.Process.withCreateProcess'.
--
-- This function should ensure:
--
-- 1. If the action finishes or throws an exception, then the process is also
--    terminated.
--
-- 2. After the process is sent the signal to terminate, this function will
--    block until the process has actually exited - unless that takes longer
--    than the 5 second timeout. After the timeout has lapsed, the process will
--    be sent a kill signal.
--
-- 3. If the process exits, then the action is cancelled.
--
-- fixme: This is more or less a reimplementation of
-- 'System.Process.Typed.withProcessWait' (except for wait timeout). The
-- launcher code should be converted to use @typed-process@.
withBackendCreateProcess
    :: forall m a. (MonadUnliftIO m)
    => Tracer m LauncherLog
    -- ^ Logging
    -> CreateProcess
    -- ^ 'Command' description
    -> ProcessRun m a
    -- ^ Action to execute while process is running.
    -> m a
withBackendCreateProcess tr process (ProcessRun action) = do
    traceWith tr $ MsgLauncherStart name args
    exitVar <- newEmptyMVar
    res <- fmap join $ tryJust spawnPredicate $ bracket
        (createProcess process)
        (cleanupProcessAndWait (readMVar exitVar)) $
            \(mstdin, mstdout, mstderr, ph) -> do
                pid <- maybe "-" (T.pack . show) <$> liftIO (getPid ph)
                let tr' = contramap (WithProcessInfo name pid) tr
                let tr'' = contramap MsgLauncherWait tr'
                traceWith tr' MsgLauncherStarted
                interruptibleWaitForProcess tr'' ph (putMVar exitVar)
                race (ProcessHasExited name <$> readMVar exitVar) $ bracket_
                    (traceWith tr' MsgLauncherAction)
                    (traceWith tr' MsgLauncherActionDone)
                    (action mstdin mstdout mstderr ph)

    traceWith tr $ MsgLauncherFinish (leftToMaybe res)
    case res of
        Left e -> liftIO $ throwIO e
        Right a -> return a
  where
    -- Exceptions resulting from the @exec@ call for this command. The most
    -- likely exception is that the command does not exist. We don't want to
    -- catch exceptions thrown by the action. I couldn't find a better way of
    -- doing this.
    spawnPredicate :: IOException -> Maybe ProcessHasExited
    spawnPredicate e
        | name `isPrefixOf` show e = Just (ProcessDidNotStart name e)
        | otherwise = Nothing

     -- Run the 'cleanupProcess' function from the process library, but wait for
     -- the process to exit, rather than immediately returning. If the process
     -- doesn't exit after timeout, kill it, to avoid blocking indefinitely.
    cleanupProcessAndWait getExitStatus ps@(_, _, _, ph) = do
        traceWith tr MsgLauncherCleanup
        liftIO $ cleanupProcess ps
        let timeoutSecs = 5
        -- Async exceptions are currently masked because this is running in a
        -- bracket cleanup handler. We fork a thread and unmask so that the
        -- timeout can be cancelled.
        tid <- forkIOWithUnmask $ \unmask -> unmask $ do
            threadDelay (timeoutSecs * 1000 * 1000)
            traceWith tr (MsgLauncherCleanupTimedOut timeoutSecs)
            liftIO (getPid ph >>= mapM_ killProcess)
        void getExitStatus `finally` killThread tid
        traceWith tr MsgLauncherCleanupFinished

    -- Wraps 'waitForProcess' in another thread. This works around the unwanted
    -- behaviour of the process library on Windows where 'waitForProcess' seems
    -- to block all concurrent async actions in the thread.
    interruptibleWaitForProcess
        :: Tracer m WaitForProcessLog
        -> ProcessHandle
        -> (ExitCode -> m ())
        -> m ()
    interruptibleWaitForProcess tr' ph onExit =
        void $ forkIO (waitThread `onException` continue)
      where
        waitThread = do
            traceWith tr' MsgWaitBefore
            status <- waitForProcess ph
            traceWith tr' (MsgWaitAfter status)
            onExit status

        continue = do
            traceWith tr' MsgWaitCancelled
            onExit (ExitFailure 256)

    (name, args) = getCreateProcessNameArgs process

-- | Recover the command name and arguments from a 'proc', just for logging.
getCreateProcessNameArgs :: CreateProcess -> (FilePath, [String])
getCreateProcessNameArgs process = case cmdspec process of
    ShellCommand cmd -> (cmd, [])
    RawCommand cmd args -> (cmd, args)

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data LauncherLog
    = MsgLauncherStart String [String]
    | WithProcessInfo String Text LaunchedProcessLog
    | MsgLauncherCleanup
    | MsgLauncherCleanupTimedOut Int
    | MsgLauncherCleanupFinished
    | MsgLauncherFinish (Maybe ProcessHasExited)
    deriving (Show, Eq, Generic)

data LaunchedProcessLog
    = MsgLauncherStarted
    | MsgLauncherAction
    | MsgLauncherActionDone
    | MsgLauncherWait WaitForProcessLog
    deriving (Show, Eq, Generic)

data WaitForProcessLog
    = MsgWaitBefore
    | MsgWaitAfter ExitCode
    | MsgWaitCancelled
    deriving (Show, Eq, Generic)

instance HasPrivacyAnnotation LauncherLog
instance HasSeverityAnnotation LauncherLog where
    getSeverityAnnotation = \case
        MsgLauncherStart _ _ -> Notice
        WithProcessInfo _ _ msg -> getSeverityAnnotation msg
        MsgLauncherFinish Nothing -> Debug
        MsgLauncherFinish (Just (ProcessDidNotStart _ _)) -> Error
        MsgLauncherFinish (Just (ProcessHasExited _ st)) -> case st of
            ExitSuccess -> Notice
            ExitFailure _ -> Error
        MsgLauncherCleanup -> Debug
        MsgLauncherCleanupTimedOut _ -> Notice
        MsgLauncherCleanupFinished -> Debug

instance HasPrivacyAnnotation LaunchedProcessLog
instance HasSeverityAnnotation LaunchedProcessLog where
    getSeverityAnnotation = \case
        MsgLauncherStarted -> Info
        MsgLauncherWait msg -> getSeverityAnnotation msg
        MsgLauncherAction -> Debug
        MsgLauncherActionDone -> Notice

instance HasPrivacyAnnotation WaitForProcessLog
instance HasSeverityAnnotation WaitForProcessLog where
    getSeverityAnnotation = \case
        MsgWaitBefore -> Debug
        MsgWaitAfter _ -> Debug
        MsgWaitCancelled -> Debug

instance ToText ProcessHasExited where
    toText (ProcessHasExited name code) =
        "Child process "+|name|+" exited with "+|statusText code|+""
    toText (ProcessDidNotStart name _e) =
        "Could not start "+|name|+""

instance ToText LauncherLog where
    toText ll = fmt $ case ll of
        MsgLauncherStart cmd args ->
            "Starting process "+|buildCommand cmd args|+""
        WithProcessInfo name pid msg ->
            "["+|name|+"."+|pid|+"] "+|toText msg|+""
        MsgLauncherFinish Nothing ->
            "Action finished"
        MsgLauncherFinish (Just exited) -> build $ toText exited
        MsgLauncherCleanup ->
            "Begin process cleanup"
        MsgLauncherCleanupTimedOut t ->
            "Timed out waiting for process to exit after "+|t|+" seconds"
        MsgLauncherCleanupFinished ->
            "Process cleanup finished"

instance ToText LaunchedProcessLog where
    toText = \case
        MsgLauncherStarted -> "Process started"
        MsgLauncherAction -> "Running withBackend action"
        MsgLauncherWait msg -> toText msg
        MsgLauncherActionDone -> "withBackend action done. Terminating child process"

instance ToText WaitForProcessLog where
    toText = \case
        MsgWaitBefore ->
            "Waiting for process to exit"
        MsgWaitAfter status -> fmt $
            "Process exited with "+|statusText status|+""
        MsgWaitCancelled ->
            "There was an exception waiting for the process"

statusText :: ExitCode -> Text
statusText ExitSuccess = "success"
statusText (ExitFailure n)
    | n >= 0 = fmt $ "code "+||n||+" (failure)"
    | otherwise = fmt $ "signal "+||(-n)||+""
