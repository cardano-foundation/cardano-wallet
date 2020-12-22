{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

{- HLINT ignore "Unused LANGUAGE pragma" -}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module contains a mechanism for launching external processes, ensuring
-- that they are terminated on exceptions.

module Cardano.Launcher
    ( Command (..)
    , StdStream(..)
    , ProcessHasExited(..)
    , withBackendProcess
    , withBackendProcessHandle
    , withBackendCreateProcess

    -- * Logging
    , LauncherLog(..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Control.Concurrent
    ( forkIO )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Monad
    ( join, void )
import Control.Tracer
    ( Tracer, contramap, traceWith )
import Data.Aeson
    ( ToJSON (..), object, (.=) )
import Data.List
    ( isPrefixOf )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
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
    ( Generic )
import System.Exit
    ( ExitCode (..) )
import System.IO
    ( Handle )
import System.Process
    ( getPid )
import UnliftIO.Async
    ( race )
import UnliftIO.Exception
    ( Exception, IOException, finally, onException, tryJust )
import UnliftIO.Process
    ( CmdSpec (..)
    , CreateProcess (..)
    , ProcessHandle
    , StdStream (..)
    , proc
    , waitForProcess
    , withCreateProcess
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

-- | Starts a command in the background and then runs an action. If the action
-- finishes (through an exception or otherwise) then the process is terminated
-- (see 'withCreateProcess') for details. If the process exits, the action is
-- cancelled. The return type reflects those two cases.
withBackendProcess
    :: Tracer IO LauncherLog
    -- ^ Logging
    -> Command
    -- ^ 'Command' description
    -> IO a
    -- ^ Action to execute while process is running.
    -> IO (Either ProcessHasExited a)
withBackendProcess tr cmd = withBackendProcessHandle tr cmd . const . const

-- | A variant of 'withBackendProcess' which also provides the 'ProcessHandle'
-- and stdin 'Handle' to the given action.
withBackendProcessHandle
    :: Tracer IO LauncherLog
    -- ^ Logging
    -> Command
    -- ^ 'Command' description
    -> (Maybe Handle -> ProcessHandle -> IO a)
    -- ^ Action to execute while process is running.
    -> IO (Either ProcessHasExited a)
withBackendProcessHandle tr (Command name args before std_in std_out) action =
    before >> withBackendCreateProcess tr process action
  where
    process = (proc name args)  { std_in, std_out, std_err = std_out }


-- | A variant of 'withBackendProcess' which accepts a general 'CreateProcess'
-- object.
withBackendCreateProcess
    :: Tracer IO LauncherLog
    -- ^ Logging
    -> CreateProcess
    -- ^ 'Command' description
    -> (Maybe Handle -> ProcessHandle -> IO a)
    -- ^ Action to execute while process is running.
    -> IO (Either ProcessHasExited a)
withBackendCreateProcess tr process action = do
    traceWith tr $ MsgLauncherStart name args
    res <- fmap join $ tryJust spawnPredicate $
        withCreateProcess process $ \mstdin _ _ h -> do
            pid <- maybe "-" (T.pack . show) <$> getPid h
            let tr' = contramap (WithProcessInfo name pid) tr
            traceWith tr' MsgLauncherStarted

            let waitForExit =
                    ProcessHasExited name <$> interruptibleWaitForProcess tr' h
            let runAction = do
                    traceWith tr' MsgLauncherAction
                    action mstdin h `finally` traceWith tr' MsgLauncherCleanup

            race waitForExit runAction
    either (traceWith tr . MsgLauncherFinish) (const $ pure ()) res
    pure res
  where
    -- Exceptions resulting from the @exec@ call for this command. The most
    -- likely exception is that the command does not exist. We don't want to
    -- catch exceptions thrown by the action. I couldn't find a better way of
    -- doing this.
    spawnPredicate :: IOException -> Maybe ProcessHasExited
    spawnPredicate e
        | name `isPrefixOf` show e = Just (ProcessDidNotStart name e)
        | otherwise = Nothing

    -- Wraps 'waitForProcess' in another thread. This works around the unwanted
    -- behaviour of the process library on Windows where 'waitForProcess' seems
    -- to block all concurrent async actions in the thread.
    interruptibleWaitForProcess
        :: Tracer IO LaunchedProcessLog
        -> ProcessHandle
        -> IO ExitCode
    interruptibleWaitForProcess tr' ph = do
        status <- newEmptyMVar
        void $ forkIO $ waitThread status `onException` continue status
        takeMVar status
      where
        waitThread var = do
            traceWith tr' MsgLauncherWaitBefore
            status <- waitForProcess ph
            traceWith tr' (MsgLauncherWaitAfter $ exitStatus status)
            putMVar var status
        continue var = do
            traceWith tr' MsgLauncherCancel
            putMVar var (ExitFailure 256)

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
    | MsgLauncherFinish ProcessHasExited
    deriving (Show, Eq, Generic, ToJSON)

data LaunchedProcessLog
    = MsgLauncherStarted
    | MsgLauncherWaitBefore
    | MsgLauncherWaitAfter Int
    | MsgLauncherCancel
    | MsgLauncherAction
    | MsgLauncherCleanup
    deriving (Show, Eq, Generic, ToJSON)

instance ToJSON Command where
    toJSON (Command name args _ _ _) = toJSON (name:args)

instance ToJSON ProcessHasExited where
    toJSON (ProcessDidNotStart name e) =
        object [ "command" .= name, "exception" .= show e ]
    toJSON (ProcessHasExited name code) =
        object [ "command" .= name, "status" .= exitStatus code ]

exitStatus :: ExitCode -> Int
exitStatus ExitSuccess = 0
exitStatus (ExitFailure n) = n

instance HasPrivacyAnnotation LauncherLog
instance HasSeverityAnnotation LauncherLog where
    getSeverityAnnotation = \case
        MsgLauncherStart _ _ -> Notice
        WithProcessInfo _ _ msg -> getSeverityAnnotation msg
        MsgLauncherFinish (ProcessDidNotStart _ _) -> Error
        MsgLauncherFinish (ProcessHasExited _ st) -> case st of
            ExitSuccess -> Notice
            ExitFailure _ -> Error

instance HasPrivacyAnnotation LaunchedProcessLog
instance HasSeverityAnnotation LaunchedProcessLog where
    getSeverityAnnotation = \case
        MsgLauncherStarted -> Info
        MsgLauncherWaitBefore -> Debug
        MsgLauncherWaitAfter _ -> Debug
        MsgLauncherCancel -> Debug
        MsgLauncherAction -> Debug
        MsgLauncherCleanup -> Notice

instance ToText LauncherLog where
    toText = fmt . launcherLogText

launcherLogText :: LauncherLog -> Builder
launcherLogText (MsgLauncherStart cmd args) =
    "Starting process "+|buildCommand cmd args|+""
launcherLogText (WithProcessInfo name pid msg) =
    "["+|name|+"."+|pid|+"] "+|launchedProcessText msg|+""
launcherLogText (MsgLauncherFinish (ProcessDidNotStart name _e)) =
    "Could not start "+|name|+""
launcherLogText (MsgLauncherFinish (ProcessHasExited name code)) =
    "Child process "+|name|+" exited with status "+||exitStatus code||+""

launchedProcessText :: LaunchedProcessLog -> Builder
launchedProcessText MsgLauncherStarted = "Process started"
launchedProcessText MsgLauncherWaitBefore = "About to waitForProcess"
launchedProcessText (MsgLauncherWaitAfter status) = "waitForProcess returned "+||status||+""
launchedProcessText MsgLauncherCancel = "There was an exception waiting for the process"
launchedProcessText MsgLauncherAction = "Running withBackend action"
launchedProcessText MsgLauncherCleanup = "Terminating child process"
