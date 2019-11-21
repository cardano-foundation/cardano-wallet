{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Copyright: © 2018-2019 IOHK
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

    -- * Program startup
    , installSignalHandlers
    , withUtf8Encoding

    -- * Logging
    , LauncherLog(..)
    , transformLauncherTrace
    ) where

import Prelude

import Cardano.BM.Data.LogItem
    ( PrivacyAnnotation (..) )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( DefinePrivacyAnnotation (..), DefineSeverity (..) )
import Cardano.BM.Trace
    ( Trace, appendName, traceNamedItem )
import Control.Concurrent
    ( forkIO )
import Control.Concurrent.Async
    ( race )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Exception
    ( Exception, IOException, onException, tryJust )
import Control.Monad
    ( join, void )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Tracer
    ( contramap )
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
    ( hSetEncoding, mkTextEncoding, stderr, stdin, stdout )
import System.IO.CodePage
    ( withCP65001 )
import System.Process
    ( CreateProcess (..)
    , ProcessHandle
    , StdStream (..)
    , getPid
    , proc
    , waitForProcess
    , withCreateProcess
    )

#ifdef WINDOWS
import Cardano.Launcher.Windows
    ( installSignalHandlers )
#else
import Cardano.Launcher.POSIX
    ( installSignalHandlers )
#endif

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
-- @
data Command = Command
    { cmdName :: String
    , cmdArgs :: [String]
    , cmdSetup :: IO ()
        -- ^ An extra action to run _before_ the command
    , cmdOutput :: StdStream
        -- ^ What to do with stdout & stderr
    } deriving (Generic)

-- Format a command nicely with one argument / option per line.
--
-- e.g.
--
-- >>> fmt $ build $ Command "cardano-wallet-server" ["--port", "8080", "--network", "mainnet"] (return ())
-- cardano-wallet-server
--     --port 8080
--     --network mainnet
instance Buildable Command where
    build (Command name args _ _) = build name
        <> "\n"
        <> indentF 4
            (blockListF' "" build $ snd $ foldl buildOptions ("", []) args)
      where
        buildOptions :: (String, [String]) -> String -> (String, [String])
        buildOptions ("", grp) arg =
            (arg, grp)
        buildOptions (partial, grp) arg =
            if ("--" `isPrefixOf` partial) && not ("--" `isPrefixOf` arg) then
                ("", grp ++ [partial <> " " <> arg])
            else
                (arg, grp ++ [partial])

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
    :: Trace IO LauncherLog
    -- ^ Logging
    -> Command
    -- ^ 'Command' description
    -> IO a
    -- ^ Action to execute while process is running.
    -> IO (Either ProcessHasExited a)
withBackendProcess tr cmd = withBackendProcessHandle tr cmd . const

-- | A variant of 'withBackendProcess' which also provides the 'ProcessHandle' to the
-- given action.
withBackendProcessHandle
    :: Trace IO LauncherLog
    -- ^ Logging
    -> Command
    -- ^ 'Command' description
    -> (ProcessHandle -> IO a)
    -- ^ Action to execute while process is running.
    -> IO (Either ProcessHasExited a)
withBackendProcessHandle tr cmd@(Command name args before output) action = do
    before
    launcherLog tr $ MsgLauncherStart cmd
    let process = (proc name args) { std_out = output, std_err = output }
    res <- fmap join $ tryJust spawnPredicate $
        withCreateProcess process $ \_ _ _ h -> do
            pid <- maybe "-" (T.pack . show) <$> getPid h
            let tr' = appendName (T.pack name <> "." <> pid) tr
            launcherLog tr' $ MsgLauncherStarted name pid

            let waitForExit =
                    ProcessHasExited name <$> interruptibleWaitForProcess tr' h
            let runAction = do
                    launcherLog tr' MsgLauncherAction
                    action h <* launcherLog tr' MsgLauncherCleanup

            race waitForExit runAction
    either (launcherLog tr . MsgLauncherFinish) (const $ pure ()) res
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
        :: Trace IO LauncherLog
        -> ProcessHandle
        -> IO ExitCode
    interruptibleWaitForProcess tr' ph = do
        status <- newEmptyMVar
        void $ forkIO $ waitThread status `onException` continue status
        takeMVar status
      where
        waitThread var = do
            launcherLog tr' MsgLauncherWaitBefore
            status <- waitForProcess ph
            launcherLog tr' (MsgLauncherWaitAfter $ exitStatus status)
            putMVar var status
        continue var = do
            launcherLog tr' MsgLauncherCancel
            putMVar var (ExitFailure 256)

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data LauncherLog
    = MsgLauncherStart Command
    | MsgLauncherStarted String Text
    | MsgLauncherWaitBefore
    | MsgLauncherWaitAfter Int
    | MsgLauncherCancel
    | MsgLauncherFinish ProcessHasExited
    | MsgLauncherAction
    | MsgLauncherCleanup
    deriving (Generic, ToJSON)

instance ToJSON Command where
    toJSON (Command name args _ _) = toJSON (name:args)

instance ToJSON ProcessHasExited where
    toJSON (ProcessDidNotStart name e) =
        object [ "command" .= name, "exception" .= show e ]
    toJSON (ProcessHasExited name code) =
        object [ "command" .= name, "status" .= exitStatus code ]

exitStatus :: ExitCode -> Int
exitStatus ExitSuccess = 0
exitStatus (ExitFailure n) = n

transformLauncherTrace :: Trace IO Text -> Trace IO LauncherLog
transformLauncherTrace = contramap (fmap toText)

launcherLog :: MonadIO m => Trace m LauncherLog -> LauncherLog -> m ()
launcherLog logTrace msg = traceNamedItem logTrace Public (defineSeverity msg) msg

instance DefinePrivacyAnnotation LauncherLog
instance DefineSeverity LauncherLog where
    defineSeverity ev = case ev of
        MsgLauncherStart _ -> Notice
        MsgLauncherStarted _ _ -> Info
        MsgLauncherWaitBefore -> Debug
        MsgLauncherWaitAfter _ -> Debug
        MsgLauncherCancel -> Debug
        MsgLauncherFinish (ProcessHasExited _ st) -> case st of
            ExitSuccess -> Notice
            ExitFailure _ -> Error
        MsgLauncherFinish (ProcessDidNotStart _ _) -> Error
        MsgLauncherAction -> Debug
        MsgLauncherCleanup -> Notice

instance ToText LauncherLog where
    toText = fmt . launcherLogText

launcherLogText :: LauncherLog -> Builder
launcherLogText (MsgLauncherStart cmd) =
    "Starting process "+|cmd|+""
launcherLogText (MsgLauncherStarted name pid) =
    "Process "+|name|+" started with pid "+|pid|+""
launcherLogText MsgLauncherWaitBefore = "About to waitForProcess"
launcherLogText (MsgLauncherWaitAfter status) = "waitForProcess returned "+||status||+""
launcherLogText MsgLauncherCancel = "There was an exception waiting for the process"
launcherLogText (MsgLauncherFinish (ProcessHasExited name code)) =
    "Child process "+|name|+" exited with status "+||exitStatus code||+""
launcherLogText (MsgLauncherFinish (ProcessDidNotStart name _e)) =
    "Could not start "+|name|+""
launcherLogText MsgLauncherAction = "Running withBackend action"
launcherLogText MsgLauncherCleanup = "Terminating child process"

{-------------------------------------------------------------------------------
                            Unicode Terminal Helpers
-------------------------------------------------------------------------------}

-- | Force the locale text encoding to UTF-8. This is needed because the CLI
-- prints UTF-8 characters regardless of the @LANG@ environment variable or any
-- other settings.
--
-- On Windows the current console code page is changed to UTF-8.
withUtf8Encoding :: IO a -> IO a
withUtf8Encoding action = withCP65001 (setUtf8EncodingHandles >> action)

setUtf8EncodingHandles :: IO ()
setUtf8EncodingHandles = do
    utf8' <- mkTextEncoding "UTF-8//TRANSLIT"
    mapM_ (`hSetEncoding` utf8') [stdin, stdout, stderr]
