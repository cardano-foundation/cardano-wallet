{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- This module contains a mechanism for launching external processes together,
-- and provides the functionality needed to kill them all if one goes down.
-- (would be achieved using @monitor@ and @kill@ in combination)

module Cardano.Launcher
    ( Command (..)
    , StdStream(..)
    , ProcessHasExited(..)
    , launch
    , withBackendProcess
    , installSignalHandlers

    -- * Logging
    , LauncherLog(..)
    , transformLauncherTrace
    ) where

import Prelude

import Cardano.BM.Data.LogItem
    ( PrivacyAnnotation (..) )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Trace
    ( Trace, appendName, traceNamedItem )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( async, race, waitAnyCancel )
import Control.Exception
    ( Exception, IOException, tryJust )
import Control.Monad
    ( forever, join )
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
import System.Process
    ( CreateProcess (..)
    , StdStream (..)
    , getPid
    , proc
    , waitForProcess
    , withCreateProcess
    )

#ifdef mingw32_HOST_OS
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

-- | Run a bunch of command in separate processes. Note that, this operation is
-- blocking and will throw when one of the given commands terminates. Commands
-- are therefore expected to be daemon or long-running services.
launch :: Trace IO LauncherLog -> [Command] -> IO ProcessHasExited
launch tr cmds = mapM start cmds >>= waitAnyCancel >>= \case
    (_, Left e) -> return e
    (_, Right _) -> error $
            "Unreachable. Supervising threads should never finish. " <>
            "They should stay running or throw @ProcessHasExited@."
  where
    sleep = forever $ threadDelay maxBound
    start = async . flip (withBackendProcess tr) sleep

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
withBackendProcess tr cmd@(Command name args before output) action = do
    before
    launcherLog tr $ MsgLauncherStart cmd
    let process = (proc name args) { std_out = output, std_err = output }
    res <- fmap join $ tryJust spawnPredicate $
        withCreateProcess process $ \_ _ _ h -> do
            pid <- maybe "-" (T.pack . show) <$> getPid h
            let tr' = appendName (T.pack name <> "." <> pid) tr
            launcherLog tr' $ MsgLauncherStarted name pid
            race (ProcessHasExited name <$> waitForProcess h)
                (action <* launcherLog tr' MsgLauncherCleanup)
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

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data LauncherLog
    = MsgLauncherStart Command
    | MsgLauncherStarted String Text
    | MsgLauncherFinish ProcessHasExited
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
transformLauncherTrace = contramap (fmap (fmt . launcherLogText))

launcherLog :: MonadIO m => Trace m LauncherLog -> LauncherLog -> m ()
launcherLog logTrace msg = traceNamedItem logTrace Public (launcherLogLevel msg) msg

launcherLogLevel :: LauncherLog -> Severity
launcherLogLevel (MsgLauncherStart _) = Notice
launcherLogLevel (MsgLauncherStarted _ _) = Info
launcherLogLevel (MsgLauncherFinish (ProcessHasExited _ st)) = case st of
    ExitSuccess -> Notice
    ExitFailure _ -> Error
launcherLogLevel (MsgLauncherFinish (ProcessDidNotStart _ _)) = Error
launcherLogLevel MsgLauncherCleanup = Notice

launcherLogText :: LauncherLog -> Builder
launcherLogText (MsgLauncherStart cmd) =
    "Starting process "+|cmd|+""
launcherLogText (MsgLauncherStarted name pid) =
    "Process "+|name|+" started with pid "+|pid|+""
launcherLogText (MsgLauncherFinish (ProcessHasExited name code)) =
    "Child process "+|name|+" exited with status "+||exitStatus code||+""
launcherLogText (MsgLauncherFinish (ProcessDidNotStart name _e)) =
    "Could not start "+|name|+""
launcherLogText MsgLauncherCleanup = "Terminating child process"
