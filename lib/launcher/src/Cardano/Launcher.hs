{-# LANGUAGE CPP #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- This module contains a mechanism for launching external processes together,
-- and provides the functionality needed to kill them all if one goes down.
-- (would be achieved using @monitor@ and @kill@ in combination)

module Cardano.Launcher
    ( Command (..)
    , StdStream(..)
    , ProcessHasExited(..)
    , launch
    , installSignalHandlers
    , logEntrySeverity
    , Severity (..)
    ) where

import Prelude

import Control.Concurrent.Async
    ( forConcurrently )
import Control.Exception
    ( Exception, throwIO, try )
import Data.Function
    ( (&) )
import Data.List
    ( isPrefixOf )
import Data.Maybe
    ( fromMaybe, listToMaybe )
import Data.Text.Class
    ( CaseStyle (..)
    , FromText (..)
    , ToText (..)
    , fromTextMaybe
    , fromTextToBoundedEnum
    , toTextFromBoundedEnum
    )
import Fmt
    ( Buildable (..), blockListF', indentF )
import System.Exit
    ( ExitCode )
import System.Process
    ( CreateProcess (..)
    , StdStream (..)
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
import qualified Data.Text.IO as T
import qualified Streamly as S
import qualified Streamly.Prelude as S

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
    }

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
data ProcessHasExited = ProcessHasExited String ExitCode
    deriving Show

instance Exception ProcessHasExited

-- | Repeat the definition of Severity here. (HACK)
data Severity = Debug | Info | Error | Emergency
    deriving (Bounded, Enum, Eq, Ord, Show)

instance ToText Severity where
    toText = toTextFromBoundedEnum SnakeUpperCase

instance FromText Severity where
    fromText = fromTextToBoundedEnum SnakeUpperCase

-- | Attempts to determine the severity of a log entry from the HTTP bridge.
--   Returns 'Nothing' if it was not possible to determine a severity.
logEntrySeverity :: T.Text -> Maybe Severity
logEntrySeverity logEntry = do
    -- Get the very first word of the log entry line.
    prefix <- listToMaybe $ words $ T.unpack logEntry
    -- Try to parse the first word as a severity level.
    fromTextMaybe (T.pack prefix)

-- | Hard code the minimum log entry severity. (HACK)
minLogEntrySeverity :: Severity
minLogEntrySeverity = Debug

-- | Run a bunch of command in separate processes. Note that, this operation is
-- blocking and will throw when one of the given commands terminates. Commands
-- are therefore expected to be daemon or long-running services.
launch :: [Command] -> IO ProcessHasExited
launch cmds = do
    res <- try $ forConcurrently cmds $ \(Command name args before output) -> do
        before
        let process = (proc name args) { std_out = output , std_err = output }
        withCreateProcess process $ \_ std_out' std_err' h -> do
            case (std_out', std_err') of
                (Just o, Just e) -> do
                    S.runStream
                        -- For now, just combine stdout and stderr.
                        $ S.parallel (S.fromHandle o) (S.fromHandle e)
                        & S.map T.pack
                        -- Attempt to tag each log line with a severity.
                        -- If we can't identify a severity for a given line,
                        -- we assign it the same severity as the previous line.
                        -- This accounts for the case where log entries span
                        -- multiple lines.
                        & S.scanl'
                            (\(s, _) t -> (fromMaybe s (logEntrySeverity t), t))
                            (minBound, mempty)
                        -- Filter by minimum severity.
                        & S.filter ((>= minLogEntrySeverity) . fst)
                        -- Throw away the severity tags.
                        & S.map snd
                        -- Ultimately, we'd wire up this to a supplied output
                        -- handle rather than just printing it out here.
                        & S.mapM T.putStrLn
                _ -> pure ()
            code <- waitForProcess h
            throwIO $ ProcessHasExited name code
    case res of
        Left e -> return e
        Right _ -> error $
            "Unreachable. Supervising threads should never finish. " <>
            "They should stay running or throw @ProcessHasExited@."
