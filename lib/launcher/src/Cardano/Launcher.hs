{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

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
    , setupStateDir
    ) where

import Prelude

import Control.Concurrent.Async
    ( forConcurrently )
import Control.Exception
    ( Exception, throwIO, try )
import Data.List
    ( isPrefixOf )
import Data.Text
    ( Text )
import Fmt
    ( Buildable (..), blockListF', indentF )
import System.Directory
    ( createDirectory, doesDirectoryExist )
import System.Exit
    ( ExitCode )
import System.Process
    ( CreateProcess (..)
    , StdStream (..)
    , proc
    , waitForProcess
    , withCreateProcess
    )

import qualified Data.Text as T

#ifdef mingw32_HOST_OS
import Cardano.Launcher.Windows
    ( installSignalHandlers )
#else
import Cardano.Launcher.POSIX
    ( installSignalHandlers )
#endif

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

-- | Run a bunch of command in separate processes. Note that, this operation is
-- blocking and will throw when one of the given commands terminates. Commands
-- are therefore expected to be daemon or long-running services.
launch :: [Command] -> IO ProcessHasExited
launch cmds = do
    res <- try $ forConcurrently cmds $ \(Command name args before output) -> do
        before
        let process = (proc name args) { std_out = output , std_err = output }
        withCreateProcess process $ \_ _ _ h -> do
            code <- waitForProcess h
            throwIO $ ProcessHasExited name code
    case res of
        Left e -> return e
        Right _ -> error $
            "Unreachable. Supervising threads should never finish. " <>
            "They should stay running or throw @ProcessHasExited@."

-- | Initialize a state directory to store blockchain data such as blocks or
-- the wallet database.
setupStateDir :: (Text -> IO ()) -> FilePath -> IO ()
setupStateDir logT dir = doesDirectoryExist dir >>= \case
    True -> logT $ "Using state directory: " <> T.pack dir
    False -> do
        logT $ "Creating state directory: " <> T.pack dir
        createDirectory dir
