-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- This module contains a mechanism for launching external processes together,
-- and provides the functionality needed to kill them all if one goes down.
-- (would be achieved using @monitor@ and @kill@ in combination)

module Cardano.Launcher
    ( Command (..)
    , ProcessHasExited(..)
    , launch
    ) where

import Prelude

import Control.Concurrent.Async
    ( forConcurrently )
import Control.Exception
    ( Exception, throwIO, try )
import Data.List
    ( isPrefixOf )
import Fmt
    ( Buildable (..), blockListF', indentF )
import System.Exit
    ( ExitCode )
import System.Process
    ( proc, waitForProcess, withCreateProcess )


data Command = Command
    { cmdName :: String
    , cmdArgs :: [String]
    , cmdSetup :: IO ()
        -- ^ An extra action to run _before_ the command
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
    build (Command name args _) = build name
        <> "\n"
        <> indentF 4 (blockListF' "" build $ snd $ foldl buildOptions ("", []) args)
      where
        buildOptions :: (String, [String]) -> String -> (String, [String])
        buildOptions ("", grp) arg =
            (arg, grp)
        buildOptions (partial, grp) arg =
            if ("--" `isPrefixOf` partial) && not ("--" `isPrefixOf` arg) then
                ("", grp ++ [partial <> " " <> arg])
            else
                (arg, grp ++ [partial])

-- | ProcessHasExited is used by a monitoring thread to signal that the process
-- has exited.
data ProcessHasExited = ProcessHasExited String ExitCode
    deriving Show

instance Exception ProcessHasExited

launch :: [Command] -> IO ProcessHasExited
launch cmds = do
    res <- try $ forConcurrently cmds $ \(Command name args before) -> do
        before
        withCreateProcess (proc name args) $ \_ _ _ h -> do
            code <- waitForProcess h
            throwIO $ ProcessHasExited name code
    case res of
        Left e -> return e
        Right _ -> error
            "Unreachable. Supervising threads should never finish. \
            \They should stay running or throw @ProcessHasExited@."
