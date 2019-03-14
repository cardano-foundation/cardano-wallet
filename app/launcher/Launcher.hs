module Launcher where

import Control.Concurrent.Async
    ( forConcurrently )
import Control.Exception
    ( Exception, throwIO, try )
import GHC.IO.Handle
    ( Handle )
import Prelude
import System.Exit
    ( ExitCode )
import System.Process
    ( ProcessHandle
    , cleanupProcess
    , createProcess
    , interruptProcessGroupOf
    , proc
    , waitForProcess
    )

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- This module contains a mechanism for launching external processes together,
-- and provides the functionality needed to kill them all if one goes down.
-- (would be achieved using @monitor@ and @kill@ in combination)

data Command = Command
    { cmdName :: String
    , cmdArgs :: [String]
    }

data RunningProcess = RunningProcess
    { rpCommand :: Command
    , rpHandles :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
    }

-- | ProcessHasExited is used by a monitoring thread to signal that the process
-- has exited.
data ProcessHasExited = ProcessHasExited String ExitCode
    deriving Show

instance Exception ProcessHasExited

getProcessHandle :: RunningProcess -> ProcessHandle
getProcessHandle p = let (_,_,_,ph) = rpHandles p in ph

launch :: [Command] -> IO [RunningProcess]
launch = mapM (\c -> RunningProcess c <$> run c)
  where
    run (Command name args) = createProcess $ proc name args

-- | 
monitor :: [RunningProcess] -> IO ProcessHasExited
monitor running = do
    r <- try $ forConcurrently running supervise
    case r of
        Left e -> return e
        Right _
            -> error "Unreachable. Supervising threads should never finish.\
                     \ They should stay running or throw @ProcessHasExited@."

supervise :: RunningProcess -> IO ()
supervise p@(RunningProcess (Command name _args) _) = do
    code <- waitForProcess (getProcessHandle p)
    throwIO $ ProcessHasExited name code

kill :: RunningProcess -> IO ()
kill p = do
    interruptProcessGroupOf (getProcessHandle p)
    cleanupProcess $ rpHandles p
