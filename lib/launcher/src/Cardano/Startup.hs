{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module contains functions relating to startup and shutdown of the
-- @cardano-wallet serve@ program.

module Cardano.Startup
    (
    -- * Program startup
      setUtf8EncodingHandles

    -- * Clean shutdown
    , withShutdownHandler
    , withShutdownHandler'
    , installSignalHandlers
    , installSignalHandlersNoLogging
    , killProcess

    -- * File permissions
    , setDefaultFilePermissions
    , restrictFileMode

    -- * Logging
    , ShutdownHandlerLog(..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Control.Tracer
    ( Tracer, traceWith )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.Text.Class
    ( ToText (..) )
import GHC.IO.Encoding
    ( setFileSystemEncoding )
import System.IO
    ( Handle, hIsOpen, hSetEncoding, mkTextEncoding, stderr, stdin, stdout )
import UnliftIO.Async
    ( race )
import UnliftIO.Concurrent
    ( forkIO )
import UnliftIO.Exception
    ( IOException, catch, handle, throwIO )
import UnliftIO.MVar
    ( MVar, newEmptyMVar, putMVar, takeMVar )

#ifdef WINDOWS
import Cardano.Startup.Windows
#else
import Cardano.Startup.POSIX
#endif

import qualified Data.ByteString as BS
import qualified Data.Text as T

{-------------------------------------------------------------------------------
                            Unicode Terminal Helpers
-------------------------------------------------------------------------------}

setUtf8EncodingHandles :: IO ()
setUtf8EncodingHandles = do
    utf8' <- mkTextEncoding "UTF-8//TRANSLIT"
    mapM_ (`hSetEncoding` utf8') [stdin, stdout, stderr]
    setFileSystemEncoding utf8'

{-------------------------------------------------------------------------------
                               Shutdown handlers
-------------------------------------------------------------------------------}

-- | Runs the given action with a cross-platform clean shutdown handler.
--
-- This is necessary when running cardano-wallet as a subprocess of Daedalus.
-- For more details, see
-- <https://github.com/input-output-hk/cardano-launcher/blob/master/docs/windows-clean-shutdown.md>
--
-- It works simply by reading from 'stdin', which is otherwise unused by the API
-- server. Once end-of-file is reached, it cancels the action, causing the
-- program to shut down.
--
-- So, when running @cardano-wallet@ as a subprocess, the parent process should
-- pass a pipe for 'stdin', then close the pipe when it wants @cardano-wallet@
-- to shut down.
withShutdownHandler :: Tracer IO ShutdownHandlerLog -> IO a -> IO (Maybe a)
withShutdownHandler tr = withShutdownHandler' tr stdin

-- | A variant of 'withShutdownHandler' where the handle to read can be chosen.
withShutdownHandler' :: Tracer IO ShutdownHandlerLog -> Handle -> IO a -> IO (Maybe a)
withShutdownHandler' tr h action = do
    enabled <- hIsOpen h
    traceWith tr $ MsgShutdownHandler enabled
    let with
            | enabled = fmap eitherToMaybe . race readerLoop
            | otherwise = fmap Just
    with action
  where
    readerLoop = do
        handle (traceWith tr . MsgShutdownError) readerLoop'
        traceWith tr MsgShutdownEOF
    readerLoop' = waitForInput >>= \case
        "" -> pure () -- eof: stop loop
        _ -> readerLoop' -- repeat
    -- Wait indefinitely for input to be available.
    -- Runs in separate thread so that it does not deadlock on Windows.
    waitForInput = do
        v <- newEmptyMVar :: IO (MVar (Either IOException BS.ByteString))
        _ <- forkIO ((BS.hGet h 1000 >>= putMVar v . Right) `catch` (putMVar v . Left))
        takeMVar v >>= either throwIO pure

data ShutdownHandlerLog
    = MsgShutdownHandler Bool
    | MsgShutdownEOF
    | MsgShutdownError IOException
    deriving (Show, Eq)

instance ToText ShutdownHandlerLog where
    toText = \case
        MsgShutdownHandler enabled ->
            "Cross-platform subprocess shutdown handler is "
            <> if enabled then "enabled." else "disabled."
        MsgShutdownEOF ->
            "Starting clean shutdown..."
        MsgShutdownError e ->
            "Error waiting for shutdown: " <> T.pack (show e)
            <> ". Shutting down..."

instance HasPrivacyAnnotation ShutdownHandlerLog
instance HasSeverityAnnotation ShutdownHandlerLog where
    getSeverityAnnotation = \case
        MsgShutdownHandler _ -> Debug
        MsgShutdownEOF -> Notice
        MsgShutdownError _ -> Error

{-------------------------------------------------------------------------------
                          Termination Signal Handling
-------------------------------------------------------------------------------}

installSignalHandlersNoLogging :: IO ()
installSignalHandlersNoLogging = installSignalHandlers (pure ())
