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
      withUtf8Encoding
    , setUtf8EncodingHandles

    -- * Clean shutdown
    , withShutdownHandler
    , withShutdownHandler'
    , installSignalHandlers

    -- * Logging
    , ShutdownHandlerLog(..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( DefinePrivacyAnnotation (..), DefineSeverity (..) )
import Control.Concurrent.Async
    ( race )
import Control.Exception
    ( IOException, handle )
import Control.Tracer
    ( Tracer, traceWith )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.Text.Class
    ( ToText (..) )
import System.IO
    ( Handle
    , hIsOpen
    , hSetEncoding
    , mkTextEncoding
    , stderr
    , stderr
    , stdin
    , stdin
    , stdout
    , stdout
    )
import System.IO.CodePage
    ( withCP65001 )

#ifdef WINDOWS
import Cardano.Startup.Windows
    ( installSignalHandlers )
#else
import Cardano.Startup.POSIX
    ( installSignalHandlers )
#endif

import qualified Data.ByteString as BS
import qualified Data.Text as T


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
--
-- TODO: may need to add 'forkIO' if 'hGet' blocks on windows
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
    readerLoop' = BS.hGet h 1000 >>= \case
        "" -> pure () -- EOF
        _ -> readerLoop

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

instance DefinePrivacyAnnotation ShutdownHandlerLog
instance DefineSeverity ShutdownHandlerLog where
    defineSeverity = \case
        MsgShutdownHandler _ -> Debug
        MsgShutdownEOF -> Notice
        MsgShutdownError _ -> Error
