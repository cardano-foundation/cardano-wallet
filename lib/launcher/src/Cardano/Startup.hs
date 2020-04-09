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
    , installSignalHandlers

    -- * Logging
    , ShutdownHandlerLog(..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Control.Concurrent.Async
    ( async, race, wait )
import Control.Exception
    ( IOException, try )
import Control.Tracer
    ( Tracer, traceWith )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.Text.Class
    ( ToText (..) )
import GHC.IO.Handle.FD
    ( fdToHandle )
import System.IO
    ( hGetChar, hSetEncoding, mkTextEncoding, stderr, stdin, stdout )
import System.IO.CodePage
    ( withCP65001 )
import System.IO.Error
    ( isEOFError )
import System.Posix.Types
    ( Fd (Fd) )

#ifdef WINDOWS
import Cardano.Startup.Windows
    ( installSignalHandlers )
#else
import Cardano.Startup.POSIX
    ( installSignalHandlers )
#endif

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

-- | Runs the given action with an optional cross-platform clean shutdown
-- handler.
--
-- This is necessary when running cardano-wallet as a subprocess of Daedalus.
-- For more details, see
-- <https://github.com/input-output-hk/cardano-launcher/blob/master/docs/windows-clean-shutdown.md>
--
-- It works by reading from the given file descriptor. Once end-of-file is
-- reached, either deliberately by the parent process closing the write end, or
-- automatically because the parent process itself terminated, it cancels the
-- action, causing the program to shut down.
--
withShutdownHandler :: Tracer IO ShutdownHandlerLog -> Maybe Fd -> IO a -> IO (Maybe a)
withShutdownHandler _ Nothing action = Just <$> action
withShutdownHandler tr (Just (Fd fd)) action = do
    traceWith tr $ MsgShutdownHandlerEnabled (Fd fd)
    eitherToMaybe <$> race (wrapUninterruptableIO waitForEOF) action
  where
    waitForEOF :: IO ()
    waitForEOF = do
        hnd <- fdToHandle fd
        r <- try $ hGetChar hnd
        case r of
            Left e
                | isEOFError e -> traceWith tr MsgShutdownEOF
                | otherwise -> traceWith tr $ MsgShutdownError e
            Right _  -> traceWith tr MsgShutdownIncorrectUsage

-- | Windows blocking file IO calls like 'hGetChar' are not interruptable by
-- asynchronous exceptions, as used by async 'cancel' (as of base-4.12).
--
-- This wrapper works around that problem by running the blocking IO in a
-- separate thread. If the parent thread receives an async cancel then it
-- will return. Note however that in this circumstance the child thread may
-- continue and remain blocked, leading to a leak of the thread. As such this
-- is only reasonable to use a fixed number of times for the whole process.
--
wrapUninterruptableIO :: IO a -> IO a
wrapUninterruptableIO action = async action >>= wait

data ShutdownHandlerLog
    = MsgShutdownHandlerEnabled Fd
    | MsgShutdownEOF
    | MsgShutdownError IOException
    | MsgShutdownIncorrectUsage
    deriving (Show, Eq)

instance ToText ShutdownHandlerLog where
    toText = \case
        MsgShutdownHandlerEnabled (Fd fd)->
            "Cross-platform subprocess shutdown handler is enabled on fd "
            <> T.pack (show fd) <> "."
        MsgShutdownEOF ->
            "Starting clean shutdown..."
        MsgShutdownError e ->
            "Error waiting for shutdown: " <> T.pack (show e)
            <> ". Shutting down..."
        MsgShutdownIncorrectUsage ->
             "--shutdown-ipc FD does not expect input. Shutting down..."

instance HasPrivacyAnnotation ShutdownHandlerLog
instance HasSeverityAnnotation ShutdownHandlerLog where
    getSeverityAnnotation = \case
        MsgShutdownHandlerEnabled _ -> Debug
        MsgShutdownEOF -> Notice
        MsgShutdownError _ -> Error
        MsgShutdownIncorrectUsage -> Error
