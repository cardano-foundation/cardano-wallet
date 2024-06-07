{-# LANGUAGE RankNTypes #-}

module Cardano.BM.ToTextTracer
    ( ToTextTracer (..)
    , newToTextTracerFromHandle
    , logHandleFromFilePath
    , withFile
    )
where

import Prelude

import Cardano.BM.Data.Tracer
    ( HasSeverityAnnotation (..)
    , Tracer (Tracer)
    )
import Cardano.BM.Tracing
    ( Severity
    )
import Control.Monad
    ( forever
    , unless
    )
import Control.Monad.STM
    ( retry
    )
import Control.Monad.Trans.Cont
    ( ContT (..)
    )
import Data.Text.Class
    ( ToText (..)
    )
import Data.Time
    ( getCurrentTime
    )
import Data.Time.Format.ISO8601
    ( iso8601Show
    )
import System.FilePath
    ( takeDirectory
    )
import System.IO
    ( Handle
    )
import UnliftIO
    ( BufferMode (NoBuffering)
    , IOMode (WriteMode)
    , SomeException (..)
    , atomically
    , catch
    , hClose
    , hSetBuffering
    , isEmptyTChan
    , newTChanIO
    , openFile
    , readTChan
    , throwIO
    , withAsync
    , writeTChan
    )
import UnliftIO.Directory
    ( createDirectoryIfMissing
    )

import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | A thread-safe tracer that logs messages to a file or stdout for anything
-- that has ToText instance
newtype ToTextTracer
    = ToTextTracer
        (forall a. (HasSeverityAnnotation a, ToText a) => Tracer IO a)

-- | Create a new `Handle` from a file path, opened in `WriteMode` with
-- `NoBuffering`
logHandleFromFilePath :: FilePath -> ContT r IO Handle
logHandleFromFilePath clusterLogsFile = do
    h <- ContT $ withFile clusterLogsFile WriteMode
    hSetBuffering h NoBuffering
    pure h

-- | Create a new `ToTextTracer`
newToTextTracerFromHandle
    :: Handle
    -- ^ If provided, logs will be written to this file, otherwise to stdout
    -> Maybe Severity
    -- ^ Minimum severity level to log
    -> ContT r IO ToTextTracer
newToTextTracerFromHandle h minSeverity = do
    ch <- newTChanIO
    _printer <- ContT $ withAsync $ forever $ do
        (x, s, t) <- atomically $ readTChan ch
        T.hPutStrLn h
            $ T.pack (iso8601Show t)
                <> " ["
                <> T.pack (show s)
                <> "] "
                <> x
    ContT $ \k -> do
        r <- k $ ToTextTracer $ Tracer $ \msg -> do
            let severity = getSeverityAnnotation msg
            unless (Just severity < minSeverity) $ do
                t <- getCurrentTime
                atomically $ writeTChan ch (toText msg, severity, t)
        -- wait until the channel is empty
        atomically $ do
            empty <- isEmptyTChan ch
            unless empty retry
        pure r

-- | A withFile function that creates the directory if it doesn't exist,
-- and sets the buffering to NoBuffering. It also catches exceptions and
-- closes the handle before rethrowing the exception.
-- This cover also a problem with the original withFile function that
-- replace any exception happening in the action with a generic
-- "withFile: openFile: does not exist"
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile path mode action = do
    createDirectoryIfMissing True (takeDirectory path)
    h <- openFile path mode
    hSetBuffering h NoBuffering
    let action' = do
            r <- action h
            hClose h
            pure r
        handler (SomeException e) = do
            hClose h
            throwIO e
    catch action' handler
