{-# LANGUAGE RankNTypes #-}

module Cardano.BM.ToTextTracer
    ( ToTextTracer (..)
    , newToTextTracer
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
    , (>=>)
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
import UnliftIO
    ( BufferMode (NoBuffering)
    , IOMode (WriteMode)
    , MonadIO (liftIO)
    , async
    , atomically
    , hSetBuffering
    , isEmptyTChan
    , link
    , newTChanIO
    , readTChan
    , withFile
    , writeTChan
    )

import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | A thread-safe tracer that logs messages to a file or stdout for anything
-- that has ToText instance
newtype ToTextTracer
    = ToTextTracer
        (forall a. (HasSeverityAnnotation a, ToText a) => Tracer IO a)

-- | Create a new `ToTextTracer`
newToTextTracer
    :: FilePath
    -- ^ If provided, logs will be written to this file, otherwise to stdout
    -> Maybe Severity
    -- ^ Minimum severity level to log
    -> (ToTextTracer -> IO r)
    -- ^ Action to run with the new tracer
    -> IO r
newToTextTracer clusterLogsFile minSeverity = runContT $ do
    ch <- newTChanIO
    h <- ContT $ withFile clusterLogsFile WriteMode
    hSetBuffering h NoBuffering
    liftIO $ hSetBuffering h NoBuffering
    liftIO $ async >=> link $ forever $ do
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
