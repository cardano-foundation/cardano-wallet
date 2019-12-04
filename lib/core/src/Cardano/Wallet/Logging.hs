{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- This module contains utility functions for logging and mapping trace data.

module Cardano.Wallet.Logging
    ( transformTextTrace
    , logTrace

      -- * Latency logging
    , LatencyLog (..)
    , LatencyLogMsg (..)
    , measureLatency
    , setupLatencyLogging
    , captureLatencyLogging
    ) where

import Prelude

import Cardano.BM.Configuration.Static
    ( defaultConfigStdout )
import Cardano.BM.Data.LogItem
    ( LOMeta (..), LogObject (..) )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( DefinePrivacyAnnotation (..), DefineSeverity (..) )
import Cardano.BM.Setup
    ( setupTrace_ )
import Cardano.BM.Trace
    ( Trace, traceInTVarIO, traceNamedItem )
import Control.Concurrent.STM.TVar
    ( newTVarIO, readTVarIO )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Tracer
    ( contramap )
import Data.Aeson
    ( ToJSON (..) )
import Data.Maybe
    ( mapMaybe )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Time
    ( UTCTime )
import Data.Time.Clock
    ( NominalDiffTime, diffUTCTime )
import Fmt
    ( fmt, (+|), (|+) )
import GHC.Generics
    ( Generic )


-- | Converts a 'Text' trace into any other type of trace that has a 'ToText'
-- instance.
transformTextTrace :: ToText a => Trace IO Text -> Trace IO a
transformTextTrace = contramap (fmap toText)

-- | Traces some data.
logTrace
    :: (MonadIO m, DefinePrivacyAnnotation a, DefineSeverity a)
    => Trace m a
    -> a
    -> m ()
logTrace tr msg = traceNamedItem tr priv sev msg
  where
    priv = definePrivacyAnnotation msg
    sev = defineSeverity msg


{-------------------------------------------------------------------------------
                               Latency Logging
-------------------------------------------------------------------------------}

-- | The log messages for IO action latency measurement.
data LatencyLog = LatencyLog
    { latencyLogFun :: String
    , latencyLogMsg :: LatencyLogMsg
    } deriving (Generic, Show, Eq, ToJSON)

-- | Log messages about latency measurement.
data LatencyLogMsg
    = MsgStartMeasurement
    | MsgStopMeasurement
    deriving (Generic, Show, Eq, ToJSON)

instance DefinePrivacyAnnotation LatencyLogMsg

instance DefineSeverity LatencyLogMsg where
    defineSeverity ev = case ev of
        MsgStartMeasurement -> Info
        MsgStopMeasurement -> Info

instance ToText LatencyLog where
    toText (LatencyLog name msg) =
        fmt $ "Measuring latency of "+|name|+": "+|toText msg|+""

instance ToText LatencyLogMsg where
    toText = \case
        MsgStartMeasurement -> "Starting point"
        MsgStopMeasurement -> "Ending point"

-- | Measuring latency of IO action
measureLatency
    :: MonadIO m
    => Trace IO LatencyLog
    -> String
    -> m b
    -> m b
measureLatency tr name action = do
    trace MsgStartMeasurement
    res <- action
    trace MsgStopMeasurement
    pure res
  where
    tr' = contramap (fmap (LatencyLog name)) tr
    trace = liftIO . logTrace tr'

setupLatencyLogging :: IO (Trace IO LatencyLog)
setupLatencyLogging = do
    cfg <- defaultConfigStdout
    transformTextTrace . fst <$> setupTrace_ cfg "LatencyLogging"

withLatencyLogging :: ((Trace IO LatencyLog, IO [UTCTime]) -> IO a) -> IO a
withLatencyLogging action = do
    tvar <- newTVarIO []
    let getTimes = reverse . mapMaybe (Just . tstamp . loMeta) <$> readTVarIO tvar
    action (traceInTVarIO tvar, getTimes)

captureLatencyLogging :: (Trace IO LatencyLog -> IO a) -> IO (a, NominalDiffTime)
captureLatencyLogging action = withLatencyLogging $ \(tr, getMsgs) -> do
    res <- action tr
    [startT, endT] <- getMsgs
    let timeDiff = diffUTCTime endT startT
    pure (res, timeDiff)
