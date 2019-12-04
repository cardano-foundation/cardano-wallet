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
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( DefinePrivacyAnnotation (..), DefineSeverity (..) )
import Cardano.BM.Trace
    ( Trace, traceNamedItem )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Tracer
    ( contramap )
import Data.Aeson
    ( ToJSON (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Time
    ( UTCTime )
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
    = MsgStartMeasurement UTCTime
    | MsgStopMeasurement UTCTime
    deriving (Generic, Show, Eq, ToJSON)

instance DefinePrivacyAnnotation LatencyLogMsg

instance DefineSeverity LatencyLogMsg where
    defineSeverity ev = case ev of
        MsgStartMeasurement _ -> Info
        MsgStopMeasurement _ -> Info

instance ToText LatencyLog where
    toText (LatencyLog name msg) =
        fmt $ "Measuring latency of "+|name|+": "+|toText msg|+""

instance ToText LatencyLogMsg where
    toText = \case
        MsgStartMeasurement t0 ->
            fmt $ "Started at "+|t0|+""
        MsgStopMeasurement t1 ->
            fmt $ "Finished at "+|t1|+""
