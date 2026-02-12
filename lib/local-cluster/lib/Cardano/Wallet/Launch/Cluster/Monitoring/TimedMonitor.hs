{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Cardano.Wallet.Launch.Cluster.Monitoring.TimedMonitor
    ( timedMonitor
    )
where

import Control.Concurrent.Class.MonadSTM
    ( MonadSTM
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Monitoring.Folder
    ( mkTracingFromFold
    )
import Control.Monitoring.Monitor
    ( Monitor
    , mkMonitor
    )
import Control.Monitoring.Tracing
    ( AnyTracing (AnyTracing)
    , StateS (..)
    , Tracing
    )
import Data.Time
    ( UTCTime
    , getCurrentTime
    )
import Prelude

import qualified Control.Foldl as F

-- | A monitor that patch the current time to the value being monitored.
timedMonitor
    :: forall w m a
     . (MonadSTM m, MonadIO m)
    => StateS w
    -> m (Monitor m a [(UTCTime, a)])
timedMonitor initialState = do
    let tracer :: Tracing w (UTCTime, a) [(UTCTime, a)]
        tracer = mkTracingFromFold F.list initialState
    mkMonitor
        (AnyTracing initialState tracer)
        (\x -> (,x) <$> liftIO getCurrentTime)
