{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module Cardano.Wallet.Launch.Cluster.Monitoring.Monitor
    ( MsgHttpMonitoring (..)
    , MonitorConfiguration (..)
    , withMonitoring
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.Monitoring.Http.Client
    ( RunQuery
    , withHttpClient
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Http.Logging
    ( MsgHttpMonitoring (..)
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Http.Server
    ( mkHandlers
    , withHttpServer
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Phase
    ( History (..)
    , Phase
    )
import Cardano.Wallet.Network.Ports
    ( getRandomPort
    )
import Control.Concurrent.Class.MonadSTM
    ( MonadSTM
    )
import Control.Monad.Cont
    ( ContT (..)
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Monitoring.Folder
import Control.Monitoring.Monitor
    ( Monitor
    , mkMonitor
    , monitorTracer
    )
import Control.Monitoring.Tracing
    ( AnyTracing (AnyTracing)
    , MonitorState
    , StateS (..)
    , Tracing
    , withTracingState
    )
import Control.Tracer
    ( Tracer (..)
    , traceWith
    )
import Data.Functor.Contravariant
    ( (>$<)
    )
import Data.Profunctor
    ( Profunctor (..)
    )
import Data.Time
    ( UTCTime
    , getCurrentTime
    )
import Network.Socket
    ( PortNumber
    )

import qualified Control.Foldl as F

-- | Configuration for the monitoring service
data MonitorConfiguration = MonitorConfiguration
    { monitorPort :: Maybe PortNumber
    -- ^ The port to run the monitoring service on
    -- If `Nothing`, a random port will be chosen
    , monitorInitialState :: MonitorState
    -- ^ The initial state of the monitor
    }
    deriving stock (Show)

timedMonitor
    :: forall m w a
     . (MonadSTM m, MonadIO m)
    => StateS w
    -> m (Monitor m a [(UTCTime,a)])
timedMonitor initialState = do
    let tracer :: Tracing w (UTCTime, a) [(UTCTime,a)]
        tracer = mkTracingFromFold F.list initialState
    mkMonitor
        (AnyTracing initialState tracer)
        (\x -> (,x) <$> liftIO getCurrentTime)

-- | Start a monitoring service, returning a tracer to write `Phase` values to
-- and a function to interact with the monitoring service
withMonitoring
    :: Tracer IO MsgHttpMonitoring
    -- ^ Tracer for logging the monitoring operations
    -> MonitorConfiguration
    -- ^ Configuration for the monitoring service
    -> ContT () IO (Tracer IO Phase, RunQuery IO)
withMonitoring tr MonitorConfiguration{..} = do
    monitor <- liftIO $ withTracingState timedMonitor monitorInitialState
    port <- liftIO $ maybe getRandomPort pure monitorPort
    liftIO $ traceWith tr $ MsgHttpMonitoringPort port
    withHttpServer port $ mkHandlers $ rmap History monitor
    liftIO $ traceWith tr MsgHttpMonitoringServerStarted
    runQueries <- withHttpClient (MsgHttpMonitoringQuery >$< tr) port
    ContT $ \k -> do
        k (monitorTracer monitor, runQueries)
        traceWith tr MsgHttpMonitoringDone
