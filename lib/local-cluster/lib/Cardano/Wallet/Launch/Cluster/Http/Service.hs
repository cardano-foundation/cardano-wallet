{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module Cardano.Wallet.Launch.Cluster.Http.Service
    ( MsgHttpService (..)
    , ServiceConfiguration (..)
    , withService
    , withServiceServer
    , withServiceClient
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.Config
    ( Config
    )
import Cardano.Wallet.Launch.Cluster.Http.Client
    ( withHttpClient
    )
import Cardano.Wallet.Launch.Cluster.Http.Faucet.Client
    ( RunFaucetQ
    )
import Cardano.Wallet.Launch.Cluster.Http.Faucet.Server
    ( NodeConnVar
    , mkFaucetHandlers
    )
import Cardano.Wallet.Launch.Cluster.Http.Logging
    ( MsgHttpService (..)
    )
import Cardano.Wallet.Launch.Cluster.Http.Monitor.Client
    ( RunMonitorQ
    )
import Cardano.Wallet.Launch.Cluster.Http.Server
    ( mkControlHandlers
    , withHttpServer
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Phase
    ( History (..)
    , Phase
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.TimedMonitor
    ( timedMonitor
    )
import Cardano.Wallet.Network.Ports
    ( getRandomPort
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId
    , SNetworkId
    )
import Control.Monad.Cont
    ( ContT (..)
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Monitoring.Monitor
    ( monitorTracer
    )
import Control.Monitoring.Tracing
    ( MonitorState
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
import Network.Socket
    ( PortNumber
    )

-- | Configuration for the monitoring service
data ServiceConfiguration = ServiceConfiguration
    { servicePort :: Maybe PortNumber
    -- ^ The port to run the monitoring service on
    -- If `Nothing`, a random port will be chosen
    , monitorInitialState :: MonitorState
    -- ^ The initial state of the monitor
    }
    deriving stock (Show)

-- | Start a monitoring service, returning a tracer to write `Phase` values to
-- and a function to interact with the monitoring service
withService
    :: HasSNetworkId n
    => SNetworkId n
    -> NodeConnVar
    -> Config
    -> Tracer IO MsgHttpService
    -- ^ Tracer for logging the monitoring operations
    -> ServiceConfiguration
    -- ^ Configuration for the monitoring service
    -> ContT () IO (Tracer IO Phase, (RunMonitorQ IO, RunFaucetQ IO))
withService network conn clusterConfig tr config = do
    (port, tracer) <- withServiceServer network conn clusterConfig tr config
    queries <- withServiceClient network port tr
    pure (tracer, queries)

withServiceClient
    :: HasSNetworkId n
    => SNetworkId n
    -> PortNumber
    -> Tracer IO MsgHttpService
    -> ContT r IO (RunMonitorQ IO, RunFaucetQ IO)
withServiceClient network port tr = do
    liftIO $ traceWith tr MsgHttpServiceClientStarted
    queries <- withHttpClient network (MsgHttpServiceQuery >$< tr) port
    ContT $ \k -> do
        r <- k queries
        traceWith tr MsgHttpServiceClientStopped
        pure r

withServiceServer
    :: HasSNetworkId n
    => SNetworkId n
    -> NodeConnVar
    -> Config
    -> Tracer IO MsgHttpService
    -> ServiceConfiguration
    -> ContT r IO (PortNumber, Tracer IO Phase)
withServiceServer network conn clusterConfig tr ServiceConfiguration{..} = do
    monitor <- liftIO $ withTracingState timedMonitor monitorInitialState
    port <- liftIO $ maybe getRandomPort pure servicePort
    liftIO $ traceWith tr $ MsgHttpServicePort port
    withHttpServer
        network
        port
        (mkControlHandlers $ rmap History monitor)
        (mkFaucetHandlers conn clusterConfig)
    liftIO $ traceWith tr MsgHttpServiceServerStarted
    ContT $ \k -> do
        r <- k (port, monitorTracer monitor)
        traceWith tr MsgHttpServiceServerStopped
        pure r
