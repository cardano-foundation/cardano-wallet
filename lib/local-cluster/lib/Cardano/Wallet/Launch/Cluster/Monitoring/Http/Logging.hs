{-# LANGUAGE DerivingStrategies #-}

module Cardano.Wallet.Launch.Cluster.Monitoring.Http.Logging
    ( MsgHttpMonitoring (..)
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.Monitoring.Http.Client
    ( MsgClient
    )
import Network.Socket
    ( PortNumber
    )

-- | Messages for the HTTP monitoring service
data MsgHttpMonitoring
    = MsgHttpMonitoringPort PortNumber
    | MsgHttpMonitoringQuery MsgClient
    | MsgHttpMonitoringServerStarted
    | MsgHttpMonitoringDone
    deriving stock (Show)
