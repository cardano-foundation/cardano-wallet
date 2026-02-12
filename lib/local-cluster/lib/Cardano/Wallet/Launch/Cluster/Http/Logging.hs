{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Launch.Cluster.Http.Logging
    ( MsgHttpService (..)
    )
where

import Cardano.BM.Tracing
    ( HasSeverityAnnotation (..)
    , Severity (..)
    )
import Cardano.Wallet.Launch.Cluster.Http.Client
    ( MsgClient
    )
import Data.Text.Class
    ( ToText (..)
    )
import Network.Socket
    ( PortNumber
    )
import Prelude

-- | Messages for the HTTP monitoring service
data MsgHttpService
    = MsgHttpServicePort PortNumber
    | MsgHttpServiceQuery MsgClient
    | MsgHttpServiceServerStarted
    | MsgHttpServiceServerStopped
    | MsgHttpServiceClientStarted
    | MsgHttpServiceClientStopped
    | MsgHttpServiceDone
    deriving stock (Show)

instance ToText MsgHttpService where
    toText = \case
        MsgHttpServicePort port ->
            "HTTP monitoring service started on port " <> toText (show port)
        MsgHttpServiceQuery msgClient ->
            "HTTP monitoring query: " <> toText msgClient
        MsgHttpServiceServerStarted ->
            "HTTP monitoring server started"
        MsgHttpServiceServerStopped ->
            "HTTP monitoring server stopped"
        MsgHttpServiceClientStarted ->
            "HTTP monitoring client started"
        MsgHttpServiceClientStopped ->
            "HTTP monitoring client stopped"
        MsgHttpServiceDone ->
            "HTTP monitoring done"

instance HasSeverityAnnotation MsgHttpService where
    getSeverityAnnotation = \case
        MsgHttpServicePort _ -> Info
        MsgHttpServiceQuery _ -> Info
        MsgHttpServiceServerStarted -> Info
        MsgHttpServiceServerStopped -> Info
        MsgHttpServiceClientStarted -> Info
        MsgHttpServiceClientStopped -> Info
        MsgHttpServiceDone -> Info
