{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.Http.Monitor.Client
    ( RunMonitorQ (..)
    , MonitorQ (..)
    , MsgMonitorClient (..)
    , AnyMonitorQ (..)
    , newRunQuery
    , mkMonitorClient
    , recovering
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.Http.Monitor.API
    ( ApiT (..)
    , ObserveAPI
    , ReadyAPI
    , StepAPI
    , SwitchAPI
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Phase
    ( History
    )
import Control.Monad
    ( unless
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Monitoring.Tracing
    ( MonitorState
    )
import Control.Retry
    ( RetryPolicyM
    , RetryStatus (..)
    , capDelay
    , exponentialBackoff
    , recoverAll
    )
import Control.Tracer
    ( Tracer
    , traceWith
    )
import Data.Functor
    ( ($>)
    )
import Data.Text.Class
    ( ToText (..)
    )
import Servant
    ( NoContent
    , Proxy (..)
    )
import Servant.Client
    ( ClientM
    , client
    )
import UnliftIO
    ( MonadUnliftIO
    , UnliftIO (..)
    , askUnliftIO
    )

-- | Queries that can be sent to the monitoring server via HTTP.
data MonitorQ a where
    ReadyQ :: MonitorQ Bool
    ObserveQ :: MonitorQ (History, MonitorState)
    StepQ :: MonitorQ ()
    SwitchQ :: MonitorQ MonitorState

data MonitorClient = MonitorClient
    { ready :: ClientM Bool
    , observe :: ClientM (ApiT (History, MonitorState))
    , step :: ClientM NoContent
    , switch :: ClientM (ApiT MonitorState)
    }

mkMonitorClient
    :: MonitorClient
mkMonitorClient =
    let ready = client (Proxy @ReadyAPI)
        observe = client (Proxy @ObserveAPI)
        step = client (Proxy @StepAPI)
        switch = client (Proxy @SwitchAPI)
    in  MonitorClient{..}

-- | A showable existential wrapper around a 'Query' value, for logging purposes.
data AnyMonitorQ = forall a. Show a => AnyQuery (MonitorQ a)

instance Show AnyMonitorQ where
    show (AnyQuery ReadyQ) = "Ready"
    show (AnyQuery ObserveQ) = "Observe"
    show (AnyQuery StepQ) = "Step"
    show (AnyQuery SwitchQ) = "Switch"

-- | Run any query against the monitoring server.
newtype RunMonitorQ m = RunQuery (forall a. Show a => MonitorQ a -> m a)

-- | Messages that can be logged by the http client.
data MsgMonitorClient
    = MsgMonitorClientReq AnyMonitorQ
    | MsgMonitorClientRetry AnyMonitorQ
    deriving stock (Show)

instance ToText MsgMonitorClient where
    toText = \case
        MsgMonitorClientReq q -> "Client request: " <> toText (show q)
        MsgMonitorClientRetry q -> "Client retry: " <> toText (show q)

newRunQuery
    :: forall m
        . MonadUnliftIO m
    => (forall a. ClientM a -> IO a)
    -> Tracer m MsgMonitorClient
    -> MonitorClient
    -> m (RunMonitorQ m)
newRunQuery query tr MonitorClient{ready, observe, step, switch} =
    do
        UnliftIO unlift <- askUnliftIO
        pure $ RunQuery $ \request -> do
            traceWith tr $ MsgMonitorClientReq $ AnyQuery request
            let f = unlift
                    . traceWith tr . MsgMonitorClientRetry
                    $ AnyQuery request
            liftIO $ recovering f $ case request of
                ReadyQ -> query ready
                ObserveQ -> unApiT <$> query observe
                StepQ -> query step $> ()
                SwitchQ -> unApiT <$> query switch

recovering :: IO () -> IO a -> IO a
recovering f doing = recoverAll retryPolicy
    $ \rt -> do
        unless (firstTry rt) f
        doing
    where
        retryPolicy :: RetryPolicyM IO
        retryPolicy = capDelay (60 * oneSecond) $ exponentialBackoff oneSecond
        oneSecond = 1_000_000 :: Int
        firstTry :: RetryStatus -> Bool
        firstTry (RetryStatus 0 _ _) = True
        firstTry _ = False
