{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.Monitoring.Http.Client
    ( withHttpClient
    , RunQuery (..)
    , Query (..)
    , MsgClient (..)
    , AnyQuery (..)
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.Monitoring.Http.API
    ( API
    , ApiT (..)
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Phase
    ( History
    )
import Control.Monad
    ( unless
    )
import Control.Monad.Cont
    ( ContT (..)
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
import Network.HTTP.Client
    ( ManagerSettings (..)
    , defaultManagerSettings
    , newManager
    , responseTimeoutNone
    )
import Network.Socket
    ( PortNumber
    )
import Servant
    ( NoContent
    , Proxy (..)
    , (:<|>) (..)
    )
import Servant.Client
    ( BaseUrl (..)
    , ClientM
    , Scheme (..)
    , client
    , mkClientEnv
    , runClientM
    )
import UnliftIO
    ( MonadUnliftIO
    , UnliftIO (..)
    , askUnliftIO
    , throwIO
    )

-- | Queries that can be sent to the monitoring server via HTTP.
data Query a where
    ReadyQ :: Query Bool
    ObserveQ :: Query (History, MonitorState)
    StepQ :: Query ()
    SwitchQ :: Query MonitorState

data Client = Client
    { ready :: ClientM Bool
    , observe :: ClientM (ApiT (History, MonitorState))
    , step :: ClientM NoContent
    , switch :: ClientM (ApiT MonitorState)
    }

mkClient :: Client
mkClient =
    let ready :<|> step :<|> switch :<|> observe = client (Proxy @API)
    in  Client{..}

-- | A showable existential wrapper around a 'Query' value, for logging purposes.
data AnyQuery = forall a. Show a => AnyQuery (Query a)

instance Show AnyQuery where
    show (AnyQuery ReadyQ) = "Ready"
    show (AnyQuery ObserveQ) = "Observe"
    show (AnyQuery StepQ) = "Step"
    show (AnyQuery SwitchQ) = "Switch"

-- | Run any query against the monitoring server.
newtype RunQuery m = RunQuery (forall a. Show a => Query a -> m a)

-- | Messages that can be logged by the http client.
data MsgClient
    = MsgClientStart
    | MsgClientReq AnyQuery
    | MsgClientRetry AnyQuery
    | MsgClientDone
    deriving stock (Show)

-- | Produce a closure over the http client of an http monitoring server that
-- can be used to query the server.
withHttpClient
    :: MonadUnliftIO m
    => Tracer m MsgClient
    -- ^ how to trace the http client operations
    -> PortNumber
    -- ^ Monitoring port to attach to (http://localhost is hardcoded)
    -> ContT () m (RunQuery m)
withHttpClient tracer httpPort = ContT $ \continue -> do
    let tr = traceWith tracer
    tr MsgClientStart
    UnliftIO unlift <- askUnliftIO
    let url = BaseUrl Http "localhost" (fromIntegral httpPort) ""
    manager <-
        liftIO
            $ newManager
            $ defaultManagerSettings
                { managerResponseTimeout = responseTimeoutNone
                }
    let
        query :: ClientM a -> IO a
        query f = do
            r <- runClientM f $ mkClientEnv manager url
            either throwIO pure r
        Client{ready, observe, step, switch} = mkClient
    continue $ RunQuery $ \request -> do
        tr $ MsgClientReq $ AnyQuery request
        liftIO $ case request of
            ReadyQ -> recoverAll retryPolicy
                $ \rt -> do
                    unless (firstTry rt)
                        $ unlift
                        $ tr
                        $ MsgClientRetry
                        $ AnyQuery request
                    query ready
            ObserveQ -> unApiT <$> query observe
            StepQ -> query step $> ()
            SwitchQ -> unApiT <$> query switch

    tr MsgClientDone

retryPolicy :: RetryPolicyM IO
retryPolicy = capDelay (60 * oneSecond) $ exponentialBackoff oneSecond
  where
    oneSecond = 1_000_000 :: Int

firstTry :: RetryStatus -> Bool
firstTry (RetryStatus 0 _ _) = True
firstTry _ = False
