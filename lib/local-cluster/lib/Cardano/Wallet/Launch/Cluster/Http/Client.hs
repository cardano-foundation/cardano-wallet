{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Launch.Cluster.Http.Client
    ( withHttpClient
    , MsgClient (..)
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.Http.Faucet.Client
    ( MsgFaucetClient
    , RunFaucetQ
    , mkFaucet
    , newFaucetQ
    )
import Cardano.Wallet.Launch.Cluster.Http.Monitor.Client
    ( MsgMonitorClient
    , RunMonitorQ
    , mkMonitorClient
    , newRunQuery
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId
    , SNetworkId
    )
import Control.Monad.Cont
    ( ContT (..)
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Tracer
    ( Tracer
    , traceWith
    )
import Data.Functor.Contravariant
    ( (>$<)
    )
import Data.Text.Class
    ( ToText (..)
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
import Servant.Client
    ( BaseUrl (..)
    , ClientM
    , Scheme (..)
    , mkClientEnv
    , runClientM
    )
import UnliftIO
    ( MonadUnliftIO
    , throwIO
    )

data MsgClient
    = MsgClientStart
    | MsgClientDone
    | MsgMonitorClient MsgMonitorClient
    | MsgFaucetClient MsgFaucetClient
    deriving stock (Show)

instance ToText MsgClient where
    toText = \case
        MsgClientStart -> "HTTP client started"
        MsgClientDone -> "HTTP client done"
        MsgMonitorClient msg -> toText msg
        MsgFaucetClient msg -> toText msg

-- | Produce a closure over the http client of an http monitoring server that
-- can be used to query the server.
withHttpClient
    :: (MonadUnliftIO m, HasSNetworkId n)
    => SNetworkId n
    -> Tracer m MsgClient
    -- ^ how to trace the http client operations
    -> PortNumber
    -- ^ Monitoring port to attach to (http://localhost is hardcoded)
    -> ContT r m (RunMonitorQ m, RunFaucetQ m)
withHttpClient networkId tracer httpPort = ContT $ \continue -> do
    let tr = traceWith tracer
    tr MsgClientStart
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
    runQuery <- newRunQuery query (MsgMonitorClient >$< tracer) mkMonitorClient
    runFaucet <-
        newFaucetQ
            query
            (MsgFaucetClient >$< tracer)
            $ mkFaucet networkId
    r <- continue (runQuery, runFaucet)
    tr MsgClientDone
    pure r
