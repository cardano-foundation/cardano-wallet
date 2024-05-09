{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.Http.Server
    ( withHttpServer
    , httpServer
    , mkControlHandlers
    , ControlHandlers
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.Http.API
    ( API
    )
import Cardano.Wallet.Launch.Cluster.Http.Faucet.Server
    ( FaucetHandlers (..)
    , mkFaucetServer
    )
import Cardano.Wallet.Launch.Cluster.Http.Monitor.Server
    ( ControlHandlers
    , mkControlHandlers
    , mkControlServer
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId
    , SNetworkId
    )
import Control.Monad
    ( (<=<)
    )
import Control.Monad.Cont
    ( ContT (..)
    )
import Network.Socket
    ( PortNumber
    )
import Network.Wai.Handler.Warp
    ( run
    )
import Servant
    ( Application
    , Proxy (..)
    , (:<|>) (..)
    )
import Servant.Server
    ( serve
    )
import UnliftIO
    ( async
    , link
    )

server
    :: forall n
     . HasSNetworkId n
    => SNetworkId n
    -> ControlHandlers
    -> FaucetHandlers
    -> Application
server _ handlers appHandlers =
    serve (Proxy @(API n))
        $ mkControlServer handlers
            :<|> mkFaucetServer appHandlers

-- | Run a HTTP server that serves the monitoring API
httpServer
    :: HasSNetworkId n
    => SNetworkId n
    -> PortNumber
    -> ControlHandlers
    -> FaucetHandlers
    -> IO ()
httpServer api port handlers appHandlers =
    run (fromIntegral port) $ server api handlers appHandlers

-- | Start a HTTP server in a linked thread that serves the monitoring API
withHttpServer
    :: HasSNetworkId n
    => SNetworkId n
    -> PortNumber
    -> ControlHandlers
    -> FaucetHandlers
    -> ContT r IO ()
withHttpServer api port handlers app = ContT $ \k -> do
    link <=< async $ httpServer api port handlers app
    k ()
