{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.Http.Faucet.Client
    ( newFaucetQ
    , mkFaucet
    , FaucetQ (..)
    , AnyFaucetQ (..)
    , RunFaucetQ (..)
    , Faucet
    , MsgFaucetClient (..)
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.Http.Faucet.API
    ( SendFaucetAssetsAPI
    )
import Cardano.Wallet.Launch.Cluster.Http.Faucet.SendFaucetAssets
    ( SendFaucetAssets
    , WithNetwork (..)
    )
import Cardano.Wallet.Launch.Cluster.Http.Monitor.Client
    ( recovering
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId
    , SNetworkId
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Tracer
    ( Tracer
    , traceWith
    )
import Data.Data
    ( Proxy (Proxy)
    )
import Data.Functor
    ( ($>)
    )
import Data.Text.Class
    ( ToText (..)
    )
import Servant
    ( NoContent
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

-- | Queries that can be run against the local cluster
data FaucetQ a where
    SendFaucetAssetsQ :: SendFaucetAssets -> FaucetQ ()

-- | Existential wrapper for any application query that has a show instance
data AnyFaucetQ = forall a. Show a => AnyFaucetQ (FaucetQ a)

instance Show AnyFaucetQ where
    show (AnyFaucetQ (SendFaucetAssetsQ _)) = "SendFaucetAssets"

-- | Opaque record of the client application
newtype Faucet n = Faucet
    { sendFaucetAssets :: WithNetwork SendFaucetAssets n -> ClientM NoContent
    }

-- | Construct the client application given the network id witness
mkFaucet :: forall n. HasSNetworkId n => SNetworkId n -> Faucet n
mkFaucet _ =
    Faucet
        { sendFaucetAssets = client (Proxy @(SendFaucetAssetsAPI n))
        }

newtype MsgFaucetClient = MsgFaucetRequest AnyFaucetQ
    deriving stock (Show)

instance ToText MsgFaucetClient where
    toText (MsgFaucetRequest q) = "Faucet request: " <> toText (show q)

-- | Run any query against the monitoring server.
newtype RunFaucetQ m
    = RunFaucetQ (forall a. Show a => FaucetQ a -> m a)

-- | Construct the run function for the client application
newFaucetQ
    :: MonadUnliftIO m
    => (forall a. ClientM a -> IO a)
    -> Tracer m MsgFaucetClient
    -> Faucet n
    -> m (RunFaucetQ m)
newFaucetQ query tr Faucet{..} = do
    UnliftIO unlift <- askUnliftIO
    pure
        $ RunFaucetQ
        $ \request -> do
            let f =
                    unlift
                        . traceWith tr
                        . MsgFaucetRequest
                        $ AnyFaucetQ request
            liftIO $ recovering f $ case request of
                SendFaucetAssetsQ assets ->
                    liftIO
                        $ query
                        $ sendFaucetAssets (WithNetwork assets) $> ()
