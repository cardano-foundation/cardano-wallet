{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Pool.Metadata
    ( Api
    , Client(..)
    , mkClient

    -- * Re-export
    , BaseUrl (..)
    , Scheme (..)
    , Manager
    , defaultManagerSettings
    , newManager
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiT (..) )
import Cardano.Wallet.Primitive.Types
    ( PoolId, StakePoolOffChainMetadata (..) )
import Data.Proxy
    ( Proxy (..) )
import Network.HTTP.Client
    ( Manager, defaultManagerSettings, newManager )
import Servant
    ( (:>), Capture, Get, JSON )
import Servant.Client
    ( BaseUrl (..)
    , ClientError (..)
    , ClientM
    , Scheme (..)
    , client
    , mkClientEnv
    , runClientM
    )

--
-- Api
--

type Api
    =  "api"
    :> "v1"
    :> "metadata"
    :> Capture "hash" (ApiT PoolId)
    :> Get '[JSON] (ApiT StakePoolOffChainMetadata)

--
-- Client
--

-- | A client for fetching metadata from an Aggregation server.
--
-- See also 'newClient' to construct a client.
newtype Client api = Client
    { getStakePoolMetadata
        :: PoolId
        -> IO (Either ClientError StakePoolOffChainMetadata)
    }

mkClient
    :: Manager
    -> BaseUrl
    -> Client Api
mkClient mgr baseUrl = Client
    { getStakePoolMetadata = \pid ->
        fmap getApiT <$> run (getMetadata (ApiT pid))
    }
  where
    run :: ClientM a -> IO (Either ClientError a)
    run query = runClientM query (mkClientEnv mgr baseUrl)

    getMetadata =
        client (Proxy @Api)
