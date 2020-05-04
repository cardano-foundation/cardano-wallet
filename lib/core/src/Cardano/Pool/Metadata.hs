{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- API and corresponding client for dealing with a metadata aggregation server
-- in Shelley. Such servers follow an OpenAPI specification, and on existing
-- implementation written in Haskell is available at:
--
-- - https://github.com/input-output-hk/smash
--
-- This module is expected to be mostly used qualified as 'Metadata' to give
-- context to the exposed functions and data-types.
module Cardano.Pool.Metadata
    ( Api
    , Client(..)
    , mkClientIO
    , refresh

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
import Control.Monad
    ( forM_ )
import Data.Proxy
    ( Proxy (..) )
import Data.Time.Clock
    ( NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime )
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
newtype Client api m = Client
    { getStakePoolMetadata
        :: PoolId
        -> m (Either ClientError StakePoolOffChainMetadata)
    }

mkClientIO
    :: Manager
    -> BaseUrl
    -> Client Api IO
mkClientIO mgr baseUrl = Client
    { getStakePoolMetadata = \pid ->
        fmap getApiT <$> run (getMetadata (ApiT pid))
    }
  where
    run :: ClientM a -> IO (Either ClientError a)
    run query = runClientM query (mkClientEnv mgr baseUrl)

    getMetadata =
        client (Proxy @Api)

-- | A configuration for managing metadata with the aggregation server.
-- Callbacks and parameterized effects allows for easier testing while a real
-- specialization would wire a database connector in here.
data MetadataConfig (m :: * -> *) = MetadataConfig
    { saveMetadata
        :: PoolId -> (Maybe StakePoolOffChainMetadata, UTCTime) -> m ()
        -- ^ A callback action for storing an off-chain metadata. The callback
        -- may be called with 'Nothing' to store that no metadata were found for
        -- a particular 'PoolId; this allows for not constantly re-fetching data
        -- for pools that are known to have no metadata.

    , getModificationTime
        :: PoolId -> m (Maybe UTCTime)
        -- ^ Action for fetching the last modification time of a cached result.
        -- 'Nothing' is expected when there's no cached result.

    , apiClient
        :: Client Api m
        -- ^ A client constructed with 'mkClient' for interacting with a
        -- metadata aggregation server.

    , cacheTTL
        :: NominalDiffTime
        -- ^ A constant for the maximum age of cached registry metadatabefore
        -- it's considered to be stale.
    }

-- | Refresh metadata for a list of pool ids. Where metadata are cached is
-- out of the scope for this function and provided through callbacks.
--
-- TODO: Add some typed log messages in here.
refresh
    :: MetadataConfig IO
        -- ^ Configuration and callbacks for persistence of metadata.

    -> [PoolId]
        -- ^ A list of pool ids for which metadata need to be fetched or
        -- refreshed.
    -> IO ()
refresh cfg pids = do
    now <- getCurrentTime
    forM_ pids $ \pid -> getModificationTime pid >>= \case
        Just timestamp | diffUTCTime now timestamp < cacheTTL -> do
            pure ()
        _expiredOrNotCached -> do
            meta <- either onClientError (pure . Just) =<< getStakePoolMetadata pid
            saveMetadata (meta, now)
  where
    MetadataConfig{saveMetadata,getModificationTime,apiClient,cacheTTL} = cfg
    Client{getStakePoolMetadata} = apiClient
