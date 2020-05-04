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

    -- * Refreshing Metadata
    , refresh
    , MetadataRegistryLog (..)

    -- * Re-export
    , BaseUrl (..)
    , Scheme (..)
    , Manager
    , defaultManagerSettings
    , newManager
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Wallet.Api.Types
    ( ApiT (..) )
import Cardano.Wallet.Primitive.Types
    ( PoolId, StakePoolOffChainMetadata (..) )
import Control.Monad
    ( forM_ )
import Control.Tracer
    ( Tracer, traceWith )
import Data.Functor
    ( (<&>) )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( ToText (..) )
import Data.Time.Clock
    ( NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime )
import Fmt
    ( pretty )
import Network.HTTP.Client
    ( Manager, defaultManagerSettings, newManager )
import Network.HTTP.Types
    ( status404 )
import Servant
    ( (:>), Capture, Get, JSON )
import Servant.Client
    ( BaseUrl (..)
    , ClientError (..)
    , ClientM
    , Scheme (..)
    , client
    , mkClientEnv
    , responseStatusCode
    , runClientM
    )

import qualified Data.Text as T

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
        -> m (Either ClientError (Maybe StakePoolOffChainMetadata))
    }

mkClientIO
    :: Manager
    -> BaseUrl
    -> Client Api IO
mkClientIO mgr baseUrl = Client
    { getStakePoolMetadata = \pid -> do
        run (getMetadata (ApiT pid)) <&> \case
            Right (ApiT meta) ->
                Right (Just meta)

            Left (FailureResponse _ res) | responseStatusCode res == status404 -> do
                Right Nothing

            Left e ->
                Left e
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
    :: Tracer IO MetadataRegistryLog
        -- ^ Logging object for capturing events in the 'refresh' function.
        --
    -> MetadataConfig IO
        -- ^ Configuration and callbacks for persistence of metadata.

    -> [PoolId]
        -- ^ A list of pool ids for which metadata need to be fetched or
        -- refreshed.
    -> IO ()
refresh tr cfg pids = do
    now <- getCurrentTime
    forM_ pids $ \pid -> getModificationTime pid >>= \case
        Just timestamp | diffUTCTime now timestamp < cacheTTL -> do
            traceWith tr $ MsgUsingCached pid timestamp
        _expiredOrNotCached -> getStakePoolMetadata pid >>= \case
            Right meta -> do
                traceWith tr $ MsgRefreshingMetadata pid (meta, now)
                saveMetadata pid (meta, now)
            Left e ->
                traceWith tr $ MsgUnexpectedError e
  where
    MetadataConfig{saveMetadata,getModificationTime,apiClient,cacheTTL} = cfg
    Client{getStakePoolMetadata} = apiClient

-- | Capture log events for the 'refresh' function.
data MetadataRegistryLog
    = MsgUsingCached PoolId UTCTime
    | MsgRefreshingMetadata PoolId (Maybe StakePoolOffChainMetadata, UTCTime)
    | MsgUnexpectedError ClientError
    deriving (Show, Eq)

instance HasPrivacyAnnotation MetadataRegistryLog
instance HasSeverityAnnotation MetadataRegistryLog where
    getSeverityAnnotation = \case
        MsgUsingCached{} -> Debug
        MsgRefreshingMetadata{} -> Debug
        MsgUnexpectedError{} -> Warning

instance ToText MetadataRegistryLog where
    toText = \case
        MsgUsingCached pid time -> T.unwords
            [ "Using cached result for"
            , pretty pid
            , "last modified at"
            , T.pack (show time)
            ]
        MsgRefreshingMetadata pid (meta, time) -> T.unwords
            [ "Setting metadata for "
            , pretty pid
            , "="
            , maybe "ø" (T.pack . show) meta
            , ", last modified at"
            , T.pack (show time)
            ]
        MsgUnexpectedError e -> T.unwords
            [ "Unexpected error from the aggregation server:"
            , T.pack (show e)
            ]
