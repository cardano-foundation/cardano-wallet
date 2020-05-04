{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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

    -- * Client
    , Client(..)
    , ClientConfig (..)
    , ClientCallbacks (..)
    , newClient
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
import Control.Tracer
    ( Tracer, traceWith )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( ToText (..) )
import Data.Time.Clock
    ( NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime )
import Fmt
    ( pretty )
import GHC.Generics
    ( Generic )
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
        -> m (Maybe StakePoolOffChainMetadata)
    }

-- | A configuration for managing metadata with the aggregation server.
-- Callbacks and parameterized effects allows for easier testing while a real
-- specialization would wire a database connector in here.
data ClientConfig = ClientConfig
    { manager
        :: Manager
        -- ^ An HTTP connection manager.

    , baseUrl
        :: BaseUrl
        -- ^ Url for reaching out to the metadata aggregation server.

    , cacheTTL
        :: NominalDiffTime
        -- ^ A constant for the maximum age of cached registry metadatabefore
        -- it's considered to be stale.
    }

-- | Callbacks interfaces allowing the client to cache and manage cached
-- entities. These would typically be hooked up with a database.
data ClientCallbacks (m :: * -> *) = ClientCallbacks
    { saveMetadata
        :: PoolId -> (Maybe StakePoolOffChainMetadata, UTCTime) -> m ()
        -- ^ A callback action for storing an off-chain metadata. The callback
        -- may be called with 'Nothing' to store that no metadata were found for
        -- a particular 'PoolId; this allows for not constantly re-fetching data
        -- for pools that are known to have no metadata.

    , getCachedMetadata
        :: PoolId -> m (Maybe (Maybe StakePoolOffChainMetadata, UTCTime))
        -- ^ Action for fetching the last modification time of a cached result.
        -- 'Nothing' is expected when there's no cached result.
    }

-- | Create a new HTTP 'Client' in IO with caching support.
newClient
    :: Tracer IO MetadataRegistryLog
    -> ClientConfig
    -> ClientCallbacks IO
    -> Client Api IO
newClient tr ClientConfig{manager,baseUrl,cacheTTL} callbacks =
    Client { getStakePoolMetadata }
  where
    run :: ClientM a -> IO (Either ClientError a)
    run query = runClientM query (mkClientEnv manager baseUrl)

    getFromServer =
        client (Proxy @Api)

    ClientCallbacks{getCachedMetadata,saveMetadata} =
        callbacks

    getStakePoolMetadata
        :: PoolId
        -> IO (Maybe StakePoolOffChainMetadata)
    getStakePoolMetadata pid = do
        now <- getCurrentTime
        getCachedMetadata pid >>= \case
            Just (meta, time) | diffUTCTime now time < cacheTTL -> do
                traceWith tr $ MsgUsingCached pid time
                pure meta

            _expiredOrNotCached ->
                (handleRequest <$> run (getFromServer (ApiT pid))) >>= \case
                    Right meta -> do
                        traceWith tr $ MsgRefreshingMetadata pid (meta, now)
                        saveMetadata pid (meta, now)
                        pure meta
                    Left e -> do
                        traceWith tr $ MsgUnexpectedError e
                        pure Nothing
      where
        handleRequest = \case
            Right (ApiT meta) ->
                Right (Just meta)
            Left (FailureResponse _ res) | responseStatusCode res == status404 ->
                Right Nothing
            Left e ->
                Left e

-- | Capture log events for the Client.
data MetadataRegistryLog
    = MsgUsingCached PoolId UTCTime
    | MsgRefreshingMetadata PoolId (Maybe StakePoolOffChainMetadata, UTCTime)
    | MsgUnexpectedError ClientError
    deriving (Generic, Show, Eq)

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
