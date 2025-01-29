{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Buildkite.Connection
    ( PipelineName (..)
    , OrganizationName (..)

      -- * Connector
    , Connector
    , newConnector

      -- * Error handling
    , HandleClientError (..)
    )
where

import Prelude

import Buildkite.API
    ( HandleClientError (..)
    , Skipping (..)
    , handleClientError
    )
import Buildkite.Client
    ( Query (..)
    )
import Buildkite.LimitsLock
    ( LimitsLockLog
    , newLimitsLock
    )
import Control.Tracer
    ( Tracer
    )
import Data.Functor
    ( (<&>)
    )
import Data.String
    ( IsString
    )
import Data.Text
    ( Text
    )
import Network.HTTP.Client
    ( Manager
    , ManagerSettings (..)
    , Request (..)
    , newManager
    )
import Network.HTTP.Client.TLS
    ( tlsManagerSettings
    )
import Servant.Client
    ( BaseUrl (..)
    , ClientEnv
    , ClientM
    , Scheme (..)
    , mkClientEnv
    , runClientM
    )
import System.Environment
    ( getEnv
    )

import qualified Data.Text as T

buildkiteDomain :: String
buildkiteDomain = "api.buildkite.com"

buildkitePort :: Int
buildkitePort = 443

-- | The name of the pipeline
newtype PipelineName = PipelineName Text
    deriving (IsString)

-- | The name of the organization
newtype OrganizationName = OrganizationName Text
    deriving (IsString)

-- | How to handle http client errors

-- | The way to create a query type
type Connector =
    OrganizationName -> PipelineName -> Skipping -> Query

-- | Create a new connect computer
newConnector
    :: String
    -- ^ Environment variable for the API token
    -> Int
    -- ^ The maximum remaining request per minute, before locking (< 200)
    -> Tracer IO LimitsLockLog
    -- ^ The tracer for the limits lock events
    -> IO Connector
newConnector tokenEnvVar lockLimit lockTracer = do
    apiToken <- getEnv tokenEnvVar <&> \t -> "Bearer " ++ t
    manager <- newManager $ stripAuthOnRedirect tlsManagerSettings
    let env = buildkiteEnv manager
        runQuery :: HandleClientError -> ClientM a -> IO (Maybe a)
        runQuery (HandleClientError f) action = f $ runClientM action env
    limitsLock <- newLimitsLock lockTracer lockLimit
    pure $ \(OrganizationName organizationSlug) (PipelineName slug) h ->
        let withLockingAuthPipeline action =
                action limitsLock (Just $ T.pack apiToken) organizationSlug slug
            withAuthPipeline action =
                action (Just $ T.pack apiToken) organizationSlug slug
        in  Query
                (runQuery $ handleClientError h)
                withLockingAuthPipeline
                withAuthPipeline

-- the environment for the buildkite API
buildkiteEnv :: Manager -> ClientEnv
buildkiteEnv manager =
    mkClientEnv manager
        $ BaseUrl Https buildkiteDomain buildkitePort ""

-- necessary for the buildkite API on getting artifacts content (redirect to AWS)
stripAuthOnRedirect :: ManagerSettings -> ManagerSettings
stripAuthOnRedirect settings =
    settings
        { managerModifyRequest = \req -> do
            pure
                $ req
                    { shouldStripHeaderOnRedirect =
                        \case
                            "Authorization" -> True
                            _ -> False
                    }
        }
