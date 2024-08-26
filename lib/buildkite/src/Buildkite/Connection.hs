{-# LANGUAGE FlexibleInstances #-}
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
    , bailout
    , skip410
    , handleClientError
    )
where

import Prelude

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
import Network.HTTP.Types
    ( status410
    )
import Servant.Client
    ( BaseUrl (..)
    , ClientEnv
    , ClientError (..)
    , ClientM
    , ResponseF (..)
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

-- | The name of the organization
newtype OrganizationName = OrganizationName Text

-- | How to handle http client errors
newtype HandleClientError
    = HandleClientError (forall a. IO (Either ClientError a) -> IO (Maybe a))

-- | The way to create a query type
type Connector = OrganizationName -> PipelineName -> HandleClientError -> Query

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
        in  Query (runQuery h) withLockingAuthPipeline withAuthPipeline

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

data SkipOrAbort = Skip | Abort

bailout :: HandleClientError
bailout = handleClientError $ const Abort

handleClientError :: (ClientError -> SkipOrAbort) -> HandleClientError
handleClientError g = HandleClientError $ \f -> do
    res <- f
    case res of
        Left e -> case g e of
            Abort -> error $ show e
            Skip -> pure Nothing
        Right a -> pure $ Just a

skip410 :: HandleClientError
skip410 = handleClientError $ \case
    FailureResponse _ (Response s _ _ _)
        | s == status410 -> Skip
    _ -> Abort
