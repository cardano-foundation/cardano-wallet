{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Buildkite.API
    ( Artifact (..)
    , ArtifactURL (..)
    , Build (..)
    , Job (..)
    , Time (..)
    , renderDate
    , parseDate
    , fetchBuilds
    , fetchBuildsOfBranch
    , fetchArtifacts
    , jobId
    , artifactId
    , WithLockingAuthPipeline
    , WithAuthPipeline
    , WithLocking
    , overJobs
    , GetArtifact
    , newLimitsLock
    , LimitsLock
    , withLimitsLock
    )
where

import Prelude

import Buildkite.LimitsLock
    ( LimitsLock (..)
    , newLimitsLock
    )
import Control.Monad
    ( (>=>)
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    )
import Data.Data
    ( Proxy (..)
    )
import Data.Text
    ( Text
    )
import Data.Time
    ( UTCTime
    , defaultTimeLocale
    , formatTime
    , parseTimeM
    )
import GHC.Generics
    ( Generic
    )
import GHC.Stack
    ( HasCallStack
    )
import Servant.API
    ( Capture
    , Get
    , Header
    , Headers (getResponse)
    , JSON
    , QueryParam
    , ResponseHeader (..)
    , lookupResponseHeader
    , (:>)
    )
import Servant.Client
    ( ClientM
    , client
    )

import qualified Data.Text as T

newtype Time = Time
    { time :: UTCTime
    }
    deriving (Show)

parseDate :: MonadFail m => [Char] -> m UTCTime
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"

renderDate :: UTCTime -> Text
renderDate = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"

instance FromJSON Time where
    parseJSON = parseJSON >=> parseDate >=> pure . Time
instance ToJSON Time where
    toJSON = toJSON . renderDate . time

data Job = Job
    { id :: Text
    , name :: Maybe Text
    , step_key :: Maybe Text
    , finished_at :: Maybe Time
    , created_at :: Maybe Time
    }
    deriving (Show, Generic)

instance FromJSON Job
instance ToJSON Job

jobId :: Job -> Text
jobId (Job i _ _ _ _) = i

data Build l = Build
    { number :: Int
    , branch :: Text
    , jobs :: l Job
    }
    deriving (Generic)

deriving instance (Show (l Job)) => Show (Build l)

overJobs :: Build l -> (l Job -> l' Job) -> Build l'
overJobs (Build n b j) f = Build n b (f j)

data Artifact = Artifact
    { filename :: Text
    , id :: Text
    , job_id :: Text
    , state :: Text
    , file_size :: Int
    , path :: Text
    }
    deriving (Show, Generic)

artifactId :: Artifact -> Text
artifactId (Artifact _ i _ _ _ _) = i

newtype ArtifactURL = ArtifactURL
    { url :: Text
    }
    deriving (Show, Generic)

instance FromJSON (l Job) => FromJSON (Build l)
instance ToJSON (l Job) => ToJSON (Build l)

instance FromJSON Artifact
instance ToJSON Artifact

instance FromJSON ArtifactURL

type PreamblePipeline a =
    "v2"
        :> Header "Authorization" Text
        :> "organizations"
        :> Capture "org_slug" Text
        :> "pipelines"
        :> Capture "pipeline_slug" Text
        :> a

type GetBuilds =
    PreamblePipeline
        ( "builds"
            :> QueryParam "page" Int
            :> Get '[JSON] (BuildKiteHeaders [Build []])
        )

type GetBuildsOfBranch =
    PreamblePipeline
        ( "builds"
            :> QueryParam "branch" String
            :> QueryParam "page" Int
            :> Get '[JSON] (BuildKiteHeaders [Build []])
        )

type PreambleBuilds a =
    PreamblePipeline
        ( "builds"
            :> Capture "build_number" Int
            :> a
        )

type GetArtifacts =
    PreambleBuilds
        ( QueryParam "page" Int
            :> "artifacts"
            :> Get '[JSON] (BuildKiteHeaders [Artifact])
        )

type PreambleJobs a =
    PreambleBuilds
        ( "jobs"
            :> Capture "jobs" Text
            :> a
        )

type GetArtifact mime content =
    PreambleJobs
        ( "artifacts"
            :> Capture "artifact" Text
            :> "download"
            :> Get '[mime] content
        )

type WithLockingAuthPipeline a = WithLocking (Maybe Text -> Text -> Text -> a)
type WithLocking a = LimitsLock -> a
type WithAuthPipeline a =  Maybe Text -> Text -> Text -> a

respectLimits :: HasCallStack => LimitsLock -> BuildKiteHeaders o -> ClientM o
respectLimits l r = do
    let remaining = case lookupResponseHeader r
                            :: ResponseHeader "RateLimit-Remaining" Int of
            Header remaining' -> remaining'
            _ -> error "RateLimit-Remaining header not found"
    let reset = case lookupResponseHeader r
                        :: ResponseHeader "RateLimit-Reset" Int of
            Header reset' -> reset'
            _ -> error "RateLimit-Reset header not found"
    liftIO $ setLimit l remaining reset
    pure $ getResponse r

withLimitsLock :: HasCallStack => LimitsLock -> ClientM (BuildKiteHeaders b) -> ClientM b
withLimitsLock l f = do
    liftIO $ checkLimit l
    r <- f
    respectLimits l r

fetchBuilds
    :: WithLockingAuthPipeline
        (Maybe Int -> ClientM [Build []])
fetchBuilds l ma o r mc =
    withLimitsLock l $ client (Proxy @GetBuilds) ma o r mc

fetchBuildsOfBranch
    :: WithLockingAuthPipeline
        (Maybe String -> Maybe Int -> ClientM [Build []])
fetchBuildsOfBranch l ma o r mb mc =
    withLimitsLock l $ client (Proxy @GetBuildsOfBranch) ma o r mb mc

fetchArtifacts
    :: WithLockingAuthPipeline
        (Int -> Maybe Int -> ClientM [Artifact])
fetchArtifacts l ma o r bn mp = do
    withLimitsLock l $ client (Proxy @GetArtifacts) ma o r bn mp

type RateLimitRemaining = Header "RateLimit-Remaining" Int
type RateLimitReset = Header "RateLimit-Reset" Int
type BuildKiteHeaders = Headers '[RateLimitRemaining, RateLimitReset]
