{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
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
    , jobNameContains
    , deleteJobLog
    , HandleClientError (..)
    , bailout
    , skip410
    , skip400
    , handleClientError
    , Skipping (..)
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
    , NoContent
    , QueryParam
    , ResponseHeader (..)
    , StdMethod (..)
    , Verb
    , lookupResponseHeader
    , (:>)
    )
import Servant.Client
    ( ClientError (..)
    , ClientM
    , ResponseF (..)
    , client
    )
import Text.Regex.TDFA
    ( (=~)
    )

import qualified Data.Text as T
import Network.HTTP.Types
    ( Status
    , status400
    , status410
    )

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

-- Function to check if a job name matches a regex
jobNameContains :: Text -> Job -> Bool
jobNameContains regex (Job _ (Just job) _ _ _) =
    job =~ T.unpack regex
jobNameContains _ _ = False

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

type DeleteJobLog =
    PreambleJobs
        ("log" :> Verb 'DELETE 204 '[] (BuildKiteHeaders NoContent))

type WithLockingAuthPipeline a =
    WithLocking (Maybe Text -> Text -> Text -> a)
type WithLocking a = LimitsLock -> a
type WithAuthPipeline a = Maybe Text -> Text -> Text -> a

respectLimits
    :: HasCallStack => LimitsLock -> BuildKiteHeaders o -> ClientM o
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

withLimitsLock
    :: HasCallStack
    => LimitsLock
    -> ClientM (BuildKiteHeaders b)
    -> ClientM b
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

deleteJobLog
    :: WithLockingAuthPipeline (Int -> Text -> ClientM NoContent)
deleteJobLog l ma o r b j =
    withLimitsLock l
        $ client (Proxy @DeleteJobLog) ma o r b j

fetchArtifacts
    :: WithLockingAuthPipeline
        (Int -> Maybe Int -> ClientM [Artifact])
fetchArtifacts l ma o r bn mp = do
    withLimitsLock l $ client (Proxy @GetArtifacts) ma o r bn mp

type RateLimitRemaining = Header "RateLimit-Remaining" Int
type RateLimitReset = Header "RateLimit-Reset" Int
type BuildKiteHeaders = Headers '[RateLimitRemaining, RateLimitReset]

newtype HandleClientError
    = HandleClientError
        (forall a. IO (Either ClientError a) -> IO (Maybe a))

data SkipOrAbort = Skip | Abort

newtype Skipping = Skipping (ClientError -> SkipOrAbort)
instance Semigroup Skipping where
    Skipping f <> Skipping g = Skipping $ \e -> case f e of
        Skip -> Skip
        Abort -> g e

bailout :: Skipping
bailout = Skipping $ const Abort

handleClientError :: Skipping -> HandleClientError
handleClientError (Skipping g) = HandleClientError $ \f -> do
    res <- f
    case res of
        Left e -> case g e of
            Abort -> error $ show e
            Skip -> pure Nothing
        Right a -> pure $ Just a

skippingStatus :: Status -> Skipping
skippingStatus s = Skipping $ \case
    FailureResponse _ (Response s' _ _ _)
        | s == s' -> Skip
    _ -> Abort

skip410 :: Skipping
skip410 = skippingStatus status410

skip400 :: Skipping
skip400 = skippingStatus status400
