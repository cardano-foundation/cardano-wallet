{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Buildkite.Client
    ( Query (..)
    , JobMap
    , BuildJobsMap
    , BuildAPI
    , getBuilds
    , getBuildsOfBranch
    , getArtifacts
    , getArtifactsContent
    , downloadArtifact
    )
where

import Prelude

import Buildkite.API
    ( Artifact (..)
    , ArtifactURL (..)
    , Job
    , WithAuthPipeline
    , fetchArtifacts
    , fetchBuilds
    , fetchBuildsOfBranch
    , jobId
    , number
    , overJobs
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Data.Map
    ( Map
    )
import Data.Text
    ( Text
    )
import Network.HTTP.Client
    ( Response (responseBody)
    , httpLbs
    , parseRequest
    )
import Network.HTTP.Client.TLS
    ( newTlsManager
    )
import Servant.Client
    ( ClientM
    )
import Streaming
    ( MonadTrans (lift)
    , Of
    , Stream
    )

import qualified Buildkite.API as BKAPI
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map
import qualified Streaming.Prelude as S

-- | An opaque containg a handle to the Buildkite API
data Query
    = Query
    { _query :: forall a. ClientM a -> IO (Maybe a)
    , _withAuth :: forall a. WithAuthPipeline a -> a
    }

type JobMap = Map Text Job

type BuildJobsMap = BKAPI.Build (Map Text)

type BuildAPI = BKAPI.Build []

paging
    :: Monad m
    => (Maybe Int -> m (Maybe [a]))
    -> Stream (Of a) m ()
paging f = go 1
  where
    go page = do
        mbs <- lift $ f $ Just page
        case mbs of
            Nothing ->
                pure () -- arbitrary choice ?
            Just bs -> do
                S.each bs
                case bs of
                    [] -> pure ()
                    _ -> go $ page + 1

getBuilds :: Query -> Stream (Of BuildAPI) IO ()
getBuilds (Query q w) = paging $ q . w fetchBuilds

getBuildsOfBranch :: Query -> String -> Stream (Of BuildAPI) IO ()
getBuildsOfBranch (Query q w) branch =
    paging $ q . w fetchBuildsOfBranch (Just branch)

getArtifacts :: Query -> BuildAPI -> Stream (Of (BuildJobsMap, Artifact)) IO ()
getArtifacts (Query q w) build =
    S.map (build',) $ paging $ q . w fetchArtifacts (number build)
  where
    build' = overJobs build $ \job' -> Map.fromList $ do
        jobV <- job'
        pure (jobId jobV, jobV)

getArtifactsContent
    :: Query
    -> WithAuthPipeline
        ( Int
          -> Text
          -> Text
          -> ClientM r
        )
    -> BuildJobsMap
    -> Artifact
    -> Stream (Of (BuildJobsMap, Artifact, r)) IO ()
getArtifactsContent (Query q w) getArtifact build artifact = do
    mBenchResults <- do
        lift
            $ q
            $ w
                getArtifact
                (number build)
                (job_id artifact)
                (BKAPI.artifactId artifact)
    case mBenchResults of
        Nothing -> pure ()
        Just benchResults -> S.yield (build, artifact, benchResults)

downloadArtifact :: ArtifactURL -> Stream (Of BL.ByteString) IO ()
downloadArtifact (ArtifactURL url') = do
    response <- liftIO $ do
        manager <- newTlsManager
        request <- parseRequest $ show url'
        httpLbs request manager
    S.yield $ responseBody response
