{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Buildkite.Artifacts
    ( ArtifactRow (..)
    , mkArtifactRow
    , artifactsHeader
    )
where

import Prelude

import Buildkite.API
    ( Artifact (..)
    , Build (..)
    , Time (..)
    , artifactId
    , created_at
    , file_size
    , job_id
    , parseDate
    , path
    , renderDate
    , time
    )
import Buildkite.Client
    ( BuildJobsMap
    )
import Data.Csv
    ( Field
    , FromField (..)
    , FromNamedRecord (..)
    , Header
    , Parser
    , ToField (..)
    , ToNamedRecord (..)
    , header
    , namedRecord
    , (.:)
    , (.=)
    )
import Data.Text
    ( Text
    )

import qualified Data.Map as M

-- | A row in the artifact CSV.
data ArtifactRow = ArtifactRow
    { row_artifact_id :: Text
    , row_file_size :: Int
    , row_path :: Text
    , row_created_at :: Time
    , row_build :: Int
    , row_job_id :: Text
    }
    deriving (Show)

instance ToNamedRecord ArtifactRow where
    toNamedRecord
        ArtifactRow
            { row_artifact_id
            , row_file_size
            , row_path
            , row_created_at
            , row_build
            , row_job_id
            } =
            namedRecord
                [ "artifact_id" .= row_artifact_id
                , "file_size" .= row_file_size
                , "path" .= row_path
                , "created_at" .= row_created_at
                , "build" .= row_build
                , "job_id" .= row_job_id
                ]

instance ToField Time where
    toField :: Time -> Field
    toField = toField . renderDate . time

instance FromField Time where
    parseField :: Field -> Parser Time
    parseField f = do
        f' <- parseField f
        Time <$> parseDate f'

instance FromNamedRecord ArtifactRow where
    parseNamedRecord m =
        ArtifactRow
            <$> m .: "artifact_id"
            <*> m .: "file_size"
            <*> m .: "path"
            <*> m .: "created_at"
            <*> m .: "build"
            <*> m .: "job_id"

artifactsHeader :: Header
artifactsHeader =
    header
        [ "artifact_id"
        , "file_size"
        , "path"
        , "created_at"
        , "build"
        , "job_id"
        ]

-- | Create an 'ArtifactRow' from a 'BuildJobsMap' and an 'Artifact'.
mkArtifactRow :: BuildJobsMap -> Artifact -> Maybe ArtifactRow
mkArtifactRow (Build number _ jobs) a = do
    job <- M.lookup (job_id a) jobs
    row_created_at <- created_at job
    pure
        $ ArtifactRow
            { row_artifact_id = artifactId a
            , row_file_size = file_size a
            , row_path = path a
            , row_created_at
            , row_build = number
            , row_job_id = job_id a
            }
