{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- This module contains a function for downloading stake pool metadata from an
-- external repository.

module Cardano.Pool.Registry
    ( getStakePoolMetadata
    , cardanoFoundationRegistryZip
    , FetchError(..)

    -- * Logging
    , transformTrace
    , RegistryLog (..)
    , RegistryLogMsg (..)
    )
    where

import Prelude

import Cardano.BM.Data.LogItem
    ( PrivacyAnnotation (..) )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Trace
    ( Trace, traceNamedItem )
import Cardano.Pool.Metrics
    ( StakePoolMetadata )
import Cardano.Wallet.Primitive.Types
    ( PoolId (..) )
import Codec.Archive.Zip
import Control.Exception
    ( IOException, displayException, tryJust )
import Control.Monad
    ( join )
import Control.Monad.IO.Class
    ( MonadIO (..), liftIO )
import Control.Tracer
    ( contramap )
import Data.Aeson
    ( ToJSON (..), eitherDecodeStrict )
import Data.List
    ( find )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Fmt
    ( fmt, (+|), (|+) )
import GHC.Generics
    ( Generic )
import Network.HTTP.Client
    ( HttpException, parseUrlThrow )
import Network.HTTP.Simple
    ( httpSink )
import System.FilePath
    ( isPathSeparator, (<.>), (</>) )
import System.IO
    ( Handle, hClose, hTell )
import System.IO.Temp
    ( withSystemTempFile )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Map.Strict as Map

-- | Associate a list of stake pool IDs with their
-- metadata (if present), which is downloaded from the given URL.
--
-- The URL must point to a zip archive, with the following stucture:
--
-- @
-- master.zip
-- └── testnet-stake-pool-registry-master/
--     └── registry/
--         ├── pk1afhcpw2tg7nr2m3wr4x8jaa4dv7d09gnv27kwfxpjyvukwxs8qdqwg85xp.json
--         └── pk1z4vh8gva25w07x8574uujuveu8gz43fu6qfln3t4prcavrvcphjsk0pdqs.json
-- @
--
-- * The name of the top-level directory can be anything.
-- * Other files in the archive are ignored by 'getStakePoolMetadata'.
-- * The JSON file names are 'PoolId's.
-- * The required JSON structure is given by the "Data.Aeson.FromJSON"
--   instance of 'StakePoolMetadata'.
--
-- The URL which should be used for incentived testnet is
-- 'cardanoFoundationRegistryZip'.
--
-- The returned list will have the same length as the given list of 'PoolId'. If
-- an metadata entry does not exist or could not be parsed, it will be
-- 'Nothing'.
getStakePoolMetadata
    :: Trace IO RegistryLog
    -- ^ Logging object - use 'transformTrace' to convert to 'Text'.
    -> String
    -- ^ URL of a zip archive to fetch and extract.
    -> [PoolId]
    -- ^ List of stake pools to get metadata for.
    -> IO (Either FetchError [Maybe StakePoolMetadata])
getStakePoolMetadata tr url poolIds = fmap join $ tryJust fileExceptionHandler $
    withSystemTempFile "stake-pool-metadata.zip" $ \zipFileName zipFile -> do
        let tr' = contramap (fmap (RegistryLog url zipFileName)) tr
        registryLog tr' MsgDownloadStarted
        fetchStakePoolMetaZip url zipFile >>= \case
            Right size -> do
                registryLog tr' $ MsgDownloadComplete size
                hClose zipFile
                Right <$> getMetadataFromZip tr' zipFileName poolIds
            Left e -> pure $ Left e
  where
    fileExceptionHandler = Just . FetchErrorFile . displayException @IOException
    
-- | Fetch a URL, streaming to the given file handle.
fetchStakePoolMetaZip :: String -> Handle -> IO (Either FetchError Integer)
fetchStakePoolMetaZip url zipFile = tryJust handler $ do
    req <- parseUrlThrow url
    httpSink req $ \_response -> Conduit.mapM_ (BS.hPut zipFile)
    hTell zipFile
  where
    handler = Just . FetchErrorDownload . displayException @HttpException

-- | With the given zip file, extract and parse metadata for the given stake
-- pools.
getMetadataFromZip
    :: Trace IO RegistryLogMsg
    -> FilePath -- ^ Zip file path.
    -> [PoolId] -- ^ Stake pools to extract.
    -> IO [Maybe StakePoolMetadata]
getMetadataFromZip tr zipFileName = 
    withArchive zipFileName . mapM (findStakePoolMeta tr . registryFile)
        
-- | Try to read and parse a metadata JSON file within a 'ZipArchive', if it is
-- present.
--
-- Any validation/parse error is unexpected because the data should already be
-- validated. We shall simply log this occurrence and return 'Nothing'.
-- 
-- This may throw "Codec.Archive.Zip.EntrySelectorException" if there is an
-- internal error and the pool id results in an invalid filename.
findStakePoolMeta
    :: Trace IO RegistryLogMsg -- ^ Logging.
    -> FilePath -- ^ Filename within zip file to work on.
    -> ZipArchive (Maybe StakePoolMetadata)
findStakePoolMeta tr entry = do
    trace $ MsgExtractFile entry
    findArchiveFile entry >>= \case
        Just sel -> do
            json <- getEntry sel
            case eitherDecodeStrict json of
                Right meta -> do
                    trace $ MsgExtractFileResult (Just (Right meta))
                    pure $ Just meta
                Left err -> do
                    trace $ MsgExtractFileResult (Just (Left err))
                    pure Nothing
        Nothing -> do
            trace $ MsgExtractFileResult Nothing
            pure Nothing
  where
    trace = liftIO . registryLog tr

-- | A GitHub repo zip file archive contains the files beneath a top level
-- directory (which has a variable name). This function searches the archive
-- entries for the one which corresponds to a JSON file for the pool ID.
findArchiveFile :: FilePath -> ZipArchive (Maybe EntrySelector)
findArchiveFile repoPath = find isRepoFile . Map.keys <$> getEntries
  where
    isRepoFile entry = dropTopLevel (unEntrySelector entry) == repoPath
    dropTopLevel = drop 1 . dropWhile (not . isPathSeparator)
    
-- | The path within the registry repo of metadata for a stake pool.
registryFile :: PoolId -> FilePath
registryFile (PoolId poolId) = "registry" </> B8.unpack poolId <.> "json"

-- | The stake pool registry zipfile download URL for CF.
cardanoFoundationRegistryZip :: String
cardanoFoundationRegistryZip =
    "https://github.com/" <> owner <> "/" <> repo <> "/archive/" <> archive
  where
    owner = "cardano-foundation"
    repo = "incentivized-testnet-stakepool-registry"
    archive = ref <> ".zip"
    ref = "master"

{-------------------------------------------------------------------------------
                                    Errors
-------------------------------------------------------------------------------}

-- | Things that could go wrong with fetching metadata, which aren't programming
-- errors or expected failures.
data FetchError
     = FetchErrorDownload String
     -- ^ Failed to download the registry archive.
     | FetchErrorFile String
     -- ^ Failed to store the registry archive in a temporary location.
     | FetchErrorZipParsingFailed FilePath String
     -- ^ Failed to open the registry archive with the given path, as a zip
     -- file, due to the given cause.
     deriving (Show, Eq, Generic, ToJSON)

instance ToText FetchError where
    toText = \case
        FetchErrorDownload err -> fmt $
            "Failed to download the registry archive: "+|err|+""
        FetchErrorFile err -> fmt $
            "Failed to store the registry archive in a temporary location."
            +|err|+""
        FetchErrorZipParsingFailed entry cause -> fmt $
            "Failed to open the registry zip file "+|entry|+": "+|cause|+""

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

-- | The log messages for 'getStakePoolMetadata'.
data RegistryLog = RegistryLog
    { registryLogUrl :: String
    , registryLogZipFile :: FilePath
    , registryLogMsg :: RegistryLogMsg
    } deriving (Generic, Show, Eq, ToJSON)

-- | Log messages about processing a specific archive.
data RegistryLogMsg
    = MsgDownloadStarted
    | MsgDownloadComplete Integer
    | MsgDownloadError FetchError
    | MsgExtractFile FilePath
    | MsgExtractFileResult (Maybe (Either String StakePoolMetadata))
    deriving (Generic, Show, Eq, ToJSON)

-- | Converts a text tracer into a stake pool registry tracer.
transformTrace :: Trace IO Text -> Trace IO RegistryLog
transformTrace = contramap (fmap toText)

registryLog :: MonadIO m => Trace m RegistryLogMsg -> RegistryLogMsg -> m ()
registryLog logTrace msg = traceNamedItem logTrace Public sev msg
  where
    sev = registryLogLevel msg

registryLogLevel :: RegistryLogMsg -> Severity
registryLogLevel MsgDownloadStarted = Info
registryLogLevel (MsgDownloadComplete _) = Info
registryLogLevel (MsgDownloadError _) = Error
registryLogLevel (MsgExtractFile _) = Debug
registryLogLevel (MsgExtractFileResult (Just (Left _))) = Warning
registryLogLevel (MsgExtractFileResult _) = Debug

instance ToText RegistryLog where
    toText (RegistryLog url zipFile msg) =
        fmt $ "Downloading "+|url|+" -> "+|zipFile|+": "+|toText msg|+""

instance ToText RegistryLogMsg where
    toText = \case
        MsgDownloadStarted ->
            "Started download"
        MsgDownloadComplete size ->
            fmt $ "Completed download of "+|size|+" bytes"
        MsgDownloadError e ->
            "Failed: " <> toText e
        MsgExtractFile f ->
            fmt $ "Extracting file: "+|f|+""
        MsgExtractFileResult Nothing ->
            "Metadata is not present in the registry"
        MsgExtractFileResult (Just (Left e)) ->
            fmt $ "Could not parse metadata: "+|e|+""
        MsgExtractFileResult (Just (Right _)) ->
            "Successfully parsed metadata"
