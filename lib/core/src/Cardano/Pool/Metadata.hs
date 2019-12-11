{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- This module contains a function for downloading stake pool metadata from an
-- external registry.

module Cardano.Pool.Metadata
    ( -- * Types
      StakePoolMetadata (..)
    , StakePoolTicker

      -- * Fetching metadata
    , getStakePoolMetadata
    , getRegistryZipUrl
    , cardanoFoundationRegistryZip
    , FetchError (..)

      -- * Logging
    , RegistryLog (..)
    , RegistryLogMsg (..)
    )
    where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( DefinePrivacyAnnotation (..), DefineSeverity (..) )
import Cardano.BM.Trace
    ( Trace )
import Cardano.Wallet.Logging
    ( logTrace )
import Cardano.Wallet.Primitive.Types
    ( PoolOwner (..), ShowFmt (..) )
import Codec.Archive.Zip
import Control.Exception
    ( IOException, displayException, tryJust )
import Control.Monad
    ( join, (>=>) )
import Control.Monad.IO.Class
    ( MonadIO (..), liftIO )
import Control.Tracer
    ( contramap )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , camelTo2
    , eitherDecodeStrict
    , fieldLabelModifier
    , genericParseJSON
    , genericToJSON
    , omitNothingFields
    )
import Data.List
    ( find )
import Data.Maybe
    ( fromMaybe )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Fmt
    ( fmt, (+|), (|+) )
import GHC.Generics
    ( Generic )
import Network.HTTP.Client
    ( HttpException, parseUrlThrow )
import Network.HTTP.Simple
    ( httpSink )
import System.Environment
    ( lookupEnv )
import System.FilePath
    ( isPathSeparator, (<.>), (</>) )
import System.IO
    ( Handle, hClose, hTell )
import System.IO.Temp
    ( withSystemTempFile )

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

{-------------------------------------------------------------------------------
                                     Types
-------------------------------------------------------------------------------}

-- | Information about a stake pool, published by a stake pool owner in the
-- stake pool registry.
--
-- The wallet searches for registrations involving the owner, to find metadata
-- for a given PoolID.
--
-- The metadata information is not used directly by cardano-wallet, but rather
-- passed straight through to API consumers.
data StakePoolMetadata = StakePoolMetadata
    { owner :: PoolOwner
    -- ^ Bech32-encoded ed25519 public key.
    , ticker :: StakePoolTicker
    -- ^ Very short human-readable ID for the stake pool.
    , name :: Text
    -- ^ Name of the stake pool.
    , description :: Maybe Text
    -- ^ Short description of the stake pool.
    , homepage :: Text
    -- ^ Absolute URL for the stake pool's homepage link.
    , pledgeAddress :: Text
    -- ^ Bech32-encoded address.
    } deriving (Eq, Show, Generic)

-- | Very short name for a stake pool.
newtype StakePoolTicker = StakePoolTicker { unStakePoolTicker :: Text }
    deriving stock (Generic, Show, Eq)
    deriving newtype (ToText)

instance FromText StakePoolTicker where
    fromText t
        | T.length t >= 3 && T.length t <= 5
            = Right $ StakePoolTicker t
        | otherwise
            = Left . TextDecodingError $
                "stake pool ticker length must be 3-5 characters"

-- NOTE
-- JSON instances for 'StakePoolMetadata' and 'StakePoolTicker' matching the
-- format described by the registry. The server API may then use different
-- formats if needed.

instance FromJSON StakePoolMetadata where
    parseJSON = genericParseJSON defaultRecordTypeOptions

instance ToJSON StakePoolMetadata where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON StakePoolTicker where
    parseJSON = parseJSON >=> either (fail . show . ShowFmt) pure . fromText

instance ToJSON StakePoolTicker where
    toJSON = toJSON . toText

defaultRecordTypeOptions :: Aeson.Options
defaultRecordTypeOptions = Aeson.defaultOptions
    { fieldLabelModifier = camelTo2 '_' . dropWhile (== '_')
    , omitNothingFields = True
    }

{-------------------------------------------------------------------------------
                       Fetching metadata from a registry
-------------------------------------------------------------------------------}

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
-- * The JSON file names are 'PoolOwner's.
-- * The required JSON structure is given by the "Data.Aeson.FromJSON"
--   instance of 'StakePoolMetadata'.
--
-- The URL which should be used for incentived testnet is
-- 'cardanoFoundationRegistryZip'.
--
-- The returned list will have the same length as the given list of
-- 'PoolOwner'. If an metadata entry does not exist or could not be parsed, it
-- will be 'Nothing'.
getStakePoolMetadata
    :: Trace IO RegistryLog
    -- ^ Logging object - use 'transformTrace' to convert to 'Text'.
    -> String
    -- ^ URL of a zip archive to fetch and extract.
    -> [PoolOwner]
    -- ^ List of stake pools to get metadata for.
    -> IO (Either FetchError [Maybe StakePoolMetadata])
getStakePoolMetadata tr url poolOwners = fmap join $ tryJust fileExceptionHandler $
    withSystemTempFile "stake-pool-metadata.zip" $ \zipFileName zipFile -> do
        let tr' = contramap (fmap (RegistryLog url zipFileName)) tr
        logTrace tr' MsgDownloadStarted
        fetchStakePoolMetaZip url zipFile >>= \case
            Right size -> do
                logTrace tr' $ MsgDownloadComplete size
                hClose zipFile
                Right <$> getMetadataFromZip tr' zipFileName poolOwners
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
    -> [PoolOwner] -- ^ Stake pools to extract.
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
    trace = liftIO . logTrace tr

-- | A GitHub repo zip file archive contains the files beneath a top level
-- directory (which has a variable name). This function searches the archive
-- entries for the one which corresponds to a JSON file for the pool ID.
findArchiveFile :: FilePath -> ZipArchive (Maybe EntrySelector)
findArchiveFile repoPath = find isRepoFile . Map.keys <$> getEntries
  where
    isRepoFile entry = dropTopLevel (unEntrySelector entry) == repoPath
    dropTopLevel = drop 1 . dropWhile (not . isPathSeparator)

-- | The path within the registry repo of metadata for a stake pool.
registryFile :: PoolOwner -> FilePath
registryFile owner_ = "registry" </> T.unpack (toText owner_) <.> "json"

-- | Returns the Cardano Foundation stake pool registry zipfile URL, or the
-- @CARDANO_WALLET_STAKE_POOL_REGISTRY_URL@ environment variable if it is set.
getRegistryZipUrl :: IO String
getRegistryZipUrl = fromMaybe cardanoFoundationRegistryZip <$> lookupEnv var
  where
    var = "CARDANO_WALLET_STAKE_POOL_REGISTRY_URL"

-- | The stake pool registry zipfile download URL for CF.
cardanoFoundationRegistryZip :: String
cardanoFoundationRegistryZip =
    "https://github.com/" <> owner_ <> "/" <> repo <> "/archive/" <> archive
  where
    owner_ = "cardano-foundation"
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

instance DefinePrivacyAnnotation RegistryLog where
    definePrivacyAnnotation = definePrivacyAnnotation . registryLogMsg

instance DefineSeverity RegistryLog where
    defineSeverity = defineSeverity . registryLogMsg

-- | Log messages about processing a specific archive.
data RegistryLogMsg
    = MsgDownloadStarted
    | MsgDownloadComplete Integer
    | MsgDownloadError FetchError
    | MsgExtractFile FilePath
    | MsgExtractFileResult (Maybe (Either String StakePoolMetadata))
    deriving (Generic, Show, Eq, ToJSON)

instance DefinePrivacyAnnotation RegistryLogMsg
instance DefineSeverity RegistryLogMsg where
    defineSeverity ev = case ev of
        MsgDownloadStarted -> Info
        MsgDownloadComplete _ -> Info
        MsgDownloadError _ -> Error
        MsgExtractFile _ -> Debug
        MsgExtractFileResult (Just (Left _)) -> Warning
        MsgExtractFileResult _ -> Debug

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
