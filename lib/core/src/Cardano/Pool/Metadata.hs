{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- HTTP-client(s) for fetching stake pool metadata from remote servers (directly
-- from pool operators, or from smash).

module Cardano.Pool.Metadata
    (

    -- * Fetch
      fetchFromRemote
    , StakePoolMetadataFetchLog (..)
    , fetchDelistedPools
    , healthCheck

    -- * Construct URLs
    , identityUrlBuilder
    , registryUrlBuilder

    -- * re-exports
    , newManager
    , defaultManagerSettings
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Wallet.Api.Types
    ( defaultRecordTypeOptions )
import Cardano.Wallet.Primitive.AddressDerivation
    ( hex )
import Cardano.Wallet.Primitive.Types
    ( PoolId (..)
    , StakePoolMetadata (..)
    , StakePoolMetadataHash (..)
    , StakePoolMetadataUrl (..)
    )
import Control.Exception
    ( IOException, handle )
import Control.Monad
    ( forM, when )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Monad.Trans.Except
    ( ExceptT (..), except, runExceptT, throwE, withExceptT )
import Control.Tracer
    ( Tracer, traceWith )
import Crypto.Hash.Utils
    ( blake2b256 )
import Data.Aeson
    ( FromJSON
    , ToJSON
    , eitherDecodeStrict
    , genericParseJSON
    , genericToJSON
    , object
    , parseJSON
    , toJSON
    , withObject
    , (.:)
    , (.=)
    )
import Data.Bifunctor
    ( Bifunctor (first) )
import Data.ByteArray.Encoding
    ( Base (..), convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.List
    ( intercalate )
import Data.Text.Class
    ( TextDecodingError (..), ToText (..), fromText )
import Fmt
    ( pretty )
import GHC.Generics
    ( Generic )
import Network.HTTP.Client
    ( HttpException (..)
    , Manager
    , ManagerSettings
    , brConsume
    , brReadSome
    , managerResponseTimeout
    , requestFromURI
    , responseBody
    , responseStatus
    , responseTimeoutMicro
    , withResponse
    )
import Network.HTTP.Types.Status
    ( status200, status404 )
import Network.URI
    ( URI (..), parseURI, pathSegments )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Client.TLS as HTTPS


-- | TODO: import SMASH types
newtype SMASHPoolId = SMASHPoolId T.Text
  deriving stock (Eq, Show, Ord)

instance ToJSON SMASHPoolId where
    toJSON (SMASHPoolId poolId) =
        object
            [ "poolId" .= poolId
            ]

instance FromJSON SMASHPoolId where
    parseJSON = withObject "SMASHPoolId" $ \o -> do
        poolId <- o .: "poolId"
        return $ SMASHPoolId poolId

toPoolId :: SMASHPoolId -> Either TextDecodingError PoolId
toPoolId (SMASHPoolId pid) = fromText pid

-- | Some default settings, overriding some of the library's default with
-- stricter values.
defaultManagerSettings :: ManagerSettings
defaultManagerSettings =
    HTTPS.tlsManagerSettings
        { managerResponseTimeout = responseTimeoutMicro tenSeconds }
  where
    tenSeconds = 10_000_000 -- in μs

-- | Create a connection manager that supports TLS connections.
newManager :: MonadIO m => ManagerSettings -> m Manager
newManager = HTTPS.newTlsManagerWith

-- | Simply return a pool metadata url, unchanged
identityUrlBuilder
    :: PoolId
    -> StakePoolMetadataUrl
    -> StakePoolMetadataHash
    -> Either HttpException URI
identityUrlBuilder _ (StakePoolMetadataUrl url) _ =
    maybe (Left e) Right $ parseURI (T.unpack url)
  where
    e = InvalidUrlException (T.unpack url) "Invalid URL"

-- | Build a URL from a metadata hash compatible with an aggregation registry
registryUrlBuilder
    :: URI
    -> PoolId
    -> StakePoolMetadataUrl
    -> StakePoolMetadataHash
    -> Either HttpException URI
registryUrlBuilder baseUrl pid _ (StakePoolMetadataHash bytes) =
    Right $ baseUrl
        { uriPath = "/" <> intercalate "/"
            (pathSegments baseUrl ++ [pidStr,hashStr])
        }
  where
    hashStr = T.unpack $ T.decodeUtf8 $ convertToBase Base16 bytes
    pidStr  = T.unpack $ toText pid

-- | A smash GET request that reads the result at once into memory.
smashRequest
    :: Tracer IO StakePoolMetadataFetchLog
    -> URI
    -> Manager
    -> ExceptT String IO ByteString
smashRequest tr uri manager = getPayload
  where
    getPayload :: ExceptT String IO ByteString
    getPayload = do
        req <- withExceptT show $ except $ requestFromURI uri
        liftIO $ traceWith tr $ MsgFetchSMASH uri
        ExceptT
            $ handle fromIOException
            $ handle fromHttpException
            $ withResponse req manager handleResponseStatus

    handleResponseStatus response = case responseStatus response of
        s | s == status200 -> do
            let body = responseBody response
            Right . BS.concat <$> brConsume body
        s ->
            pure $ Left $ mconcat
                [ "The server replied with something unexpected: "
                , show s
                ]

    fromHttpException :: Monad m => HttpException -> m (Either String a)
    fromHttpException = return . Left . ("HTTP exception: " <>) . show

data HealthCheck = HealthCheck
    { status :: T.Text
    , version :: T.Text
    } deriving (Generic, Show, Eq, Ord)

instance FromJSON HealthCheck where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON HealthCheck where
    toJSON = genericToJSON defaultRecordTypeOptions

healthCheck
    :: Tracer IO StakePoolMetadataFetchLog
    -> URI
    -> Manager
    -> IO (Maybe HealthCheck)
healthCheck tr uri manager = runExceptTLog $ do
    pl <- smashRequest tr uri manager
    except . eitherDecodeStrict @HealthCheck $ pl
  where
    runExceptTLog
        :: ExceptT String IO HealthCheck
        -> IO (Maybe HealthCheck)
    runExceptTLog action = runExceptT action >>= \case
        Left msg ->
            Nothing <$ traceWith tr (MsgFetchHealthCheckFailure msg)

        Right meta ->
            Just meta <$ traceWith tr (MsgFetchHealthCheckSuccess meta)

fetchDelistedPools
    :: Tracer IO StakePoolMetadataFetchLog
    -> URI
    -> Manager
    -> IO (Maybe [PoolId])
fetchDelistedPools tr uri manager = runExceptTLog $ do
    pl <- smashRequest tr uri manager
    smashPids <- except $ eitherDecodeStrict @[SMASHPoolId] pl
    forM smashPids $ except . first getTextDecodingError . toPoolId
  where
    runExceptTLog
        :: ExceptT String IO [PoolId]
        -> IO (Maybe [PoolId])
    runExceptTLog action = runExceptT action >>= \case
        Left msg ->
            Nothing <$ traceWith tr (MsgFetchDelistedPoolsFailure msg)

        Right meta ->
            Just meta <$ traceWith tr (MsgFetchDelistedPoolsSuccess meta)

-- TODO: refactor/simplify this
fetchFromRemote
    :: Tracer IO StakePoolMetadataFetchLog
    -> [   PoolId
        -> StakePoolMetadataUrl
        -> StakePoolMetadataHash
        -> Either HttpException URI
       ]
    -> Manager
    -> PoolId
    -> StakePoolMetadataUrl
    -> StakePoolMetadataHash
    -> IO (Maybe StakePoolMetadata)
fetchFromRemote tr builders manager pid url hash = runExceptTLog $ do
    chunk <- getChunk `fromFirst` builders
    when (blake2b256 chunk /= coerce hash) $ throwE $ mconcat
        [ "Metadata hash mismatch. Saw: "
        , B8.unpack $ hex $ blake2b256 chunk
        , ", but expected: "
        , B8.unpack $ hex $ coerce @_ @ByteString hash
        ]
    except $ eitherDecodeStrict chunk
  where
    runExceptTLog
        :: ExceptT String IO StakePoolMetadata
        -> IO (Maybe StakePoolMetadata)
    runExceptTLog action = runExceptT action >>= \case
        Left msg ->
            Nothing <$ traceWith tr (MsgFetchPoolMetadataFailure hash msg)

        Right meta ->
            Just meta <$ traceWith tr (MsgFetchPoolMetadataSuccess hash meta)

    -- Try each builder in order, but only if the previous builder led to an
    -- IO exception. Other exceptions like HTTP exceptions are treated as
    -- 'normal' responses from the an aggregation server and do not cause a
    -- retry.
    fromFirst _ [] =
        throwE "Metadata server(s) didn't reply in a timely manner."
    fromFirst action (builder:rest) = do
        uri <- withExceptT show $ except $ builder pid url hash
        action uri >>= \case
            Nothing -> do
                liftIO $ traceWith tr $ MsgFetchPoolMetadataFallback uri (null rest)
                fromFirst action rest
            Just chunk ->
                pure chunk

    getChunk :: URI -> ExceptT String IO (Maybe ByteString)
    getChunk uri = do
        req <- withExceptT show $ except $ requestFromURI uri
        liftIO $ traceWith tr $ MsgFetchPoolMetadata hash uri
        ExceptT
            $ handle fromIOException
            $ handle fromHttpException
            $ withResponse req manager $ \res -> do
            -- NOTE
            -- Metadata are _supposed to_ be made of:
            --
            -- - A name (at most 50 UTF-8 bytes)
            -- - An optional description (at most 250 UTF-8 bytes)
            -- - A ticker (at most 5 UTF-8 bytes)
            -- - A homepage (at most 100 UTF-8 bytes)
            --
            -- So, the total, including a pretty JSON encoding with newlines ought
            -- to be less than 512 bytes. For security reasons, we only download the
            -- first 512 bytes.
            case responseStatus res of
                s | s == status200 -> do
                    let body = responseBody res
                    Right . Just . BL.toStrict <$> brReadSome body 512

                s | s == status404 -> do
                    pure $ Left "There's no known metadata for this pool."

                s -> do
                    pure $ Left $ mconcat
                        [ "The server replied with something unexpected: "
                        , show s
                        ]

    fromHttpException :: Monad m => HttpException -> m (Either String (Maybe a))
    fromHttpException = const (return $ Right Nothing)

fromIOException :: Monad m => IOException -> m (Either String a)
fromIOException = return . Left . ("IO exception: " <>) . show
data StakePoolMetadataFetchLog
    = MsgFetchPoolMetadata StakePoolMetadataHash URI
    | MsgFetchPoolMetadataSuccess StakePoolMetadataHash StakePoolMetadata
    | MsgFetchPoolMetadataFailure StakePoolMetadataHash String
    | MsgFetchPoolMetadataFallback URI Bool
    | MsgFetchSMASH URI
    | MsgFetchDelistedPoolsFailure String
    | MsgFetchDelistedPoolsSuccess [PoolId]
    | MsgFetchHealthCheckFailure String
    | MsgFetchHealthCheckSuccess HealthCheck
    deriving (Show, Eq)

instance HasPrivacyAnnotation StakePoolMetadataFetchLog
instance HasSeverityAnnotation StakePoolMetadataFetchLog where
    getSeverityAnnotation = \case
        MsgFetchPoolMetadata{} -> Info
        MsgFetchPoolMetadataSuccess{} -> Info
        MsgFetchPoolMetadataFailure{} -> Warning
        MsgFetchPoolMetadataFallback{} -> Warning
        MsgFetchSMASH{} -> Debug
        MsgFetchDelistedPoolsFailure{} -> Warning
        MsgFetchDelistedPoolsSuccess{} -> Info
        MsgFetchHealthCheckFailure{} -> Warning
        MsgFetchHealthCheckSuccess{} -> Info

instance ToText StakePoolMetadataFetchLog where
    toText = \case
        MsgFetchPoolMetadata hash uri -> mconcat
            [ "Fetching metadata with hash ", pretty hash
            , " from ", T.pack (show uri)
            ]
        MsgFetchPoolMetadataSuccess hash meta -> mconcat
            [ "Successfully fetched metadata with hash ", pretty hash
            , ": ", T.pack (show meta)
            ]
        MsgFetchPoolMetadataFailure hash msg -> mconcat
            [ "Failed to fetch metadata with hash ", pretty hash, ": ", T.pack msg
            ]
        MsgFetchPoolMetadataFallback uri noMoreUrls -> mconcat
            [ "Couldn't reach server at ", T.pack (show uri), "."
            , if noMoreUrls
                then ""
                else " Falling back using a different strategy."
            ]
        MsgFetchSMASH uri -> mconcat
            [ "Making a SMASH request to ", T.pack (show uri)
            ]
        MsgFetchDelistedPoolsSuccess poolIds -> mconcat
            [ "Successfully fetched delisted "
            , T.pack (show . length $ poolIds)
            , " pools."
            ]
        MsgFetchDelistedPoolsFailure err -> mconcat
            [ "Failed to fetch delisted pools: ", T.pack err
            ]
        MsgFetchHealthCheckSuccess health -> mconcat
            [ "Successfully checked health "
            , T.pack (show health)
            ]
        MsgFetchHealthCheckFailure err -> mconcat
            [ "Failed to check health: ", T.pack err
            ]

