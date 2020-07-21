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
import Cardano.Wallet.Primitive.AddressDerivation
    ( hex )
import Cardano.Wallet.Primitive.Types
    ( StakePoolMetadata (..)
    , StakePoolMetadataHash (..)
    , StakePoolMetadataUrl (..)
    )
import Control.Exception
    ( IOException, handle )
import Control.Monad
    ( when )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Monad.Trans.Except
    ( ExceptT (..), except, runExceptT, throwE, withExceptT )
import Control.Tracer
    ( Tracer, traceWith )
import Crypto.Hash.Utils
    ( blake2b256 )
import Data.Aeson
    ( eitherDecodeStrict )
import Data.ByteArray.Encoding
    ( Base (..), convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.List
    ( intercalate )
import Data.Text.Class
    ( ToText (..) )
import Fmt
    ( pretty )
import Network.HTTP.Client
    ( HttpException (..)
    , Manager
    , ManagerSettings
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

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Client.TLS as HTTPS


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
    :: StakePoolMetadataUrl
    -> StakePoolMetadataHash
    -> Either HttpException URI
identityUrlBuilder (StakePoolMetadataUrl url) _ =
    maybe (Left e) Right $ parseURI (T.unpack url)
  where
    e = InvalidUrlException (T.unpack url) "Invalid URL"

-- | Build a URL from a metadata hash compatible with an aggregation registry
registryUrlBuilder
    :: URI
    -> StakePoolMetadataUrl
    -> StakePoolMetadataHash
    -> Either HttpException URI
registryUrlBuilder baseUrl _ (StakePoolMetadataHash bytes) =
    Right $ baseUrl
        { uriPath = "/" <> intercalate "/" (pathSegments baseUrl ++ [hash])
        }
  where
    hash = T.unpack $ T.decodeUtf8 $ convertToBase Base16 bytes

fetchFromRemote
    :: Tracer IO StakePoolMetadataFetchLog
    -> [StakePoolMetadataUrl -> StakePoolMetadataHash -> Either HttpException URI]
    -> Manager
    -> StakePoolMetadataUrl
    -> StakePoolMetadataHash
    -> IO (Maybe StakePoolMetadata)
fetchFromRemote tr builders manager url hash = runExceptTLog $ do
    chunk <- fromFirst builders
    when (blake2b256 chunk /= coerce hash) $ throwE $ mconcat
        [ "Metadata hash mismatch. Saw: "
        , B8.unpack $ hex $ blake2b256 chunk
        , ", but expected: "
        , B8.unpack $ hex $ coerce @_ @ByteString hash
        ]
    except $ eitherDecodeStrict chunk
  where
    fromIOException :: Monad m => IOException -> m (Either String a)
    fromIOException = return . Left . ("IO exception: " <>) . show

    fromHttpException :: Monad m => HttpException -> m (Either String (Maybe a))
    fromHttpException = const (return $ Right Nothing)

    runExceptTLog
        :: ExceptT String IO StakePoolMetadata
        -> IO (Maybe StakePoolMetadata)
    runExceptTLog action = runExceptT action >>= \case
        Left msg ->
            Nothing <$ traceWith tr (MsgFetchPoolMetadataFailure hash msg)

        Right meta ->
            Just meta <$ traceWith tr (MsgFetchPoolMetadataSuccess hash meta)

    -- Try each builder in order, but only if the previous builder led to a
    -- "ConnectionFailure" exception. Other exceptions mean that we could
    -- likely reach the server.
    fromFirst (builder:rest) = do
        uri <- withExceptT show $ except $ builder url hash
        req <- requestFromURI uri
        liftIO $ traceWith tr $ MsgFetchPoolMetadata hash uri
        mChunk <- ExceptT
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
                    pure $ Left $ "The server replied something unexpected: " <> show s
        case mChunk of
            Nothing -> do
                liftIO $ traceWith tr $ MsgFetchPoolMetadataFallback uri (null rest)
                fromFirst rest
            Just chunk -> pure chunk
    fromFirst [] =
        throwE "Metadata server(s) didn't reply in a timely manner."

data StakePoolMetadataFetchLog
    = MsgFetchPoolMetadata StakePoolMetadataHash URI
    | MsgFetchPoolMetadataSuccess StakePoolMetadataHash StakePoolMetadata
    | MsgFetchPoolMetadataFailure StakePoolMetadataHash String
    | MsgFetchPoolMetadataFallback URI Bool
    deriving (Show, Eq)

instance HasPrivacyAnnotation StakePoolMetadataFetchLog
instance HasSeverityAnnotation StakePoolMetadataFetchLog where
    getSeverityAnnotation = \case
        MsgFetchPoolMetadata{} -> Info
        MsgFetchPoolMetadataSuccess{} -> Info
        MsgFetchPoolMetadataFailure{} -> Warning
        MsgFetchPoolMetadataFallback{} -> Warning

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
