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

    -- * Construct URLs
    , identityUrlBuilder
    , registryUrlBuilder

    -- * re-exports
    , newManager
    , defaultManagerSettings
    ) where

import Prelude

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
import Control.Monad.Catch
    ( MonadThrow (..) )
import Control.Monad.IO.Class
    ( MonadIO )
import Control.Monad.Trans.Except
    ( ExceptT (..), except, runExceptT, throwE )
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
    ( status200 )
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
    :: forall m. (MonadThrow m)
    => StakePoolMetadataUrl
    -> StakePoolMetadataHash
    -> m URI
identityUrlBuilder (StakePoolMetadataUrl url) _ =
    maybe (throwM e) pure $ parseURI (T.unpack url)
  where
    e = InvalidUrlException (T.unpack url) "Invalid URL"

-- | Build a URL from a metadata hash compatible with an aggregation registry
registryUrlBuilder
    :: forall m. (MonadThrow m)
    => URI
    -> StakePoolMetadataUrl
    -> StakePoolMetadataHash
    -> m URI
registryUrlBuilder baseUrl _ (StakePoolMetadataHash bytes) =
    pure $ baseUrl
        { uriPath = "/" <> intercalate "/" (pathSegments baseUrl ++ [hash])
        }
  where
    hash = T.unpack $ T.decodeUtf8 $ convertToBase Base16 bytes

fetchFromRemote
    :: [StakePoolMetadataUrl -> StakePoolMetadataHash -> ExceptT String IO URI]
    -> Manager
    -> StakePoolMetadataUrl
    -> StakePoolMetadataHash
    -> IO (Either String StakePoolMetadata)
fetchFromRemote builders manager url hash = runExceptT $ do
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

    -- Try each builder in order, but only if the previous builder led to a
    -- "ConnectionFailure" exception. Other exceptions mean that we could
    -- likely reach the server.
    fromFirst (builder:rest) = do
        req <- requestFromURI =<< builder url hash
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
            if responseStatus res /= status200 then
                return $ Left "Server did reply, but not positively. This metadata may be blacklisted."
            else
                Right . Just . BL.toStrict <$> brReadSome (responseBody res) 512
        case mChunk of
            Nothing -> fromFirst rest
            Just chunk -> pure chunk
    fromFirst [] =
        throwE "Metadata server(s) didn't reply in a timely manner."
