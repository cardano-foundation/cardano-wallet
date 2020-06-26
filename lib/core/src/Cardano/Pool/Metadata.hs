{-# LANGUAGE NumericUnderscores #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- HTTP-client(s) for fetching stake pool metadata from remote servers (directly
-- from pool operators, or from smash).

module Cardano.Pool.Metadata
    ( fetchFromRemote
    -- TODO: fetchFromAggregationServer

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
import Control.Arrow
    ( left )
import Control.Exception
    ( IOException, SomeException, handle )
import Control.Monad
    ( when )
import Control.Monad.IO.Class
    ( MonadIO )
import Control.Monad.Trans.Except
    ( ExceptT (..), except, runExceptT, throwE )
import Crypto.Hash.Utils
    ( blake2b256 )
import Data.Aeson
    ( eitherDecodeStrict )
import Network.HTTP.Client
    ( HttpException
    , Manager
    , ManagerSettings
    , brReadSome
    , managerResponseTimeout
    , parseUrlThrow
    , responseBody
    , responseTimeoutMicro
    , withResponse
    )

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
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

fetchFromRemote
    :: Manager
    -> StakePoolMetadataUrl
    -> StakePoolMetadataHash
    -> IO (Either String StakePoolMetadata)
fetchFromRemote manager url_ hash_ = runExceptT $ do
    req <- fromInvalidUrl $ parseUrlThrow (T.unpack url)
    chunk <- ExceptT
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
        pure . BL.toStrict <$> brReadSome (responseBody res) 512
    when (blake2b256 chunk /= hash) $ throwE $ mconcat
        [ "Metadata hash mismatch. Saw: "
        , B8.unpack $ hex $ blake2b256 chunk
        , ", but expected: "
        , B8.unpack $ hex hash
        ]
    except $ eitherDecodeStrict chunk
  where
    StakePoolMetadataUrl  url  = url_
    StakePoolMetadataHash hash = hash_

    fromIOException :: Monad m => IOException -> m (Either String a)
    fromIOException = return . Left . ("IO exception: " <>) . show

    fromHttpException :: Monad m => HttpException -> m (Either String a)
    fromHttpException = return . Left . ("HTTP exception: " <>) . show

    fromInvalidUrl :: Monad m => Either SomeException a -> ExceptT String m a
    fromInvalidUrl = except . left (("Invalid metadata url: " <>) . show)
