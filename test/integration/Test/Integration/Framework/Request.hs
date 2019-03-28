{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Integration.Framework.Request
    ( request
    , unsafeRequest
    , Headers(..)
    , Payload(..)
    , RequestException(..)
    , Context(..)
    ) where

import Prelude

import Control.Concurrent.Async
    ( Async )
import Control.Monad.Catch
    ( Exception (..), MonadCatch (..), MonadThrow, throwM )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Data.Aeson
    ( FromJSON )
import Data.ByteString.Lazy
    ( ByteString )
import Data.Maybe
    ( fromMaybe )
import Data.Text
    ( Text )
import Network.HTTP.Client
    ( Manager
    , RequestBody (..)
    , httpLbs
    , method
    , parseRequest
    , requestBody
    , requestHeaders
    , responseBody
    , responseStatus
    )
import Network.HTTP.Types.Header
    ( RequestHeaders )
import Network.HTTP.Types.Method
    ( Method )
import Network.HTTP.Types.Status
    ( status500 )

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as HTTP


-- | Running Context for our integration test
data Context = Context
    { _cluster
        :: Async ()
        -- ^ A handle to the running cluster / chain producer
    , _manager
        :: (Text, Manager)
        -- ^ The underlying BaseUrl and Manager used by the Wallet Client
    }

-- | The result when 'request' fails.
data RequestException
    = DecodeFailure ByteString
      -- ^ JSON decoding the given response data failed.
    | ClientError Aeson.Value
      -- ^ The HTTP response status code indicated failure.
    deriving (Show)

instance Exception RequestException

-- | The payload of the request
data Payload
    = Json Aeson.Value
    | NonJson ByteString
    | Empty

-- | The headers of the request
data Headers
    = Headers RequestHeaders
    | Default
    | None

-- | Makes a request to the API and decodes the response.
request
    :: forall a m.
        ( FromJSON a
        , MonadIO m
        , MonadThrow m
        , MonadCatch m
        )
    => Context
    -> (Method, Text)
        -- ^ HTTP method and request path
    -> Headers
        -- ^ Request headers
    -> Payload
        -- ^ Request body
    -> m (HTTP.Status, Either RequestException a)
request (Context _ (base, manager)) (verb, path) reqHeaders body = do
    req <- parseRequest $ T.unpack $ base <> path
    handleResponse <$> liftIO (httpLbs (prepareReq req) manager)
  where
    prepareReq :: HTTP.Request -> HTTP.Request
    prepareReq req = req
        { method = verb
        , requestBody = payload
        , requestHeaders = headers
        }
        where
            headers = case h of
                Headers x -> x
                Default -> [ ("Content-Type", "application/json")
                           , ("Accept", "application/json")
                           ]
                None -> mempty

            payload = case body of
                Json x -> (RequestBodyLBS . Aeson.encode) x
                NonJson x -> RequestBodyLBS x
                Empty -> mempty

    handleResponse res = case responseStatus res of
        s | s < status500 -> maybe
            (s, Left $ decodeFailure res)
            ((s,) . Right)
            (Aeson.decode $ responseBody res)
        -- TODO: decode API error responses into ClientError
        s -> (s, Left $ decodeFailure res)

    decodeFailure :: HTTP.Response ByteString -> RequestException
    decodeFailure res = DecodeFailure $ responseBody res

-- | Makes a request to the API, but throws if it fails.
unsafeRequest
    :: forall a m.
        ( FromJSON a
        , MonadIO m
        , MonadThrow m
        , MonadCatch m
        )
    => Context
    -> (Method, Text)
    -> Maybe Aeson.Value
    -> m (HTTP.Status, a)
unsafeRequest ctx req body = do
    (s, res) <- request ctx req Nothing body
    either throwM (pure . (s,)) res
