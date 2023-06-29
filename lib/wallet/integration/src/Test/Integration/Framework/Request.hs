{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Integration.Framework.Request
    ( request
    , rawRequest
    , unsafeRequest
    , Headers (..)
    , Payload (..)
    , RequestException (..)
    ) where

import Prelude

import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Monad.IO.Unlift
    ( MonadUnliftIO (..)
    )
import Data.Aeson
    ( FromJSON
    , Value
    , eitherDecode
    , encode
    )
import Data.Bifunctor
    ( first
    )
import Data.ByteString.Lazy
    ( ByteString
    )
import Data.Generics.Internal.VL.Lens
    ( (^.)
    )
import Data.Generics.Product.Typed
    ( HasType
    , typed
    )
import Data.Text
    ( Text
    )
import Network.HTTP.Client
    ( HttpException (..)
    , HttpExceptionContent
    , Manager
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
    ( RequestHeaders
    )
import Network.HTTP.Types.Method
    ( Method
    )
import Network.HTTP.Types.Status
    ( status400
    , status500
    , statusIsSuccessful
    )
import Network.URI
    ( URI
    )
import Test.Integration.Framework.Context
    ( Context
    )
import UnliftIO.Exception
    ( Exception (..)
    , fromEither
    , handle
    , throwIO
    )

import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as HTTP

-- | The result when 'request' fails.
data RequestException
    = -- | JSON decoding the given response data failed.
      DecodeFailure ByteString String
    | -- | The HTTP response status code indicated failure.
      ClientError Value
    | -- | The HTTP response status code indicated failure and the response was
      -- not valid JSON.
      RawClientError ByteString
    | -- | A wild exception upon sending the request
      HttpException HttpExceptionContent
    deriving (Show)

instance Exception RequestException

-- | The payload of the request
data Payload
    = Json Value
    | NonJson ByteString
    | Empty
    deriving (Show)

-- | The headers of the request
data Headers
    = Headers RequestHeaders
    | Default
    | None
    deriving (Show)

-- | Makes a request to the API and decodes the response.
request
    :: forall a m s
     . ( FromJSON a
       , MonadUnliftIO m
       , HasType (URI, Manager) s
       )
    => s
    -> (Method, Text)
    -- ^ HTTP method and request path
    -> Headers
    -- ^ Request headers
    -> Payload
    -- ^ Request body
    -> m (HTTP.Status, Either RequestException a)
request = baseRequest defaultHeaders handleResponse
  where
    handleResponse res = (status, decode body)
      where
        status = responseStatus res
        body = responseBody res

        decode
            | statusIsSuccessful status = decodeBody
            | otherwise = decodeErrorBody

        decodeBody = first errDecode . eitherDecode

        -- decode API error responses into ClientError
        decodeErrorBody = Left . either errDecode ClientError . eitherDecode

        errDecode = DecodeFailure body

    defaultHeaders =
        [ ("Content-Type", "application/json")
        , ("Accept", "application/json")
        ]

-- | Like 'request', but does not attempt to deserialize the response.
rawRequest
    :: forall m s
     . ( MonadUnliftIO m
       , HasType (URI, Manager) s
       )
    => s
    -> (Method, Text)
    -- ^ HTTP method and request path
    -> Headers
    -- ^ Request headers
    -> Payload
    -- ^ Request body
    -> m (HTTP.Status, Either RequestException ByteString)
rawRequest = baseRequest mempty handleResponse
  where
    handleResponse res = case responseStatus res of
        s | s >= status400 -> (s, Left $ RawClientError $ responseBody res)
        s -> (s, Right $ responseBody res)

baseRequest
    :: forall a m s
     . ( MonadUnliftIO m
       , HasType (URI, Manager) s
       )
    => RequestHeaders
    -> (HTTP.Response ByteString -> (HTTP.Status, Either RequestException a))
    -> s
    -> (Method, Text)
    -- ^ HTTP method and request path
    -> Headers
    -- ^ Request headers
    -> Payload
    -- ^ Request body
    -> m (HTTP.Status, Either RequestException a)
baseRequest defaultHeaders handleResponse ctx (verb, path) headers payload = do
    let (base, manager) = ctx ^. typed @(URI, Manager)
    handle handleException $ do
        req <- fromEither $ parseRequest $ show base <> T.unpack path
        handleResponse <$> liftIO (httpLbs (prepareReq req) manager)
  where
    prepareReq :: HTTP.Request -> HTTP.Request
    prepareReq req = req{method = verb, requestBody, requestHeaders}

    requestHeaders = case headers of
        Headers x -> x
        Default -> defaultHeaders
        None -> mempty

    requestBody = case payload of
        Json x -> RequestBodyLBS (encode x)
        NonJson x -> RequestBodyLBS x
        Empty -> mempty

    handleException
        :: HttpException -> m (HTTP.Status, Either RequestException a)
    handleException = \case
        e@InvalidUrlException{} -> throwIO e
        HttpExceptionRequest _ e -> pure (status500, Left (HttpException e))

-- | Makes a request to the API, but throws if it fails.
unsafeRequest
    :: forall a m
     . ( FromJSON a
       , MonadUnliftIO m
       )
    => Context
    -> (Method, Text)
    -> Payload
    -> m (HTTP.Status, a)
unsafeRequest ctx req body = do
    (s, res) <- request ctx req Default body
    either throwIO (pure . (s,)) res
