{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Integration.Framework.Request
    ( request
    , rawRequest
    , unsafeRequest
    , Headers(..)
    , Payload(..)
    , RequestException(..)
    ) where

import Prelude

import Control.Monad.Catch
    ( Exception (..), MonadCatch (..), throwM )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Data.Aeson
    ( FromJSON )
import Data.ByteString.Lazy
    ( ByteString )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Product.Typed
    ( HasType, typed )
import Data.Text
    ( Text )
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
    ( RequestHeaders )
import Network.HTTP.Types.Method
    ( Method )
import Network.HTTP.Types.Status
    ( status400, status500 )
import Test.Integration.Framework.Context
    ( Context )

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as HTTP

-- | The result when 'request' fails.
data RequestException
    = DecodeFailure ByteString
      -- ^ JSON decoding the given response data failed.
    | ClientError Aeson.Value
      -- ^ The HTTP response status code indicated failure.
    | HttpException HttpExceptionContent
      -- ^ A wild exception upon sending the request
    deriving (Show)

instance Exception RequestException

-- | The payload of the request
data Payload
    = Json Aeson.Value
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
    :: forall a m s.
        ( FromJSON a
        , MonadIO m
        , MonadCatch m
        , HasType (Text, Manager) s
        )
    => s
    -> (Method, Text)
        -- ^ HTTP method and request path
    -> Headers
        -- ^ Request headers
    -> Payload
        -- ^ Request body
    -> m (HTTP.Status, Either RequestException a)
request ctx (verb, path) reqHeaders body = do
    let (base, manager) = ctx ^. typed @(Text, Manager)
    req <- parseRequest $ T.unpack $ base <> path
    let io = handleResponse <$> liftIO (httpLbs (prepareReq req) manager)
    catch io handleException
  where
    prepareReq :: HTTP.Request -> HTTP.Request
    prepareReq req = req
        { method = verb
        , requestBody = payload
        , requestHeaders = headers
        }
        where
            headers = case reqHeaders of
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
        s | s < status500 -> either
            (\err -> (s, Left $ DecodeFailure $ BL8.pack $ err <> ": " <> show res))
            ((s,) . Right)
            (Aeson.eitherDecode $ responseBody res)

        -- TODO: decode API error responses into ClientError
        s -> (s, Left $ decodeFailure res)

    handleException = \case
        e@InvalidUrlException{} ->
            throwM e
        HttpExceptionRequest _ e ->
            return (status500, Left (HttpException e))

    decodeFailure :: HTTP.Response ByteString -> RequestException
    decodeFailure res = DecodeFailure $ responseBody res

-- | Like 'request', but does not attempt to deserialize the response.
rawRequest
    :: forall m s.
        ( MonadIO m
        , MonadCatch m
        , HasType (Text, Manager) s
        )
    => s
    -> (Method, Text)
        -- ^ HTTP method and request path
    -> Headers
        -- ^ Request headers
    -> Payload
        -- ^ Request body
    -> m (HTTP.Status, Either RequestException ByteString)
rawRequest ctx (verb, path) reqHeaders body = do
    let (base, manager) = ctx ^. typed @(Text, Manager)
    req <- parseRequest $ T.unpack $ base <> path
    let io = handleResponse <$> liftIO (httpLbs (prepareReq req) manager)
    catch io handleException
  where
    prepareReq :: HTTP.Request -> HTTP.Request
    prepareReq req = req
        { method = verb
        , requestBody = payload
        , requestHeaders = headers
        }
        where
            headers = case reqHeaders of
                Headers x -> x
                Default -> mempty
                None -> mempty

            payload = case body of
                Json x -> (RequestBodyLBS . Aeson.encode) x
                NonJson x -> RequestBodyLBS x
                Empty -> mempty

    handleResponse res = case responseStatus res of
        s | s >= status400 -> (s, Left $ DecodeFailure $ responseBody res)
        s -> (s, Right $ responseBody res)

    handleException = \case
        e@InvalidUrlException{} ->
            throwM e
        HttpExceptionRequest _ e ->
            return (status500, Left (HttpException e))


-- | Makes a request to the API, but throws if it fails.
unsafeRequest
    :: forall a m.
        ( FromJSON a
        , MonadIO m
        , MonadCatch m
        )
    => Context
    -> (Method, Text)
    -> Payload
    -> m (HTTP.Status, a)
unsafeRequest ctx req body = do
    (s, res) <- request ctx req Default body
    either throwM (pure . (s,)) res
