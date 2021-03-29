{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2021-* IOHK
-- License: Apache-2.0
--
-- Wai middleware to sanitize JSON outputs for clients which cannot parse big
-- integers. This is the case of JavaScript clients which choke on integer
-- values bigger than 2^53-1.
--
-- As a work-around this middleware will, when instrumented, crawl the JSON
-- response and re-encode big integers as string. To trigger this behavior,
-- clients have to provide an explicit 'X-Max-Safe-Integer' header with the
-- maximum integer value they support, values greater than the maximum will be
-- automatically turned into strings.

module Network.Wai.Middleware.BigInt
    ( handleBigIntAsString
    , traverseJSON
    ) where

import Prelude

import Control.Monad
    ( (<=<) )
import Data.Aeson
    ( ToJSON (..), Value (..) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Scientific
    ( floatingOrInteger )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), ToText (..) )
import Network.HTTP.Types.Header
    ( Header )
import Network.Wai
    ( Middleware, Request, modifyResponse, requestHeaders )
import Network.Wai.Internal
    ( Response (..) )

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Builder as BL
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Text.Encoding as T

-- | Convert JSON big integer to strings when explicitely requested by client.
--
-- e.g. if 'X-Max-Safe-Integer: 42' is set as a request header, then any integer
-- strictly greater than 42 will be converted as string.
handleBigIntAsString :: Middleware
handleBigIntAsString =
    whenRequest hasMaxSafeInteger (modifyResponse . bigIntAsString)

--
-- Response Transformation
--

-- | Transform all integers bigger than 'maxSafeInteger' into a string in a JSON
-- response. Does nothing for non-json.
bigIntAsString :: Integer -> Response -> Response
bigIntAsString maxSafeInteger = withResponse $ \bytes -> do
    case Aeson.decode bytes of
        Just rawJSON ->
            let sanitizedJSON = replaceBigInt maxSafeInteger rawJSON in
            Aeson.encode sanitizedJSON
        Nothing ->
            bytes
  where
    json :: (Applicative f, ToJSON a) => a -> f Value
    json = pure . toJSON

    replaceBigInt :: Integer -> Value -> Value
    replaceBigInt maxN =
        runIdentity . traverseJSON
            (\i -> if i > maxN then json (toText i) else json i)
            json

    withResponse
      :: (BL.ByteString -> BL.ByteString)
      -> Response
      -> Response
    withResponse withByteString = \case
        ResponseBuilder a0 a1 bytes ->
            ResponseBuilder a0 a1 (transform bytes)
        ResponseRaw _ r ->
            withResponse withByteString r
        r@ResponseFile{} ->
            r
        r@ResponseStream{} ->
            r
      where
        transform = BL.lazyByteString . withByteString . BL.toLazyByteString

-- | Recursively traverse a JSON structure with the given traversals.
--
-- NOTE (1): This allows for an 'Applicative' to run so that the function can be
-- re-used in various contexts for testing purposes.
--
-- NOTE (2): Also allows for acting upon all 'Text' value, also for facilitating
-- testing and comparisons.
traverseJSON
    :: Applicative f
    => (Integer -> f Value)
    -> (Text -> f Value)
    -> Value
    -> f Value
traverseJSON forInteger forString value =
    case value of
        Number n -> case floatingOrInteger @Double n of
            Left{}  -> pure value
            Right i -> forInteger i
        Object o -> Object <$> traverse (traverseJSON forInteger forString) o
        Array xs -> Array  <$> traverse (traverseJSON forInteger forString) xs
        String s -> forString s
        Bool {}  -> pure value
        Null     -> pure value

--
-- Request Predicate
--

-- | Return 'Just (i :: Integer)' when the request contains a
-- 'X-Max-Safe-Integer' header, where 'i' is the parsed value set by the header.
hasMaxSafeInteger :: Request -> Maybe Integer
hasMaxSafeInteger = parseHeader <=< L.find isMaxSafeInteger . requestHeaders
  where
    isMaxSafeInteger :: Header -> Bool
    isMaxSafeInteger (hName, _) =
        hName == "X-Max-Safe-Integer"

    parseHeader :: Header -> Maybe Integer
    parseHeader (_, v) =
        either (const Nothing) Just (fromText $ T.decodeUtf8 v)

-- | Execute a middleware only if the request contains some specific value,
-- forwarded to the middleware.
whenRequest :: (Request -> Maybe a) -> (a -> Middleware) -> Middleware
whenRequest predicate middleware app req =
    case predicate req of
        Just a  -> middleware a app req
        Nothing -> app req
