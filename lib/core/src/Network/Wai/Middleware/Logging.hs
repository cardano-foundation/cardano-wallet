{-# LANGUAGE LambdaCase #-}

-- The following is justified by the usage of the 'requestBody' field
-- accessor on the 'Request' object from the Network.Wai.Internal module.
--
-- The deprecation is about not using the function to get the requestBody from
-- a request since it only returns chunks, but we do use it
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Network.Wai.Middleware.Logging
    ( -- * Middleware
      withApiLogger

      -- * Settings
    , newApiLoggerSettings
    , ApiLoggerSettings
    , obfuscateKeys
    ) where

import Prelude

import Cardano.BM.Data.LogItem
    ( LoggerName )
import Cardano.BM.Trace
    ( Trace, logDebug, logError, logInfo, logWarning, modifyName )
import Control.Applicative
    ( (<|>) )
import Control.Arrow
    ( second )
import Control.Concurrent.MVar
    ( MVar, modifyMVar, newMVar )
import Control.Monad
    ( unless )
import Data.Aeson
    ( Value (..) )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Builder
    ( Builder )
import Data.IORef
    ( IORef, atomicModifyIORef, modifyIORef, newIORef, readIORef )
import Data.Text
    ( Text )
import Data.Text.Class
    ( toText )
import Data.Time.Clock
    ( NominalDiffTime, diffUTCTime, getCurrentTime )
import Network.HTTP.Types.Status
    ( Status (..) )
import Network.Wai
    ( Middleware, Request (..), rawPathInfo, rawQueryString, requestMethod )
import Network.Wai.Internal
    ( Response (..), getRequestBodyChunk )

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Installs a request & response logger on a Wai application.
--
-- The logger logs requests' and responses' bodies along with a few other
-- useful piece of information.
withApiLogger
    :: Trace IO Text
    -> ApiLoggerSettings
    -> Middleware
withApiLogger t0 settings app req0 sendResponse = do
    start <- getCurrentTime
    (req, reqBody) <- getRequestBody req0
    rid <- nextRequestId settings
    let t = modifyName (withRequestId rid) t0
    logRequest t req reqBody
    app req $ \res -> do
        builderIO <- newIORef (Nothing, mempty)
        rcvd <- recordChunks builderIO res >>= sendResponse
        time <- flip diffUTCTime start <$> getCurrentTime
        readIORef builderIO >>=
            let fromBuilder = second (BL.toStrict . B.toLazyByteString)
            in uncurry (logResponse t time) . fromBuilder
        return rcvd
  where
    logRequest
        :: Trace IO Text
        -> Request
        -> ByteString
        -> IO ()
    logRequest t req body = do
        let method = T.decodeUtf8 $ requestMethod req
        let path = T.decodeUtf8 $ rawPathInfo req
        let query = T.decodeUtf8 $ rawQueryString req
        let keys = _obfuscateKeys settings req
        let safeBody = T.decodeUtf8 $ sanitize keys body
        logInfo t $ mconcat [ "[", method, "] ", path, query ]
        unless (BS.null body) $ logDebug t safeBody

    logResponse
        :: Trace IO Text
        -> NominalDiffTime
        -> Maybe Status
        -> ByteString
        -> IO ()
    logResponse t time status body = do
        let code = maybe "???" (toText . statusCode) status
        let text = maybe "Status Unknown" (T.decodeUtf8 . statusMessage) status
        let tsec = T.pack $ show time
        let payload = mconcat [ code, " ", text, " in ", tsec ]
        case statusCode <$> status of
            Just s | s == 503 -> logWarning t payload
            Just s | s >= 500 -> logError t payload
            _ -> logInfo t payload
        unless (BS.null body) $ logDebug t (T.decodeUtf8 body)

    withRequestId :: RequestId -> LoggerName -> LoggerName
    withRequestId (RequestId rid) name = mconcat
        [ name, "request-", T.pack $ show rid ]

    -- | Removes sensitive details from valid request payloads and completely
    -- obfuscate invalid payloads.
    sanitize :: [Text] -> ByteString -> ByteString
    sanitize keys bytes = encode' $ case decode' bytes of
        Just (Object o) ->
            Object (foldr (HM.adjust obfuscate) o keys)
        Just v ->
            v
        Nothing ->
            String "Invalid payload: not JSON"
      where
        encode' = BL.toStrict . Aeson.encode
        decode' = Aeson.decode . BL.fromStrict
        obfuscate _ = String "*****"

-- | API logger settings
data ApiLoggerSettings = ApiLoggerSettings
    { _obfuscateKeys :: Request -> [Text]
        -- ^ For a given 'Request', obfuscate the values associated with the
        -- given keys from a JSON object payload.

    , _requestCounter :: MVar Integer
        -- ^ A function to get a unique identifier from a 'Request'
    }

-- | Just a wrapper for readability
newtype RequestId = RequestId Integer

-- | Create a new opaque 'ApiLoggerSettings'
newApiLoggerSettings :: IO ApiLoggerSettings
newApiLoggerSettings = do
    counter <- newMVar 0
    return ApiLoggerSettings
        { _obfuscateKeys = const []
        , _requestCounter = counter
        }

-- | Define a set of top-level object keys that should be obfuscated for a given
-- request in a JSON format.
obfuscateKeys :: (Request -> [Text]) -> ApiLoggerSettings -> ApiLoggerSettings
obfuscateKeys getKeys x =
    x { _obfuscateKeys = getKeys }

-- | Get the next request id, incrementing the request counter.
nextRequestId :: ApiLoggerSettings -> IO RequestId
nextRequestId settings =
    modifyMVar (_requestCounter settings) (\n -> pure (n+1, RequestId n))

{-------------------------------------------------------------------------------
                                  Internals

  We can't actually read the content of a request twice, which has several
  issues.

  First of all, the 'setLogger' method from the 'warp' package isn't really
  helpful as we get a `Request` object in argument, but can't extract its body
  since it has already been consumed by the time it reaches our logger. As a
  consequence, we can merely do logging on superficial request information
  (e.g. headers, status, path etc.).

  See: https://hackage.haskell.org/package/warp-3.2.27/docs/Network-Wai-Handler-Warp.html#v:setLogger

  Then, there exists packages like `wai-extra` with ways to construct detailed
  loggers from a 'Request' object and its body parsed via some ad-hoc helpers
  (see below). However, this package requires a few undesirable dependencies
  (like data-default, or wai-logger) but more importantly, it doesn't make it
  possible to use a custom trace or logger object but instead, forces log outputs
  and behaviors to what's supported by the package.

  See: http://hackage.haskell.org/package/wai-extra-3.0.26/docs/Network-Wai-Middleware-RequestLogger.html

  So, in order to implement our own Request/Response logging middleware, I've
  extracted some relevant bits from the `wai-extra` package in order to:

  - Read the request body, and put it back. There's a small performance cost in
    doing so of course, but that's rather fine for the wallet API which isn't
    meant to serve concurrent client and be under heavy load.

  - Extract the body from a response and make it available in an IORef.

  Source code from the original functions below can be found in 'wai-extra'

  See: http://hackage.haskell.org/package/wai-extra-3.0.26/docs/src/Network.Wai.Middleware.RequestLogger.html

  (Note that functions have been slightly adjust for code-style and, to return
  an extra response status when available instead of only the request body).

-------------------------------------------------------------------------------}

getRequestBody :: Request -> IO (Request, ByteString)
getRequestBody req = do
    body <- loop id
    ichunks <- newIORef body
    let rbody = atomicModifyIORef ichunks $ \case
           [] -> ([], mempty)
           x:y -> (y, x)
    return (req { requestBody = rbody }, mconcat body)
  where
    loop front = do
        bs <- getRequestBodyChunk req
        if BS.null bs
            then return $ front []
            else loop $ front . (bs:)

recordChunks :: IORef (Maybe Status, Builder) -> Response -> IO Response
recordChunks i = \case
    ResponseStream s h sb -> return . ResponseStream s h $
        let capture b (ms, b') = (ms <|> Just s, b' <> b)
        in (\send flush -> sb (\b -> modifyIORef i (capture b) >> send b) flush)
    ResponseBuilder s h b ->
        let capture (ms, b') = (ms <|> Just s, b' <> b)
        in modifyIORef i capture >> return (ResponseBuilder s h b)
    r ->
        return r
