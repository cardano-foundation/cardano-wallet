module Network.Wai.Middleware.Logging
    ( setRequestLogger
    ) where

import Prelude

import Cardano.BM.Trace
    ( Trace, logDebug, logInfo )
import Data.Text
    ( Text )
import Network.HTTP.Types.Status
    ( Status )
import Network.Wai
    ( Request, rawPathInfo, rawQueryString, requestMethod, strictRequestBody )
import Network.Wai.Handler.Warp
    ( setLogger )

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as T
import qualified Network.Wai.Handler.Warp as Warp

-- | Install a request logger on a Warp application and return a response
-- logger middleware.
setRequestLogger
    :: Trace IO Text
    -> Warp.Settings
    -> Warp.Settings
setRequestLogger t =
    setLogger requestLogger
  where
    requestLogger :: Request -> Status -> Maybe Integer -> IO ()
    requestLogger req _ _ = do
        let method = T.decodeUtf8 $ requestMethod req
        let path = T.decodeUtf8 $ rawPathInfo req
        let query = T.decodeUtf8 $ rawQueryString req
        body <- T.decodeUtf8 . BL.toStrict <$> strictRequestBody req
        logInfo  t $ mconcat [ "[", method, "] ", path, query ]
        logDebug t body
