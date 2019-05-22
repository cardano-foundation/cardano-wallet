{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Middleware between Wai <-> Servant to accomodate raw error responses returned
-- by servant. See also 'handleRawError'.

module Network.Wai.Middleware.ServantError
    ( handleRawError
    ) where

import Prelude

import Control.Monad
    ( guard )
import Data.ByteString.Lazy
    ( ByteString )
import Network.HTTP.Types.Header
    ( ResponseHeaders )
import Network.HTTP.Types.Status
    ( statusCode, statusMessage )
import Network.Wai
    ( Middleware, responseHeaders, responseStatus )
import Network.Wai.Internal
    ( Response (..) )
import Servant.Server.Internal.ServantErr
    ( ServantErr (..), responseServantErr )

import qualified Data.Binary.Builder as Binary
import qualified Data.ByteString.Char8 as B8
import qualified Network.HTTP.Types.Status as HTTP

-- | Make sure every error is converted to a suitable application-level error.
--
-- There are many cases where Servant will handle errors itself and reply to a
-- client without even disturbing the application. This is both handy and clunky
-- since our application return errors in a specific format (e.g. JSON, XML
-- ...).
--
-- This is the case for instance if the client hits a non-exiting endpoint of
-- the API, or if the client requests an invalid content-type, etc ...
--
-- Ideally, we would like clients to be able to expect one and only one format,
-- so this middleware allows for manipulating the response returned by a Wai
-- application (what servant boils down to) and adjust the response when
-- necessary. So, any response with or without payload but no content-type will
-- trigger the 'convert' function and offer the caller to adjust the response as
-- needed.
handleRawError
    :: (ServantErr -> ServantErr)
    -- ^ Convert a raw response into something that better fits the application
    -- error
    -> Middleware
handleRawError adjust app req send =
    app req (send . either (responseServantErr . adjust) id . eitherRawError)

-- | Analyze whether a given error is a raw error thrown by Servant before
-- reaching our application layer, or one from our application layer.
eitherRawError
    :: Response
    -> Either ServantErr Response
eitherRawError res =
    let
        status = responseStatus res
        code = statusCode status
        reason = B8.unpack (statusMessage status)
        headers = responseHeaders res
        body = responseBody res
        maybeToEither = maybe
            (Right res)
            (Left . flip (ServantErr code reason) headers)
    in
        maybeToEither $ guard (isRawError status headers) *> body

-- | Raw 'Servant' errors don't have any Content-Type. This is a lean predicate
-- but for lack of any better way of identifying them, that's a best effort.
isRawError
    :: HTTP.Status
    -> ResponseHeaders
    -> Bool
isRawError status headers =
    statusCode status >= 400 && null headers

-- | Extract raw body of a response, only if it suitables for transformation.
-- Servant doesn't return files or streams by default, so if one of the two is
-- met, it means it comes from our application layer anyway.
responseBody
  :: Response
  -> Maybe ByteString
responseBody = \case
    ResponseBuilder _ _ b ->
        Just (Binary.toLazyByteString b)
    ResponseRaw _ r ->
        responseBody r
    ResponseFile{} ->
        Nothing
    ResponseStream{} ->
        Nothing
