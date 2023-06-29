{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- The following is justified by the usage of the 'requestBody' field
-- accessor on the 'Request' object from the Network.Wai.Internal module as a
-- setter. The use of this field as a getter is deprecated, but we need it to
-- mount an existing request body in a request.
{-# OPTIONS_GHC -fno-warn-deprecations #-}
-- TODO: https://input-output.atlassian.net/browse/ADP-2841
{-# OPTIONS_GHC -fno-warn-star-is-type #-}

module Cardano.Wallet.ApiSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api
    ( Api
    )
import Cardano.Wallet.Api.Http.Shelley.Server
    ( IsServerError (..)
    )
import Cardano.Wallet.Api.Malformed
    ( BodyParam (..)
    , ExpectedError (..)
    , Header (..)
    , Malformed
    , PathParam (..)
    , Wellformed
    , malformed
    , wellformed
    )
import Cardano.Wallet.Read.NetworkId
    ( NetworkDiscriminant (..)
    )
import Control.Monad
    ( forM_
    )
import Data.Aeson.QQ
    ( aesonQQ
    )
import Data.Bifunctor
    ( first
    )
import Data.Function
    ( (&)
    )
import Data.IORef
    ( atomicModifyIORef
    , newIORef
    )
import Data.List
    ( delete
    , (\\)
    )
import Data.Map.Strict
    ( Map
    )
import Data.Maybe
    ( mapMaybe
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Text
    ( Text
    )
import Data.Tuple
    ( swap
    )
import Data.Type.Equality
    ( testEquality
    , (:~:) (..)
    )
import Data.Typeable
    ( Typeable
    , typeRep
    )
import Data.Void
    ( Void
    )
import GHC.TypeLits
    ( KnownSymbol
    , symbolVal
    )
import Network.HTTP.Media.RenderHeader
    ( renderHeader
    )
import Network.HTTP.Types.Header
    ( hAccept
    , hContentType
    )
import Network.HTTP.Types.Method
    ( Method
    , methodHead
    , renderStdMethod
    )
import Network.Wai
    ( Request
    , RequestBodyLength (..)
    , defaultRequest
    , pathInfo
    , requestBody
    , requestBodyLength
    , requestHeaders
    , requestMethod
    )
import Network.Wai.Middleware.ServerError
    ( handleRawError
    )
import Network.Wai.Test
    ( SResponse
    , Session
    , assertBody
    , assertHeader
    , assertStatus
    , request
    , runSession
    )
import Servant
    ( Accept (..)
    , Application
    , ReqBody
    , Server
    , StdMethod (..)
    , Verb
    , serve
    )
import Servant.API
    ( Capture
    , OctetStream
    , (:<|>) (..)
    , (:>)
    )
import Servant.API.Verbs
    ( NoContentVerb
    , ReflectMethod (..)
    )
import Test.Hspec
    ( HasCallStack
    , Spec
    , describe
    , it
    , runIO
    , xdescribe
    )
import Type.Reflection
    ( typeOf
    )

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Servant

spec :: Spec
spec = do
    gSpec (everyPathParam api) $ \(SomeTest proxy tests) ->
        describe "Malformed PathParam" $ do
            forM_ tests $ \(req, msg) ->
                it (titleize proxy req)
                    $ runSession (spec_MalformedParam req msg) application

    gSpec (everyBodyParam api) $ \(SomeTest proxy tests) ->
        describe "Malformed BodyParam" $ do
            forM_ tests $ \(req, msg) ->
                it (titleize proxy req)
                    $ runSession (spec_MalformedParam req msg) application

    gSpec (everyHeader api) $ \(SomeTest proxy tests) -> do
        case typeOf proxy `testEquality` typeOf (Proxy @"Accept") of
            Just Refl -> describe "Malformed Headers"
                $ forM_ tests
                $ \(req, msg) ->
                    it (titleize proxy req)
                        $ runSession (spec_WrongAcceptHeader req msg) application
            Nothing ->
                pure ()

        case typeOf proxy `testEquality` typeOf (Proxy @"Content-Type") of
            Just Refl -> describe "Malformed Headers"
                $ forM_ tests
                $ \(req, msg) ->
                    it (titleize proxy req)
                        $ runSession (spec_WrongContentTypeHeader req msg) application
            Nothing ->
                pure ()

    gSpec (everyAllowedMethod api) $ \(SomeTest proxy tests) ->
        describe "Not Allowed Methods"
            $ forM_ tests
            $ \(req, msg) ->
                it (titleize proxy req)
                    $ runSession (spec_NotAllowedMethod req msg) application

assertErrorResponse
    :: HasCallStack
    => Int
    -- ^ Expected status
    -> Text
    -- ^ Expected error code string
    -> ExpectedError
    -> SResponse
    -> Session ()
assertErrorResponse status code (ExpectedError msg) response = do
    response & assertStatus status
    response & assertHeader "Content-Type" "application/json;charset=utf-8"
    response
        & assertBody
            (Aeson.encode [aesonQQ|{ "code": #{code}, "message": #{msg} }|])

spec_MalformedParam :: Request -> ExpectedError -> Session ()
spec_MalformedParam malformedRequest expectedError = do
    response <- request malformedRequest
    assertErrorResponse 400 "bad_request" expectedError response

spec_WrongAcceptHeader :: Request -> ExpectedError -> Session ()
spec_WrongAcceptHeader malformedRequest expectedError = do
    response <- request malformedRequest
    assertErrorResponse 406 "not_acceptable" expectedError response

spec_WrongContentTypeHeader :: Request -> ExpectedError -> Session ()
spec_WrongContentTypeHeader malformedRequest expectedError = do
    response <- request malformedRequest
    assertErrorResponse 415 "unsupported_media_type" expectedError response

spec_NotAllowedMethod :: Request -> ExpectedError -> Session ()
spec_NotAllowedMethod malformedRequest expectedError = do
    response <- request malformedRequest
    assertErrorResponse 405 "method_not_allowed" expectedError response

--
-- Generic API Spec
--
data SomeTest where
    SomeTest
        :: forall k (a :: k)
         . (Typeable a, Typeable k)
        => Proxy a
        -> [(Request, ExpectedError)]
        -> SomeTest

class Typeable api => GenericApiSpec api where
    gSpec :: api -> (SomeTest -> Spec) -> Spec
    gSpec _ _ = xdescribe (show $ typeRep $ Proxy @api) (pure ())

instance (GenericApiSpec a, GenericApiSpec b) => GenericApiSpec (a :<|> b) where
    gSpec (a :<|> b) toS = gSpec a toS >> gSpec b toS

instance GenericApiSpec Request where
    gSpec _ _ = pure ()

instance GenericApiSpec a => GenericApiSpec [a] where
    gSpec xs toSpec = foldr (\x y -> gSpec x toSpec >> y) (pure ()) xs

instance
    ( Typeable a
    , Malformed (PathParam a)
    )
    => GenericApiSpec (PathParam a -> [Request])
    where
    gSpec toRequest toSpec = do
        let tests :: [[(Request, ExpectedError)]]
            tests =
                fmap
                    (\(xs, e) -> fmap (,e) xs)
                    (first toRequest <$> malformed @(PathParam a))
        forM_ tests (toSpec . SomeTest (Proxy @a))

instance
    ( Typeable a
    , Wellformed (PathParam a)
    , GenericApiSpec (PathParam a -> [Request])
    , Wellformed (PathParam b)
    , GenericApiSpec (PathParam b -> [Request])
    )
    => GenericApiSpec (PathParam a -> PathParam b -> [Request])
    where
    gSpec toRequest toSpec = do
        forM_ wellformed $ \w -> gSpec (toRequest w) toSpec
        forM_ wellformed $ \w -> gSpec (`toRequest` w) toSpec

instance
    ( Typeable a
    , Wellformed (PathParam a)
    , GenericApiSpec (PathParam a -> [Request])
    , Typeable b
    , Wellformed (PathParam b)
    , GenericApiSpec (PathParam b -> [Request])
    , Wellformed (PathParam c)
    , GenericApiSpec (PathParam c -> [Request])
    )
    => GenericApiSpec (PathParam a -> PathParam b -> PathParam c -> [Request])
    where
    gSpec toRequest toSpec = do
        forM_
            [(b, c) | b <- wellformed, c <- wellformed]
            (\(b, c) -> gSpec (\a -> toRequest a b c) toSpec)

        forM_
            [(a, c) | a <- wellformed, c <- wellformed]
            (\(a, c) -> gSpec (\b -> toRequest a b c) toSpec)

        forM_
            [(a, b) | a <- wellformed, b <- wellformed]
            (\(a, b) -> gSpec (\c -> toRequest a b c) toSpec)

-- The lambda above helps readability and make the pattern obvious
{- HLINT ignore "Avoid lambda" -}

instance
    ( Typeable a
    , Malformed (BodyParam a)
    )
    => GenericApiSpec (BodyParam a -> IO [Request])
    where
    gSpec toRequest toSpec = do
        let tests :: [(IO [Request], ExpectedError)]
            tests = first toRequest <$> malformed @(BodyParam a)
        toSpec . SomeTest (Proxy @a) . distributeFirst =<< traverseLeft runIO tests
      where
        -- e.g. [IO Request, ExpectedError] -> IO [Request, ExpectedError]
        traverseLeft
            :: Applicative m
            => (l0 -> m l1)
            -> [(l0, r)]
            -> m [(l1, r)]
        traverseLeft fn xs =
            fmap swap <$> traverse (traverse fn) (swap <$> xs)

instance
    ( KnownSymbol h
    , Typeable ct
    , Malformed (Header h ct)
    )
    => GenericApiSpec (Header h ct -> [Request])
    where
    gSpec toRequest toSpec = do
        let tests :: [([Request], ExpectedError)]
            tests = first toRequest <$> malformed @(Header h ct)
        toSpec . SomeTest (Proxy @h) . distributeFirst $ tests

instance
    ( Typeable ct0
    , KnownSymbol h0
    , Wellformed (Header h0 ct0)
    , Wellformed (Header h1 ct1)
    , GenericApiSpec (Header h0 ct0 -> [Request])
    , GenericApiSpec (Header h1 ct1 -> [Request])
    )
    => GenericApiSpec (Header h0 ct0 -> Header h1 ct1 -> [Request])
    where
    gSpec toRequest toSpec = do
        forM_ wellformed $ \w -> gSpec (toRequest w) toSpec
        forM_ wellformed $ \w -> gSpec (`toRequest` w) toSpec

instance GenericApiSpec (Map [Text] [Method]) where
    gSpec allowedMethods toSpec = do
        toSpec
            $ SomeTest (Proxy @Void)
            $ mconcat
            $ for (Map.toList allowedMethods)
            $ \(pathInfo, methods) ->
                forMaybe (allMethodsButHead \\ methods) $ \requestMethod ->
                    if shouldSkipRequest requestMethod pathInfo
                        then Nothing
                        else Just (defaultRequest{pathInfo, requestMethod}, msg)
      where
        for = flip map
        forMaybe = flip mapMaybe
        msg =
            "You've reached a known endpoint but I don't know how to handle the \
            \HTTP method specified. Please double-check both the endpoint and \
            \the method: one of them is likely to be incorrect (for example: \
            \POST instead of PUT, or GET instead of POST...)."

        allMethodsButHead :: [Method]
        allMethodsButHead =
            delete methodHead (renderStdMethod <$> [minBound .. maxBound])

        shouldSkipRequest :: Method -> [Text] -> Bool
        shouldSkipRequest method = \case
            ["stake-pools", "*", "wallets", _] -> method /= "DELETE"
            _ -> False

--
-- Construct test cases from the API
--

application :: Application
application =
    serve api server
        & handleRawError (curry toServerError)

api :: Proxy (Api ('Testnet 0))
api = Proxy

server :: Server (Api ('Testnet 0))
server =
    error
        "No test from this module should actually reach handlers of the server. \
        \Tests are indeed all testing the internal machinery of Servant + Wai and \
        \the way they interact with the outside world. Only valid requests are \
        \delegated to our handlers."

everyPathParam :: GEveryEndpoints api => Proxy api -> MkPathRequest api
everyPathParam proxy = gEveryPathParam proxy defaultRequest

defaultApiRequest :: Request
defaultApiRequest =
    defaultRequest
        { requestHeaders =
            [ (hContentType, "application/json")
            , (hAccept, "*/*")
            ]
        }

everyBodyParam :: GEveryEndpoints api => Proxy api -> MkBodyRequest api
everyBodyParam proxy = gEveryBodyParam proxy defaultApiRequest

everyHeader :: GEveryEndpoints api => Proxy api -> MkHeaderRequest api
everyHeader proxy = gEveryHeader proxy defaultRequest

everyAllowedMethod :: GEveryEndpoints api => Proxy api -> Map [Text] [Method]
everyAllowedMethod proxy =
    Map.fromListWith (++) (toTuple <$> gEveryEndpoint proxy)
  where
    toTuple :: Request -> ([Text], [Method])
    toTuple req = (reverse (pathInfo req), [requestMethod req])

class GEveryEndpoints api where
    gEveryEndpoint :: Proxy api -> [Request]

    type MkPathRequest api :: *
    gEveryPathParam :: Proxy api -> Request -> MkPathRequest api

    type MkBodyRequest api :: *
    gEveryBodyParam :: Proxy api -> Request -> MkBodyRequest api

    type MkHeaderRequest api :: *
    gEveryHeader :: Proxy api -> Request -> MkHeaderRequest api

-- TODO
-- Capture request query params as QueryParam
--
-- newtype QueryParam t = QueryParam Text
--     deriving (Typeable)
--
-- type MkQueryRequest api :: *
-- gEveryQueryParams :: Proxy api -> Request -> MkQueryRequest api

instance
    ( GEveryEndpoints a
    , GEveryEndpoints b
    )
    => GEveryEndpoints (a :<|> b)
    where
    gEveryEndpoint _ =
        gEveryEndpoint (Proxy @a)
            ++ gEveryEndpoint (Proxy @b)

    type MkPathRequest (a :<|> b) = MkPathRequest a :<|> MkPathRequest b
    gEveryPathParam _ req =
        do
            gEveryPathParam (Proxy @a) req
            :<|> gEveryPathParam (Proxy @b) req

    type MkBodyRequest (a :<|> b) = MkBodyRequest a :<|> MkBodyRequest b
    gEveryBodyParam _ req =
        do
            gEveryBodyParam (Proxy @a) req
            :<|> gEveryBodyParam (Proxy @b) req

    type MkHeaderRequest (a :<|> b) = MkHeaderRequest a :<|> MkHeaderRequest b
    gEveryHeader _ req =
        do
            gEveryHeader (Proxy @a) req
            :<|> gEveryHeader (Proxy @b) req

instance
    ( ReflectMethod m
    , Accept ct
    )
    => GEveryEndpoints (Verb (m :: StdMethod) s '[ct] a)
    where
    gEveryEndpoint _ =
        [ defaultRequest
            { requestMethod = reflectMethod $ Proxy @m
            , requestHeaders =
                [ (hAccept, renderHeader $ contentType $ Proxy @ct)
                ]
            }
        ]

    type MkPathRequest (Verb m s '[ct] a) = [Request]
    gEveryPathParam _ req =
        [req{requestMethod = reflectMethod (Proxy @m)}]

    type MkBodyRequest (Verb m s '[ct] a) = [Request]
    gEveryBodyParam _ req =
        [req{requestMethod = reflectMethod (Proxy @m)}]

    type MkHeaderRequest (Verb m s '[ct] a) = Header "Accept" ct -> [Request]
    gEveryHeader _ req (Header h) =
        [ req
            { requestMethod = reflectMethod $ Proxy @m
            , requestHeaders = requestHeaders req ++ [(hAccept, h)]
            }
        ]

instance
    ReflectMethod m
    => GEveryEndpoints (NoContentVerb (m :: StdMethod))
    where
    gEveryEndpoint _ =
        [defaultRequest{requestMethod = reflectMethod (Proxy @m)}]

    type MkPathRequest (NoContentVerb m) = [Request]
    gEveryPathParam _ req =
        [req{requestMethod = reflectMethod (Proxy @m)}]

    type MkBodyRequest (NoContentVerb m) = [Request]
    gEveryBodyParam _ req =
        [req{requestMethod = reflectMethod (Proxy @m)}]

    type MkHeaderRequest (NoContentVerb m) = [Request]
    gEveryHeader _ req =
        [req{requestMethod = reflectMethod (Proxy @m)}]

instance
    ( Wellformed (PathParam t)
    , GEveryEndpoints sub
    )
    => GEveryEndpoints (Capture p t :> sub)
    where
    gEveryEndpoint _ =
        concatMap (\t -> addPathFragment t <$> gEveryEndpoint (Proxy @sub)) ts
      where
        ts = wellformed :: [PathParam t]

    type MkPathRequest (Capture p t :> sub) = PathParam t -> MkPathRequest sub
    gEveryPathParam _ req t =
        gEveryPathParam (Proxy @sub) (addPathFragment t req)

    type MkBodyRequest (Capture p t :> sub) = [MkBodyRequest sub]
    gEveryBodyParam _ req =
        gEveryBodyParam (Proxy @sub) . (`addPathFragment` req) <$> ts
      where
        ts = wellformed :: [PathParam t]

    type MkHeaderRequest (Capture p t :> sub) = [MkHeaderRequest sub]
    gEveryHeader _ req =
        gEveryHeader (Proxy @sub) . (`addPathFragment` req) <$> ts
      where
        ts = wellformed :: [PathParam t]

instance
    ( KnownSymbol s
    , GEveryEndpoints sub
    )
    => GEveryEndpoints (s :> sub)
    where
    gEveryEndpoint _ =
        addPathFragment t <$> gEveryEndpoint (Proxy @sub)
      where
        t = PathParam $ T.pack $ symbolVal (Proxy @s)

    type MkPathRequest (s :> sub) = MkPathRequest sub
    gEveryPathParam _ req =
        gEveryPathParam (Proxy @sub) (addPathFragment str req)
      where
        str = PathParam $ T.pack $ symbolVal (Proxy @s)

    type MkBodyRequest (s :> sub) = MkBodyRequest sub
    gEveryBodyParam _ req =
        gEveryBodyParam (Proxy @sub) (addPathFragment t req)
      where
        t = PathParam $ T.pack $ symbolVal (Proxy @s)

    type MkHeaderRequest (s :> sub) = MkHeaderRequest sub
    gEveryHeader _ req =
        gEveryHeader (Proxy @sub) (addPathFragment t req)
      where
        t = PathParam $ T.pack $ symbolVal (Proxy @s)

-- We can provide a more specific instance than "ReqBody [ct]" to override the
-- general behaviour with more specific OctetStream behaviour.
--
-- From the GHC user guide:
--
-- GHC requires that it be unambiguous which instance declaration should be used
-- to resolve a type-class constraint. GHC also provides a way to loosen the
-- instance resolution, by allowing more than one instance to match, provided
-- there is a most specific one.
--
-- - Eliminate any candidate IX for which there is another candidate IY such
--   that both of the following hold:
--   - IY is strictly more specific than IX. That is, IY is a substitution
--     instance of IX but not vice versa.
--   - Either IX is overlappable, or IY is overlapping. (This “either/or”
--     design, rather than a “both/and” design, allow a client to deliberately
--     override an instance from a library, without requiring a change to the
--     library.)
instance
    {-# OVERLAPPING #-}
    GEveryEndpoints sub
    => GEveryEndpoints (ReqBody '[OctetStream] a :> sub)
    where
    gEveryEndpoint _ =
        gEveryEndpoint (Proxy @sub)

    type MkPathRequest (ReqBody '[OctetStream] a :> sub) = MkPathRequest sub
    gEveryPathParam _ =
        gEveryPathParam (Proxy @sub)

    type
        MkBodyRequest (ReqBody '[OctetStream] a :> sub) =
            BodyParam a -> IO (MkBodyRequest sub)
    gEveryBodyParam _ req b =
        gEveryBodyParam (Proxy @sub)
            <$> ( setRequestBody
                    b
                    req
                        { requestHeaders =
                            [ (hContentType, "application/octet-stream")
                            , (hAccept, "*/*")
                            ]
                        }
                )

    type
        MkHeaderRequest (ReqBody '[OctetStream] a :> sub) =
            Header "Content-Type" OctetStream -> MkHeaderRequest sub
    gEveryHeader _ req (Header h) =
        gEveryHeader (Proxy @sub)
            $ req
                { requestHeaders = requestHeaders req ++ [(hContentType, h)]
                }

instance
    GEveryEndpoints sub
    => GEveryEndpoints (ReqBody '[ct] a :> sub)
    where
    gEveryEndpoint _ =
        gEveryEndpoint (Proxy @sub)

    type MkPathRequest (ReqBody '[ct] a :> sub) = MkPathRequest sub
    gEveryPathParam _ =
        gEveryPathParam (Proxy @sub)

    type
        MkBodyRequest (ReqBody '[ct] a :> sub) =
            BodyParam a -> IO (MkBodyRequest sub)
    gEveryBodyParam _ req b =
        gEveryBodyParam (Proxy @sub) <$> (setRequestBody b req)

    type
        MkHeaderRequest (ReqBody '[ct] a :> sub) =
            Header "Content-Type" ct -> MkHeaderRequest sub
    gEveryHeader _ req (Header h) =
        gEveryHeader (Proxy @sub)
            $ req
                { requestHeaders = requestHeaders req ++ [(hContentType, h)]
                }

instance
    GEveryEndpoints sub
    => GEveryEndpoints (Servant.QueryParam a b :> sub)
    where
    gEveryEndpoint _ =
        gEveryEndpoint (Proxy @sub)

    type MkPathRequest (Servant.QueryParam a b :> sub) = MkPathRequest sub
    gEveryPathParam _ =
        gEveryPathParam (Proxy @sub)

    type MkBodyRequest (Servant.QueryParam a b :> sub) = MkBodyRequest sub
    gEveryBodyParam _ =
        gEveryBodyParam (Proxy @sub)

    type MkHeaderRequest (Servant.QueryParam a b :> sub) = MkHeaderRequest sub
    gEveryHeader _ =
        gEveryHeader (Proxy @sub)

instance
    GEveryEndpoints sub
    => GEveryEndpoints (Servant.QueryFlag s :> sub)
    where
    gEveryEndpoint _ =
        gEveryEndpoint (Proxy @sub)

    type MkPathRequest (Servant.QueryFlag s :> sub) = MkPathRequest sub
    gEveryPathParam _ =
        gEveryPathParam (Proxy @sub)

    type MkBodyRequest (Servant.QueryFlag s :> sub) = MkBodyRequest sub
    gEveryBodyParam _ =
        gEveryBodyParam (Proxy @sub)

    type MkHeaderRequest (Servant.QueryFlag s :> sub) = MkHeaderRequest sub
    gEveryHeader _ =
        gEveryHeader (Proxy @sub)

--
-- Helpers
--

distributeFirst :: [([x], y)] -> [(x, y)]
distributeFirst zs = [(x, y) | (xs, y) <- zs, x <- xs]

addPathFragment :: PathParam t -> Request -> Request
addPathFragment (PathParam fragment) req =
    req
        { pathInfo = pathInfo req ++ [fragment]
        }

setRequestBody :: BodyParam b -> Request -> IO Request
setRequestBody (BodyParam bytes) req = do
    ref <- newIORef $ BL.toChunks bytes
    pure
        req
            { requestBodyLength = KnownLength $ fromIntegral $ BL.length bytes
            , requestBody = atomicModifyIORef ref $ \case
                [] -> ([], mempty)
                h : q -> (q, h)
            }

titleize
    :: forall t
     . Typeable t
    => Proxy t
    -> Request
    -> String
titleize proxy req =
    unwords
        [ if proxyStr == "(Void)" then "" else proxyStr
        , B8.unpack (requestMethod req)
        , "/" <> T.unpack (T.intercalate "/" $ pathInfo req)
        ]
  where
    proxyStr = "(" <> show (typeRep proxy) <> ")"
