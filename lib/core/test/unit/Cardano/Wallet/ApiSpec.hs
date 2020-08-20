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

-- The following is justified by the usage of the 'requestBody' field
-- accessor on the 'Request' object from the Network.Wai.Internal module as a
-- setter. The use of this field as a getter is deprecated, but we need it to
-- mount an existing request body in a request.
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- See comment in Cardano.Wallet.Jormungandr.Compatibility
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.ApiSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api
    ( Api )
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
import Cardano.Wallet.Api.Server
    ( LiftHandler (..) )
import Cardano.Wallet.Api.Types
    ( ApiStakePool
    , DecodeAddress (..)
    , DecodeStakeAddress (..)
    , EncodeAddress (..)
    , EncodeStakeAddress (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..), ChimericAccount (..) )
import Control.Arrow
    ( first )
import Control.Monad
    ( forM_ )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.Function
    ( (&) )
import Data.IORef
    ( atomicModifyIORef, newIORef )
import Data.List
    ( (\\) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( mapMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Tuple
    ( swap )
import Data.Type.Equality
    ( (:~:) (..), testEquality )
import Data.Typeable
    ( Typeable, typeRep )
import Data.Void
    ( Void )
import GHC.TypeLits
    ( KnownSymbol, symbolVal )
import Network.HTTP.Media.RenderHeader
    ( renderHeader )
import Network.HTTP.Types.Header
    ( hAccept, hContentType )
import Network.HTTP.Types.Method
    ( Method )
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
    ( handleRawError )
import Network.Wai.Test
    ( Session, assertBody, assertStatus, request, runSession )
import Servant
    ( Accept (..), Application, ReqBody, Server, StdMethod (..), Verb, serve )
import Servant.API
    ( (:<|>) (..), (:>), Capture )
import Servant.API.Stream
    ( Stream )
import Servant.API.Verbs
    ( NoContentVerb, ReflectMethod (..) )
import Test.Hspec
    ( Spec, describe, it, runIO, xdescribe )
import Type.Reflection
    ( typeOf )

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
            forM_ tests $ \(req, msg) -> it (titleize proxy req) $
                runSession (spec_MalformedParam req msg) application

    gSpec (everyBodyParam api) $ \(SomeTest proxy tests) ->
        describe "Malformed BodyParam" $ do
            forM_ tests $ \(req, msg) -> it (titleize proxy req) $
                runSession (spec_MalformedParam req msg) application

    gSpec (everyHeader api) $ \(SomeTest proxy tests) -> do
        case typeOf proxy `testEquality` typeOf (Proxy @"Accept") of
            Just Refl -> describe "Malformed Headers" $
                forM_ tests $ \(req, msg) -> it (titleize proxy req) $
                    runSession (spec_WrongAcceptHeader req msg) application
            Nothing ->
                pure ()

        case typeOf proxy `testEquality` typeOf (Proxy @"Content-Type") of
            Just Refl -> describe "Malformed Headers" $
                forM_ tests $ \(req, msg) -> it (titleize proxy req) $
                    runSession (spec_WrongContentTypeHeader req msg) application
            Nothing ->
                pure ()

    gSpec (everyAllowedMethod api) $ \(SomeTest proxy tests) ->
        describe "Not Allowed Methods" $
            forM_ tests $ \(req, msg) -> it (titleize proxy req) $
                runSession (spec_NotAllowedMethod req msg) application

spec_MalformedParam :: Request -> ExpectedError -> Session ()
spec_MalformedParam malformedRequest (ExpectedError msg) = do
    response <- request malformedRequest
    response & assertStatus 400
    response & assertBody (Aeson.encode [aesonQQ|
        { "code": "bad_request"
        , "message": #{msg}
        }|])

spec_WrongAcceptHeader :: Request -> ExpectedError -> Session ()
spec_WrongAcceptHeader malformedRequest (ExpectedError msg) = do
    response <- request malformedRequest
    response & assertStatus 406
    response & assertBody (Aeson.encode [aesonQQ|
        { "code": "not_acceptable"
        , "message": #{msg}
        }|])

spec_WrongContentTypeHeader :: Request -> ExpectedError -> Session ()
spec_WrongContentTypeHeader malformedRequest (ExpectedError msg) = do
    response <- request malformedRequest
    response & assertStatus 415
    response & assertBody (Aeson.encode [aesonQQ|
        { "code": "unsupported_media_type"
        , "message": #{msg}
        }|])

spec_NotAllowedMethod :: Request -> ExpectedError -> Session ()
spec_NotAllowedMethod malformedRequest (ExpectedError msg) = do
    response <- request malformedRequest
    response & assertStatus 405
    response & assertBody (Aeson.encode [aesonQQ|
        { "code": "method_not_allowed"
        , "message": #{msg}
        }|])

--
-- Generic API Spec
--
data SomeTest where
    SomeTest
        :: forall k (a :: k). (Typeable a, Typeable k)
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

instance
    ( Typeable a, Malformed (PathParam a)
    ) => GenericApiSpec (PathParam a -> Request)
  where
    gSpec toRequest toSpec = toSpec $
        SomeTest (Proxy @a) (first toRequest <$> malformed @(PathParam a))

instance
    ( Typeable a, Wellformed (PathParam a)
    , GenericApiSpec (PathParam a -> Request)
    , Wellformed (PathParam b)
    , GenericApiSpec (PathParam b -> Request)
    ) => GenericApiSpec (PathParam a -> PathParam b -> Request)
  where
    gSpec toRequest toSpec = do
        gSpec (toRequest wellformed) toSpec
        gSpec (`toRequest` wellformed) toSpec

instance
    ( Typeable a, Malformed (BodyParam a)
    ) => GenericApiSpec (BodyParam a -> IO Request)
  where
    gSpec toRequest toSpec = do
        let tests = first toRequest <$> malformed @(BodyParam a)
        toSpec . SomeTest (Proxy @a) =<< traverseLeft runIO tests
      where
        -- e.g. [IO Request, ExpectedError] -> IO [Request, ExpectedError]
        traverseLeft
            :: Applicative m
            => (l0 -> m l1) -> [(l0, r)] -> m [(l1, r)]
        traverseLeft fn xs =
            fmap swap <$> traverse (traverse fn) (swap <$> xs)

instance
    ( KnownSymbol h
    , Typeable ct
    , Malformed (Header h ct)
    ) => GenericApiSpec (Header h ct -> Request)
  where
    gSpec toRequest toSpec = toSpec $
        SomeTest (Proxy @h) (first toRequest <$> malformed @(Header h ct))

instance
    ( Typeable ct0, KnownSymbol h0, Wellformed (Header h0 ct0)
    , GenericApiSpec (Header h0 ct0 -> Request)
    , Wellformed (Header h1 ct1)
    , GenericApiSpec (Header h1 ct1 -> Request)
    ) => GenericApiSpec (Header h0 ct0 -> Header h1 ct1 -> Request)
  where
    gSpec toRequest toSpec = do
        gSpec (toRequest wellformed) toSpec
        gSpec (`toRequest` wellformed) toSpec

instance GenericApiSpec (Map [Text] [Method])
  where
    gSpec allowedMethods toSpec = do
        toSpec $ SomeTest (Proxy @Void) $ mconcat $
            for (Map.toList allowedMethods) $ \(pathInfo, methods) ->
                forMaybe (allMethods \\ methods) $ \requestMethod ->
                    if isWhiteListed pathInfo requestMethod
                    then Nothing
                    else Just (defaultRequest { pathInfo, requestMethod }, msg)
      where
        for = flip map
        forMaybe = flip mapMaybe
        msg =
            "You've reached a known endpoint but I don't know how to handle the \
            \HTTP method specified. Please double-check both the endpoint and \
            \the method: one of them is likely to be incorrect (for example: \
            \POST instead of PUT, or GET instead of POST...)."

        allMethods :: [Method]
        allMethods =
            ["GET","PUT","POST","PATCH","DELETE","CONNECT","TRACE","OPTIONS"]

        isWhiteListed :: [Text] -> Method -> Bool
        isWhiteListed
            [ "stake-pools", "*", "wallets", _ ] "PUT" = True
        isWhiteListed
            _ _ = False


--
-- Construct test cases from the API
--

application :: Application
application = serve api server
    & handleRawError (curry handler)

-- Note: Doesn't validate the jormungandr api.
api :: Proxy (Api ('Testnet 0) ApiStakePool)
api = Proxy

server :: Server (Api ('Testnet 0) ApiStakePool)
server = error
    "No test from this module should actually reach handlers of the server. \
    \Tests are indeed all testing the internal machinery of Servant + Wai and \
    \the way they interact with the outside world. Only valid requests are \
    \delegated to our handlers."

-- Dummy instances
instance EncodeAddress ('Testnet 0) where
    encodeAddress = T.pack . show

instance DecodeAddress ('Testnet 0) where
    decodeAddress _ = pure (Address "<addr>")

-- Dummy instances
instance EncodeStakeAddress ('Testnet 0) where
    encodeStakeAddress = T.pack . show

instance DecodeStakeAddress ('Testnet 0) where
    decodeStakeAddress _ = pure (ChimericAccount "<acct>")

everyPathParam :: GEveryEndpoints api => Proxy api -> MkPathRequest api
everyPathParam proxy = gEveryPathParam proxy defaultRequest

everyBodyParam :: GEveryEndpoints api => Proxy api -> MkBodyRequest api
everyBodyParam proxy = gEveryBodyParam proxy $ defaultRequest
    { requestHeaders =
        [ (hContentType, "application/json")
        , (hAccept, "application/json")
        ]
    }

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
    ) => GEveryEndpoints (a :<|> b)
  where
    gEveryEndpoint _ =
        gEveryEndpoint (Proxy @a)
      ++
        gEveryEndpoint (Proxy @b)

    type MkPathRequest (a :<|> b) = MkPathRequest a :<|> MkPathRequest b
    gEveryPathParam _ req = do
        gEveryPathParam (Proxy @a) req
      :<|>
        gEveryPathParam (Proxy @b) req

    type MkBodyRequest (a :<|> b) = MkBodyRequest a :<|> MkBodyRequest b
    gEveryBodyParam _ req = do
        gEveryBodyParam (Proxy @a) req
      :<|>
        gEveryBodyParam (Proxy @b) req

    type MkHeaderRequest (a :<|> b) = MkHeaderRequest a :<|> MkHeaderRequest b
    gEveryHeader _ req = do
        gEveryHeader (Proxy @a) req
      :<|>
        gEveryHeader (Proxy @b) req

instance
    ( ReflectMethod m
    , Accept ct
    ) => GEveryEndpoints (Verb (m :: StdMethod) s '[ct] a)
  where
    gEveryEndpoint _ =
        [ defaultRequest
            { requestMethod = reflectMethod $ Proxy @m
            , requestHeaders =
                [ (hAccept, renderHeader $ contentType $ Proxy @ct)
                ]
            }
        ]

    type MkPathRequest (Verb m s '[ct] a) = Request
    gEveryPathParam _ req =
        req { requestMethod = reflectMethod (Proxy @m) }

    type MkBodyRequest (Verb m s '[ct] a) = Request
    gEveryBodyParam _ req =
        req { requestMethod = reflectMethod (Proxy @m) }

    type MkHeaderRequest (Verb m s '[ct] a) = Header "Accept" ct -> Request
    gEveryHeader _ req (Header h) =
        req { requestMethod = reflectMethod $ Proxy @m
            , requestHeaders = requestHeaders req ++ [ (hAccept, h) ]
            }

instance
    ( ReflectMethod m
    , Accept ct
    ) => GEveryEndpoints (Stream (m :: StdMethod) s f ct a)
  where
    gEveryEndpoint _ =
        [ defaultRequest
            { requestMethod = reflectMethod $ Proxy @m
            , requestHeaders =
                [ (hAccept, renderHeader $ contentType $ Proxy @ct)
                ]
            }
        ]

    type MkPathRequest (Stream m s f ct a) = Request
    gEveryPathParam _ req =
        req { requestMethod = reflectMethod (Proxy @m) }

    type MkBodyRequest (Stream m s f ct a) = Request
    gEveryBodyParam _ req =
        req { requestMethod = reflectMethod (Proxy @m) }

    type MkHeaderRequest (Stream m s f ct a) = Header "Accept" ct -> Request
    gEveryHeader _ req (Header h) =
        req { requestMethod = reflectMethod $ Proxy @m
            , requestHeaders = requestHeaders req ++ [ (hAccept, h) ]
            }
instance
    ( ReflectMethod m
    ) => GEveryEndpoints (NoContentVerb (m :: StdMethod))
  where
    gEveryEndpoint _ =
        [defaultRequest { requestMethod = reflectMethod (Proxy @m) }]

    type MkPathRequest (NoContentVerb m) = Request
    gEveryPathParam _ req =
        req { requestMethod = reflectMethod (Proxy @m) }

    type MkBodyRequest (NoContentVerb m) = Request
    gEveryBodyParam _ req =
        req { requestMethod = reflectMethod (Proxy @m) }

    type MkHeaderRequest (NoContentVerb m) = Request
    gEveryHeader _ req =
        req { requestMethod = reflectMethod (Proxy @m) }

instance
    ( Wellformed (PathParam t)
    , GEveryEndpoints sub
    ) => GEveryEndpoints (Capture p t :> sub)
  where
    gEveryEndpoint _ =
        addPathFragment t <$> gEveryEndpoint (Proxy @sub)
      where
        t = wellformed :: PathParam t

    type MkPathRequest (Capture p t :> sub) = PathParam t -> MkPathRequest sub
    gEveryPathParam _ req t =
        gEveryPathParam (Proxy @sub) (addPathFragment t req)

    type MkBodyRequest (Capture p t :> sub) = MkBodyRequest sub
    gEveryBodyParam _ req =
        gEveryBodyParam (Proxy @sub) (addPathFragment t req)
      where
        t = wellformed :: PathParam t

    type MkHeaderRequest (Capture p t :> sub) = MkHeaderRequest sub
    gEveryHeader _ req =
        gEveryHeader (Proxy @sub) (addPathFragment t req)
      where
        t = wellformed :: PathParam t

instance
    ( KnownSymbol s
    , GEveryEndpoints sub
    ) => GEveryEndpoints (s :> sub)
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

instance
    ( GEveryEndpoints sub
    ) => GEveryEndpoints (ReqBody '[ct] a :> sub)
  where
    gEveryEndpoint _ =
        gEveryEndpoint (Proxy @sub)

    type MkPathRequest (ReqBody '[ct] a :> sub) = MkPathRequest sub
    gEveryPathParam _ =
        gEveryPathParam (Proxy @sub)

    type MkBodyRequest (ReqBody '[ct] a :> sub) = BodyParam a -> IO (MkBodyRequest sub)
    gEveryBodyParam _ req b =
        gEveryBodyParam (Proxy @sub) <$> (setRequestBody b req)

    type MkHeaderRequest (ReqBody '[ct] a :> sub) = Header "Content-Type" ct -> MkHeaderRequest sub
    gEveryHeader _ req (Header h) =
        gEveryHeader (Proxy @sub) $ req
            { requestHeaders = requestHeaders req ++ [(hContentType, h)] }

instance
    ( GEveryEndpoints sub
    ) => GEveryEndpoints (Servant.QueryParam a b :> sub)
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
    ( GEveryEndpoints sub
    ) => GEveryEndpoints (Servant.QueryFlag s :> sub)
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

addPathFragment :: PathParam t -> Request -> Request
addPathFragment (PathParam fragment) req = req
    { pathInfo = pathInfo req ++ [fragment] }

setRequestBody :: BodyParam b -> Request -> IO Request
setRequestBody (BodyParam bytes) req = do
    ref <- newIORef $ BL.toChunks bytes
    pure req
        { requestBodyLength = KnownLength $ fromIntegral $ BL.length bytes
        , requestBody = atomicModifyIORef ref $ \case
            []  -> ([], mempty)
            h:q -> (q, h)
        }

titleize
    :: forall t. (Typeable t)
    => Proxy t
    -> Request
    -> String
titleize proxy req = unwords
    [ if proxyStr == "(Void)" then "" else proxyStr
    , B8.unpack (requestMethod req)
    , "/" <> T.unpack (T.intercalate "/" $ pathInfo req)
    ]
  where
    proxyStr = "(" <> show (typeRep proxy) <> ")"
