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

module Cardano.Wallet.ApiSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api
    ( Api )
import Cardano.Wallet.Api.Server
    ( LiftHandler (..) )
import Cardano.Wallet.Api.Types
    ( AllowedMnemonics
    , ApiAddress
    , ApiByronWallet
    , ApiByronWalletMigrationInfo
    , ApiCoinSelection
    , ApiEpochNumber
    , ApiFee
    , ApiNetworkInformation
    , ApiNetworkParameters
    , ApiNetworkTip
    , ApiSelectCoinsData
    , ApiStakePool
    , ApiT
    , ApiTransaction
    , ApiTxId
    , ApiUtxoStatistics
    , ApiWallet
    , ApiWalletPassphrase
    , BackwardCompatPlaceholder (..)
    , ByronWalletPostData
    , ByronWalletStyle (..)
    , Iso8601Time
    , PostExternalTransactionData
    , PostTransactionData
    , PostTransactionFeeData
    , StyleSymbol
    , WalletPostData
    , WalletPutData
    , WalletPutPassphraseData
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( PoolId, WalletId )
import Control.Arrow
    ( first )
import Control.Monad
    ( forM, forM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.ByteString.Lazy
    ( ByteString )
import Data.Function
    ( (&) )
import Data.IORef
    ( IORef, atomicModifyIORef, modifyIORef, newIORef )
import Data.Kind
    ( Type )
import Data.List
    ( (\\) )
import Data.Map.Strict
    ( Map )
import Data.Proxy
    ( Proxy (..) )
import Data.String
    ( IsString )
import Data.Text
    ( Text )
import Data.Typeable
    ( Typeable, typeRep )
import Data.Void
    ( Void )
import Data.Word.Odd
    ( Word31 )
import GHC.TypeLits
    ( ErrorMessage (..), KnownSymbol, Nat, Symbol, TypeError, symbolVal )
import Network.HTTP.Types.Header
    ( hAccept, hContentLength, hContentType )
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
    ( Application
    , Header'
    , ReqBody
    , Server
    , StdMethod (..)
    , Verb
    , err501
    , serve
    , throwError
    )
import Servant.API
    ( (:<|>) (..), (:>), Capture )
import Servant.API.Verbs
    ( DeleteNoContent, GetNoContent, NoContentVerb, ReflectMethod (..), Verb )
import Servant.Links
    ( URI (..), allLinks', linkURI )
import System.IO.Unsafe
    ( unsafePerformIO )
import Test.Hspec
    ( Spec, SpecWith, describe, it, runIO, shouldBe, xdescribe, xit )
import Web.HttpApiData
    ( ToHttpApiData (..) )

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Servant


spec :: Spec
spec = describe "PATATE" $ do
    gSpec (everyPathParams api) spec_MalformedParam
    gSpec (everyBodyParams api) spec_MalformedParam
    gSpec (everyAllowedMethods api) spec_NotAllowedMethod

spec_MalformedParam :: Request -> ExpectedError -> Session ()
spec_MalformedParam malformedRequest (ExpectedError msg) = do
    response <- request malformedRequest
    response & assertStatus 400
    response & assertBody (Aeson.encode [aesonQQ|
        { "code": "bad_request"
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
-- Construction of well-formed and malformed parameters
--

newtype ExpectedError = ExpectedError String
    deriving newtype (IsString)

class WellFormed t where
    wellformed :: t

class Malformed t where
    malformed :: [(t, ExpectedError)]
    malformed = []

instance WellFormed (PathParam (ApiT WalletId)) where
    wellformed = PathParam $ T.replicate 40 "0"

instance Malformed (PathParam (ApiT WalletId)) where
    malformed = first PathParam <$>
        [ (T.replicate 40 "ś", msg)
        , (T.replicate 39 "1", msg)
        , (T.replicate 41 "1", msg)
        ]
      where
        msg = "wallet id should be a hex-encoded string of 40 characters"

instance WellFormed (PathParam ApiTxId) where
    wellformed = PathParam $ T.replicate 64 "0"

instance Malformed (PathParam ApiTxId) where
    malformed = first PathParam <$>
        [ (T.replicate 64 "ś", msg)
        , (T.replicate 63 "1", msg)
        , (T.replicate 65 "1", msg)
        ]
      where
        msg = "Invalid tx hash: expecting a hex-encoded value that is 32 bytes in length."

instance WellFormed (PathParam (ApiT PoolId)) where
    wellformed = PathParam $ T.replicate 64 "0"

instance Malformed (PathParam (ApiT PoolId)) where
    malformed = first PathParam <$>
        [ (T.replicate 64 "ś", msg)
        , (T.replicate 63 "1", msg)
        , (T.replicate 65 "1", msg)
        ]
      where
        msg = "Invalid stake pool id: expecting a hex-encoded value that is 32 bytes in length."

instance WellFormed (PathParam (BackwardCompatPlaceholder (ApiT PoolId))) where
    wellformed = PathParam $ T.replicate 64 "0"

instance Malformed (PathParam (BackwardCompatPlaceholder (ApiT PoolId))) where
    malformed = first PathParam <$>
        [ (T.replicate 64 "ś", msg)
        , (T.replicate 63 "1", msg)
        , (T.replicate 65 "1", msg)
        ]
      where
        msg = "Invalid stake pool id: expecting a hex-encoded value that is 32 bytes in length."

instance WellFormed (PathParam ApiEpochNumber) where
    wellformed = PathParam "latest"

instance Malformed (PathParam ApiEpochNumber) where
    malformed = first PathParam <$>
        [ ("earliest", msg)
        , (T.pack $ show $ (+1) $ fromIntegral @Word31 @Int maxBound, msg)
        , ("invalid", msg)
        , ("-1", msg)
        ]
      where
        msg = "I couldn't parse the given epoch number. I am expecting either the word 'latest' or, an integer from 0 to 2147483647."

instance Malformed (BodyParam WalletPostData) where
    malformed = first (BodyParam . Aeson.encode) <$>
        [ ( [aesonQQ|
            { "name": "ads"
            , "mnemonic_sentence": "album execute kingdom dumb trip all salute busy case bring spell ugly umbrella choice shy"
            , "passphrase": "Secure Passphrase"
            } |]
          , "Error in $['mnemonic_sentence']: parsing [] failed, expected Array, but encountered String"
          )
        , ( [aesonQQ|
            { "name": "ads"
            , "mnemonic_sentence": 15
            , "passphrase": "Secure Passphrase"
            } |]
          , "Error in $['mnemonic_sentence']: parsing [] failed, expected Array, but encountered Number"
          )
        , ( [aesonQQ|
            { "name": "ads"
            , "passphrase": "Secure Passphrase"
            } |]
          , "Error in $: parsing Cardano.Wallet.Api.Types.WalletPostData(WalletPostData) failed, key 'mnemonic_sentence' not found"
          )
        , ( [aesonQQ|
            { "name": "Just a łallet"
            , "mnemonic_sentence": #{mnemonics15}
            , "mnemonic_second_factor": []
            , "passphrase": "Secure Passphrase"
            } |]
          , "Error in $['mnemonic_second_factor']: Invalid number of words: 9 or 12 words are expected."
          )
        , ( [aesonQQ|
            { "name": "Just a łallet"
            , "mnemonic_sentence": #{mnemonics15}
            , "mnemonic_second_factor": ["squirrel", "material", "silly", "twice", "direct", "slush", "pistol", "razor", "become"]
            , "passphrase": "Secure Passphrase"
            } |]
          , "Error in $['mnemonic_second_factor']: Invalid entropy checksum: please double-check the last word of your mnemonic sentence."
          )
        -- TODO
        -- and so forth...
        -- We should move all existing "malformed" scenarios from the
        -- integration level to here.
        ]
      where
        mnemonics15 :: [Text]
        mnemonics15 =
            [ "network", "empty", "cause", "mean", "expire"
            , "private", "finger", "accident", "session", "problem"
            , "absurd", "banner", "stage", "void", "what"
            ]

instance Malformed (BodyParam (ApiSelectCoinsData 'Testnet))

instance Malformed (BodyParam (PostTransactionData 'Testnet))

instance Malformed (BodyParam (PostTransactionFeeData 'Testnet))

instance Malformed (BodyParam (ByronWalletPostData (mw :: [Nat])))

instance Malformed (BodyParam WalletPutData)

instance Malformed (BodyParam WalletPutPassphraseData)

instance Malformed (BodyParam ApiNetworkTip)

instance Malformed (BodyParam ApiWalletPassphrase)

instance Malformed (BodyParam PostExternalTransactionData)

--
-- Generic API Spec
--

class Typeable api => GenericApiSpec api where
    gSpec :: api -> (Request -> ExpectedError -> Session ()) -> Spec
    gSpec _ _ = xdescribe (show $ typeRep $ Proxy @api) (pure ())

instance (GenericApiSpec a, GenericApiSpec b) => GenericApiSpec (a :<|> b) where
    gSpec (a :<|> b) toS = gSpec a toS >> gSpec b toS

instance GenericApiSpec Request where
    gSpec req _ = pure ()

instance
    ( Typeable a, Malformed (PathParam a)
    ) => GenericApiSpec (PathParam a -> Request)
  where
    gSpec toRequest toSession = describe "Malformed PathParam" $ do
        forM_ (first toRequest <$> malformed) $ \(req, msg) ->
            it (titleize (Proxy @a) req) $
                runSession (toSession req msg) application

instance
    ( Typeable a, Malformed (PathParam a), WellFormed (PathParam a)
    , GenericApiSpec (PathParam a -> Request)
    , Typeable b, Malformed (PathParam b), WellFormed (PathParam b)
    , GenericApiSpec (PathParam b -> Request)
    ) => GenericApiSpec (PathParam a -> PathParam b -> Request)
  where
    gSpec toRequest toSession = do
        gSpec (toRequest wellformed) toSession
        gSpec (`toRequest` wellformed) toSession

instance
    ( Typeable a, Malformed (BodyParam a)
    ) => GenericApiSpec (BodyParam a -> IO Request)
  where
    gSpec toRequest toSession = describe "Malformed BodyParam" $
        forM_ (first toRequest <$> malformed) $ \(newReq, msg) -> do
            req <- runIO newReq
            it (titleize (Proxy @a) req) $
                runSession (toSession req msg) application

instance GenericApiSpec (Map [Text] [Method])
  where
    gSpec allowedMethods toSession = describe "Not Allowed Methods" $
        forM_ (Map.toList allowedMethods) $ \(pathInfo, methods) ->
            forM_ (allMethods \\ methods) $ \requestMethod -> do
                let run = if pathInfo `elem` whiteList then xit else it
                let req = defaultRequest { pathInfo, requestMethod }
                run (titleize (Proxy @Void) req) $
                    runSession (toSession req msg) application
      where
        msg =
            "You've reached a known endpoint but I don't know how to handle the \
            \HTTP method specified. Please double-check both the endpoint and \
            \the method: one of them is likely to be incorrect (for example: \
            \POST instead of PUT, or GET instead of POST...)."

        allMethods :: [Method]
        allMethods =
            ["GET","PUT","POST","PATCH","DELETE","CONNECT","TRACE","OPTIONS"]

        -- NOTE
        -- These particular endpoints conflicts with some others that are taking
        -- a resource id as a parameter (e.g. GET /byron-wallets/{walletId}).
        --
        -- It would be best to either, have the server handling these correctly,
        -- or, revise the endpoint altogether.
        whiteList :: [[Text]]
        whiteList =
            [ [ "byron-wallets", "icarus" ]
            , [ "byron-wallets", "trezor" ]
            , [ "byron-wallets", "ledger" ]
            , [ "byron-wallets", "random" ]
            , [ "wallets", wid, "transactions", "fees" ]
            ]
          where
            PathParam wid = (wellformed :: PathParam (ApiT WalletId))

--
-- Construct test cases from the API
--

application :: Application
application = serve api server
    & handleRawError (curry handler)

api :: Proxy (Api Testnet)
api = Proxy @(Api Testnet)

server :: Server (Api Testnet)
server = error
    "No test from this module should actually reach handlers of the server. \
    \Tests are indeed all testing the internal machinery of Servant + Wai and \
    \the way they interact with the outside world. Only valid requests are \
    \delegated to our handlers."

newtype PathParam t = PathParam Text
    deriving (Typeable)

newtype BodyParam t = BodyParam ByteString
    deriving (Typeable)

everyPathParams :: GEveryParams api => Proxy api -> MkPathRequest api
everyPathParams proxy = gEveryPathParams proxy defaultRequest

everyBodyParams :: GEveryParams api => Proxy api -> MkBodyRequest api
everyBodyParams proxy = gEveryBodyParams proxy $ defaultRequest
    { requestHeaders =
        [ (hContentType, "application/json")
        , (hAccept, "application/json")
        ]
    }

everyAllowedMethods :: GEveryParams api => Proxy api -> Map [Text] [Method]
everyAllowedMethods proxy =
    Map.fromListWith (++) (toTuple <$> gEveryAllowedMethods proxy)
  where
    toTuple :: Request -> ([Text], [Method])
    toTuple req = (reverse (pathInfo req), [requestMethod req])

class GEveryParams api where
    type MkPathRequest api :: *
    gEveryPathParams :: Proxy api -> Request -> MkPathRequest api

    type MkBodyRequest api :: *
    gEveryBodyParams :: Proxy api -> Request -> MkBodyRequest api

    gEveryAllowedMethods :: Proxy api -> [Request]

    -- TODO
    -- Capture request query params as QueryParam
    --
    -- newtype QueryParam t = QueryParam Text
    --     deriving (Typeable)
    --
    -- type MkQueryRequest api :: *
    -- gEveryQueryParams :: Proxy api -> Request -> MkQueryRequest api

instance
    ( GEveryParams a
    , GEveryParams b
    ) => GEveryParams (a :<|> b)
  where
    type MkPathRequest (a :<|> b) = MkPathRequest a :<|> MkPathRequest b
    gEveryPathParams _ req = do
        gEveryPathParams (Proxy @a) req
      :<|>
        gEveryPathParams (Proxy @b) req

    type MkBodyRequest (a :<|> b) = MkBodyRequest a :<|> MkBodyRequest b
    gEveryBodyParams _ req = do
        gEveryBodyParams (Proxy @a) req
      :<|>
        gEveryBodyParams (Proxy @b) req

    gEveryAllowedMethods _ =
        gEveryAllowedMethods (Proxy @a)
      ++
        gEveryAllowedMethods (Proxy @b)

instance
    ( ReflectMethod m
    ) => GEveryParams (Verb (m :: StdMethod) s ct a)
  where
    type MkPathRequest (Verb m s ct a) = Request
    gEveryPathParams _ req =
        req { requestMethod = reflectMethod (Proxy @m) }

    type MkBodyRequest (Verb m s ct a) = Request
    gEveryBodyParams _ req =
        req { requestMethod = reflectMethod (Proxy @m) }

    gEveryAllowedMethods _ =
        [defaultRequest { requestMethod = reflectMethod (Proxy @m) }]

instance
    ( ReflectMethod m
    ) => GEveryParams (NoContentVerb (m :: StdMethod))
  where
    type MkPathRequest (NoContentVerb m) = Request
    gEveryPathParams _ req =
        req { requestMethod = reflectMethod (Proxy @m) }

    type MkBodyRequest (NoContentVerb m) = Request
    gEveryBodyParams _ req =
        req { requestMethod = reflectMethod (Proxy @m) }

    gEveryAllowedMethods _ =
        [defaultRequest { requestMethod = reflectMethod (Proxy @m) }]

instance
    ( WellFormed (PathParam t)
    , GEveryParams sub
    ) => GEveryParams (Capture p t :> sub)
  where
    type MkPathRequest (Capture p t :> sub) = PathParam t -> MkPathRequest sub
    gEveryPathParams _ req t =
        gEveryPathParams (Proxy @sub) (addPathFragment t req)

    type MkBodyRequest (Capture p t :> sub) = MkBodyRequest sub
    gEveryBodyParams _ req =
        gEveryBodyParams (Proxy @sub) (addPathFragment t req)
      where
        t = wellformed :: PathParam t

    gEveryAllowedMethods _ =
        addPathFragment t <$> gEveryAllowedMethods (Proxy @sub)
      where
        t = wellformed :: PathParam t

instance
    ( KnownSymbol s
    , GEveryParams sub
    ) => GEveryParams (s :> sub)
  where
    type MkPathRequest (s :> sub) = MkPathRequest sub
    gEveryPathParams _ req =
        gEveryPathParams (Proxy @sub) (addPathFragment str req)
      where
        str = PathParam $ T.pack $ symbolVal (Proxy @s)

    type MkBodyRequest (s :> sub) = MkBodyRequest sub
    gEveryBodyParams _ req =
        gEveryBodyParams (Proxy @sub) (addPathFragment t req)
      where
        t = PathParam $ T.pack $ symbolVal (Proxy @s)

    gEveryAllowedMethods _ =
        addPathFragment t <$> gEveryAllowedMethods (Proxy @sub)
      where
        t = PathParam $ T.pack $ symbolVal (Proxy @s)


instance
    ( GEveryParams sub
    ) => GEveryParams (ReqBody a b :> sub)
  where
    type MkPathRequest (ReqBody a b :> sub) = MkPathRequest sub
    gEveryPathParams _ =
        gEveryPathParams (Proxy @sub)

    type MkBodyRequest (ReqBody a b :> sub) = BodyParam b -> IO (MkBodyRequest sub)
    gEveryBodyParams _ req b =
        gEveryBodyParams (Proxy @sub) <$> (setRequestBody b req)

    gEveryAllowedMethods _ =
        gEveryAllowedMethods (Proxy @sub)

instance
    ( GEveryParams sub
    ) => GEveryParams (Servant.QueryParam a b :> sub)
  where
    type MkPathRequest (Servant.QueryParam a b :> sub) = MkPathRequest sub
    gEveryPathParams _ =
        gEveryPathParams (Proxy @sub)

    type MkBodyRequest (Servant.QueryParam a b :> sub) = MkBodyRequest sub
    gEveryBodyParams _ =
        gEveryBodyParams (Proxy @sub)

    gEveryAllowedMethods _ =
        gEveryAllowedMethods (Proxy @sub)

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
