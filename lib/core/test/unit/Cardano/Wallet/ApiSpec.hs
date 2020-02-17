{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.ApiSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Server
    ( LiftHandler (..) )
import Cardano.Wallet.Api.Types
    ( ApiT, ApiTxId )
import Cardano.Wallet.Primitive.Types
    ( PoolId, WalletId )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.Function
    ( (&) )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Network.Wai
    ( defaultRequest, pathInfo )
import Network.Wai.Middleware.ServerError
    ( handleRawError )
import Network.Wai.Test
    ( assertBody, assertStatus, request, runSession )
import Servant
    ( Application, Server, err501, serve, throwError )
import Servant.API
    ( (:<|>) (..), (:>), Capture )
import Servant.API.Verbs
    ( GetNoContent )
import Test.Hspec
    ( Spec, describe, it )

import qualified Data.Aeson as Aeson
import qualified Data.Text as T

spec :: Spec
spec = describe "Path params capture failures" $ do
    describe "False wallet ids" $ do
        spec_FalseWalletIds $ T.replicate 40 "ś"
        spec_FalseWalletIds $ T.replicate 39 "1"
        spec_FalseWalletIds $ T.replicate 41 "1"

    describe "False transaction ids" $ do
        spec_FalseTxIds $ T.replicate 64 "ś"
        spec_FalseTxIds $ T.replicate 63 "1"
        spec_FalseTxIds $ T.replicate 65 "1"

    describe "False stake pool ids" $ do
        spec_FalseStakePoolIds $ T.replicate 64 "ś"
        spec_FalseStakePoolIds $ T.replicate 63 "1"
        spec_FalseStakePoolIds $ T.replicate 65 "1"

spec_FalseWalletIds :: Text -> Spec
spec_FalseWalletIds wid =
    it (T.unpack wid) $ flip runSession application $ do
        response <- request $ defaultRequest
            { pathInfo = ["testCaptureWalletId", wid] }
        response & assertStatus 400
        response & assertBody (Aeson.encode [aesonQQ|
            { "code": "bad_request"
            , "message": "wallet id should be a hex-encoded string of 40 characters"
            }|])

spec_FalseTxIds :: Text -> Spec
spec_FalseTxIds tid =
    it (T.unpack tid) $ flip runSession application $ do
        response <- request $ defaultRequest
            { pathInfo = ["testCaptureTxId", tid] }
        response & assertStatus 400
        response & assertBody (Aeson.encode [aesonQQ|
            { "code": "bad_request"
            , "message": "Invalid tx hash: expecting a hex-encoded value that is 32 bytes in length."
            }|])

spec_FalseStakePoolIds :: Text -> Spec
spec_FalseStakePoolIds tid =
    it (T.unpack tid) $ flip runSession application $ do
        response <- request $ defaultRequest
            { pathInfo = ["testCaptureStakePoolId", tid] }
        response & assertStatus 400
        response & assertBody (Aeson.encode [aesonQQ|
            { "code": "bad_request"
            , "message": "Invalid stake pool id: expecting a hex-encoded value that is 32 bytes in length."
            }|])


--
-- Test API
--

application :: Application
application = serve (Proxy @Api) server
    & handleRawError (curry handler)

type Api = TestCaptureWalletId
      :<|> TestCaptureTxId
      :<|> TestCaptureStakePoolId

type TestCaptureWalletId = "testCaptureWalletId"
    :> Capture "walletId" (ApiT WalletId)
    :> GetNoContent

type TestCaptureTxId = "testCaptureTxId"
    :> Capture "txId" ApiTxId
    :> GetNoContent

type TestCaptureStakePoolId = "testCaptureStakePoolId"
    :> Capture "poolId" (ApiT PoolId)
    :> GetNoContent

server :: Server Api
server = const (throwError err501)
    :<|> const (throwError err501)
    :<|> const (throwError err501)
