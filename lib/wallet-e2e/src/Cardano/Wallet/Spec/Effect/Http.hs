{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.Spec.Effect.Http where

import qualified Network.HTTP.Simple as HS

import Cardano.Wallet.Spec.Effect.Trace
    ( FxTrace, trace )
import Effectful
    ( (:>), Eff, Effect, IOE )
import Effectful.Dispatch.Dynamic
    ( interpret )
import Effectful.TH
    ( makeEffect )
import Prelude hiding
    ( evalState, get, gets, modify, trace )
import Wallet
    ( MonadHTTP )
import Wallet.Common
    ( MonadHTTP (..) )

data FxHttp :: Effect where
    HttpQuery :: HS.Request -> FxHttp m (HS.Response ByteString)

$(makeEffect ''FxHttp)

instance (FxHttp :> es) => MonadHTTP (Eff es) where
    httpBS = httpQuery

runHttpClient :: (FxTrace :> es, IOE :> es) => Eff (FxHttp : es) a -> Eff es a
runHttpClient = interpret \_ -> \case
    HttpQuery req -> do
        trace $ "HTTP request: " <> show req
        resp <- HS.httpBS req
        trace $ "HTTP response: " <> show resp
        pure resp
