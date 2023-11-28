{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.Spec.Effect.Http where

import qualified Network.HTTP.Client as HC

import Cardano.Wallet.Spec.Effect.Trace
    ( FxTrace
    , trace
    )
import Control.Exception
    ( try
    )
import Effectful
    ( Eff
    , Effect
    , IOE
    , (:>)
    )
import Effectful.Dispatch.Dynamic
    ( interpret
    )
import Effectful.Fail
    ( Fail
    )
import Effectful.TH
    ( makeEffect
    )
import Prelude hiding
    ( evalState
    , get
    , gets
    , modify
    , trace
    )
import Wallet
    ( MonadHTTP
    )
import Wallet.Common
    ( MonadHTTP (..)
    )

data FxHttp :: Effect where
    HttpQuery :: HC.Request -> FxHttp m (HC.Response ByteString)

$(makeEffect ''FxHttp)

instance (FxHttp :> es) => MonadHTTP (Eff es) where
    httpBS = httpQuery

runHttpClient
    :: (FxTrace :> es, Fail :> es, IOE :> es)
    => HC.Manager
    -> Eff (FxHttp : es) a
    -> Eff es a
runHttpClient connectionManager = interpret \_ -> \case
    HttpQuery request -> do
        trace $ "HTTP request: " <> show request
        response <- liftIO $ try $ HC.httpLbs request connectionManager
        trace $ "HTTP response: " <> show response
        case response of
            Left (e :: SomeException) -> fail $ displayException e
            Right r -> pure $ fmap toStrict r
