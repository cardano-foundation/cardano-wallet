{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.Spec.Effect.Http where

import qualified Effectful.Error.Static as Fx
import qualified Network.HTTP.Simple as HS

import Cardano.Wallet.Spec.Effect.Trace
    ( FxTrace, trace )
import Control.Exception
    ( try )
import Effectful
    ( (:>), Eff, Effect, IOE )
import Effectful.Dispatch.Dynamic
    ( interpret )
import Effectful.Error.Static
    ( throwError )
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

runHttpClient
    :: (FxTrace :> es, Fx.Error SomeException :> es, IOE :> es)
    => Eff (FxHttp : es) a
    -> Eff es a
runHttpClient = interpret \_ -> \case
    HttpQuery req -> do
        trace $ "HTTP request: " <> show req
        resp <- liftIO $ try $ HS.httpBS req
        trace $ "HTTP response: " <> show resp
        case resp of
            Left (e :: SomeException) -> throwError e
            Right r -> pure r
