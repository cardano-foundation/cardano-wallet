module Cardano.Wallet.UI.Common.Handlers.Session where

import Prelude

import Cardano.Wallet.UI.Common.Layer
    ( SessionLayer (..)
    , UILayer (..)
    )
import Cardano.Wallet.UI.Cookies
    ( CookieResponse
    , RequestCookies
    , withSession
    , withSessionRead
    )
import Control.Monad.Trans
    ( MonadIO (..)
    )
import Servant
    ( Handler
    )

withSessionLayerRead
    :: UILayer s
    -> (SessionLayer s -> Handler a)
    -> Maybe RequestCookies
    -> Handler a
withSessionLayerRead ul f = withSessionRead $ \k -> do
    s <- liftIO $ sessions ul k
    f s

withSessionLayer
    :: UILayer s
    -> (SessionLayer s -> Handler a)
    -> Maybe RequestCookies
    -> Handler (CookieResponse a)
withSessionLayer ulayer f = withSession $ \k -> do
    s <- liftIO $ sessions ulayer k
    f s
