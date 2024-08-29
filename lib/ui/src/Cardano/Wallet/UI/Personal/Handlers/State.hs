{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.UI.Personal.Handlers.State
    ( getState
    ) where

import Prelude

import Cardano.Wallet.UI.Common.Html.Html
    ( RawHtml (..)
    )
import Cardano.Wallet.UI.Personal.Layer
    ( SessionLayer (..)
    , State
    )
import Control.Monad.Trans
    ( MonadIO (..)
    )
import Servant
    ( Handler
    )

getState
    :: SessionLayer s
    -> (State s -> RawHtml)
    -> Handler RawHtml
getState uiLayer render = fmap render . liftIO $ state uiLayer
