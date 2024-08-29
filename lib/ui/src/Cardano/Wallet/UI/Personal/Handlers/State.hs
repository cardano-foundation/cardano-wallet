{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.UI.Personal.Handlers.State
    ( getState
    ) where

import Prelude

import Cardano.Wallet.UI.Personal.Html.Html
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
    :: SessionLayer
    -> (State -> RawHtml)
    -> Handler RawHtml
getState uiLayer render = fmap render . liftIO $ state uiLayer
