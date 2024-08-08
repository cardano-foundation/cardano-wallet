{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.UI.Handlers.State
    ( getState
    ) where

import Prelude

import Cardano.Wallet.UI.Html.Html
    ( RawHtml (..)
    )
import Cardano.Wallet.UI.Layer
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
