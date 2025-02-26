{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.UI.Common.Handlers.State
    ( getState
    ) where

import Prelude

import Cardano.Wallet.UI.Common.Html.Html
    ( RawHtml (..)
    )
import Cardano.Wallet.UI.Common.Layer
    ( SessionLayer (..)
    , State
    )
import Control.Monad.Trans
    ( MonadIO (..)
    )
import Servant
    ( Handler
    )

-- | Get the current state and render it using the provided function.
getState
    :: SessionLayer s
    -> (State s -> RawHtml)
    -> Handler RawHtml
getState uiLayer render = fmap render . liftIO $ state uiLayer
