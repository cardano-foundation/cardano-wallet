{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.UI.Handlers.Settings where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( WalletId
    )
import Cardano.Wallet.UI.Html.Html
    ( RawHtml (..)
    )
import Cardano.Wallet.UI.Layer
    ( Push (..)
    , SessionLayer (..)
    , State
    , refreshEnabled
    , walletId
    )
import Control.Lens
    ( over
    , set
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

toggleSSE :: SessionLayer -> Handler ()
toggleSSE SessionLayer{..} = liftIO $ do
    update $ over refreshEnabled not
    sendSSE $ Sync "settings"

selectWallet :: SessionLayer -> WalletId -> Handler ()
selectWallet SessionLayer{..} wid = liftIO $ do
    update $ set walletId $ Just wid
    sendSSE $ Sync "wallet"
    sendSSE $ Sync "wallets"
    sendSSE $ Sync "settings"
