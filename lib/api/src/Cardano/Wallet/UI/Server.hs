{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Server where

import Prelude

import Cardano.Wallet.Address.Derivation.Icarus
    ( IcarusKey
    )
import Cardano.Wallet.Address.Derivation.Shared
    ( SharedKey
    )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey (..)
    )
import Cardano.Wallet.Address.Discovery.Random
    ( RndState
    )
import Cardano.Wallet.Address.Discovery.Sequential
    ( SeqState
    )
import Cardano.Wallet.Address.Discovery.Shared
    ( SharedState
    )
import Cardano.Wallet.Api
    ( ApiLayer
    , netLayer
    )
import Cardano.Wallet.Api.Http.Server.Handlers.NetworkInformation
    ( getNetworkInformation
    )
import Cardano.Wallet.Api.Types
    ( ApiWalletMode (..)
    )
import Cardano.Wallet.Pools
    ( StakePoolLayer
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId (..)
    , networkIdVal
    )
import Cardano.Wallet.Shelley.BlockchainSource
    ( BlockchainSource (..)
    )
import Cardano.Wallet.UI.API
    ( UI
    )
import Cardano.Wallet.UI.Cookies
    ( CookieResponse
    , RequestCookies
    , sessioning
    , withSession
    , withSessionRead
    )
import Cardano.Wallet.UI.Handlers.Settings
    ( getState
    , selectWallet
    , toggleSSE
    )
import Cardano.Wallet.UI.Handlers.SSE
    ( sse
    )
import Cardano.Wallet.UI.Handlers.Wallet
    ( getWallet
    , pickMnemonic
    , postWallet
    )
import Cardano.Wallet.UI.Handlers.Wallets
    ( listWallets
    )
import Cardano.Wallet.UI.Html.Html
    ( RawHtml (..)
    , renderHtml
    )
import Cardano.Wallet.UI.Html.Pages.Lib
    ( alertH
    )
import Cardano.Wallet.UI.Html.Pages.Network
    ( networkInfoH
    )
import Cardano.Wallet.UI.Html.Pages.Page
    ( Page (..)
    , page
    )
import Cardano.Wallet.UI.Html.Pages.Settings
    ( settingsStateH
    )
import Cardano.Wallet.UI.Html.Pages.Wallet
    ( mnemonicH
    , postWalletForm
    , walletElementH
    )
import Cardano.Wallet.UI.Html.Pages.Wallets
    ( walletListH
    )
import Cardano.Wallet.UI.Layer
    ( SessionLayer (..)
    , UILayer (..)
    )
import Control.Monad.Trans
    ( MonadIO (..)
    )
import Data.Functor
    ( ($>)
    )
import Network.NTP.Client
    ( NtpClient
    )
import Servant
    ( Handler
    , Server
    , (:<|>) (..)
    )

serveUI
    :: forall n
     . HasSNetworkId n
    => UILayer
    -> ApiLayer (RndState n)
    -> ApiLayer (SeqState n IcarusKey)
    -> ApiLayer (SeqState n ShelleyKey)
    -> ApiLayer (SharedState n SharedKey)
    -> StakePoolLayer
    -> NtpClient
    -> BlockchainSource
    -> Server UI
serveUI ulayer alByron _alIcarus alShelley _alShared _spl _ntp bs =
    sessioning (pure $ page "" Wallets)
        :<|> sessioning (pure $ page "" About)
        :<|> sessioning (pure $ page "" Network)
        :<|> sessioning (pure $ page "" WalletNew)
        :<|> sessioning (pure $ page "" Wallet)
        :<|> sessioning (pure $ page "" Wallets)
        :<|> sessioning (pure $ page "" Settings)
        :<|> sessioning (renderHtml . networkInfoH <$> getNetworkInformation nid nl mode)
        :<|> (\v -> withSessionLayer (\l -> postWallet l alShelley v))
        :<|> (\c -> sessioning $ renderHtml . mnemonicH <$> liftIO (pickMnemonic 15 c))
        :<|> sessioning . pure . renderHtml . postWalletForm
        :<|> withSessionLayer (\l -> listWallets l alShelley (fmap renderHtml . walletListH))
        :<|> withSessionLayer (\l -> getWallet l alShelley alert (renderHtml . walletElementH))
        :<|> withSessionLayer (\l -> getState l (renderHtml . settingsStateH))
        :<|> withSessionLayer (\l -> toggleSSE l $> RawHtml "")
        :<|> (\w -> withSessionLayer (\l -> selectWallet l w $> RawHtml ""))
        :<|> withSessionLayerRead (sse . sseConfig)
  where
    alert = renderHtml . alertH
    nl = netLayer alByron
    nid = networkIdVal (sNetworkId @n)
    mode = case bs of
        NodeSource{} -> Node
    _ = networkInfoH
    withSessionLayer :: (SessionLayer -> Handler a) -> Maybe RequestCookies -> Handler (CookieResponse a)
    withSessionLayer f = withSession $ \k -> do
        s <- liftIO $ sessions ulayer k
        f s
    withSessionLayerRead :: (SessionLayer -> Handler a) -> Maybe RequestCookies -> Handler a
    withSessionLayerRead f = withSessionRead $ \k -> do
        s <- liftIO $ sessions ulayer k
        f s
