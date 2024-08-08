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
    , SNetworkId
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
import Cardano.Wallet.UI.Handlers.Addresses
    ( listAddresses
    )
import Cardano.Wallet.UI.Handlers.Settings
    ( toggleSSE
    )
import Cardano.Wallet.UI.Handlers.SSE
    ( sse
    )
import Cardano.Wallet.UI.Handlers.State
    ( getState
    )
import Cardano.Wallet.UI.Handlers.Wallet
    ( deleteWallet
    , getWallet
    , pickMnemonic
    , postWallet
    , selectWallet
    )
import Cardano.Wallet.UI.Handlers.Wallets
    ( listWallets
    )
import Cardano.Wallet.UI.Html.Html
    ( RawHtml (..)
    , renderHtml
    )
import Cardano.Wallet.UI.Html.Pages.Addresses
    ( addressesH
    )
import Cardano.Wallet.UI.Html.Pages.Lib
    ( alertH
    , rogerH
    )
import Cardano.Wallet.UI.Html.Pages.Network
    ( networkInfoH
    )
import Cardano.Wallet.UI.Html.Pages.Page
    ( Page (..)
    , PageConfig
    , page
    )
import Cardano.Wallet.UI.Html.Pages.Settings
    ( settingsStateH
    )
import Cardano.Wallet.UI.Html.Pages.Wallet
    ( WalletPresent (..)
    , walletElementH
    )
import Cardano.Wallet.UI.Html.Pages.Wallets
    ( walletListH
    )
import Cardano.Wallet.UI.Html.Pages.Wallets.NewWallet
    ( mnemonicH
    , postWalletForm
    )
import Cardano.Wallet.UI.Layer
    ( SessionLayer (..)
    , UILayer (..)
    , walletId
    )
import Control.Lens
    ( view
    )
import Control.Monad.Trans
    ( MonadIO (..)
    )
import Data.Functor
    ( ($>)
    )
import Data.Text
    ( Text
    )
import Data.Time
    ( UTCTime
    , defaultTimeLocale
    , formatTime
    )
import Network.NTP.Client
    ( NtpClient
    )
import Paths_cardano_wallet_ui
    ( getDataFileName
    )
import Servant
    ( Handler
    , Server
    , (:<|>) (..)
    )

import qualified Data.ByteString.Lazy as BL

pageHandler
    :: UILayer
    -> PageConfig
    -> Page
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
pageHandler uiLayer config x =
    withSessionLayer uiLayer $ \session -> do
        state' <- liftIO $ state session
        let walletPresent = case view walletId state' of
                Just _ -> WalletPresent
                Nothing -> WalletAbsent
        pure $ page config x walletPresent

showTime :: UTCTime -> String
showTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

serveUI
    :: forall n
     . HasSNetworkId n
    => UILayer
    -> PageConfig
    -> SNetworkId n
    -> ApiLayer (RndState n)
    -> ApiLayer (SeqState n IcarusKey)
    -> ApiLayer (SeqState n ShelleyKey)
    -> ApiLayer (SharedState n SharedKey)
    -> StakePoolLayer
    -> NtpClient
    -> BlockchainSource
    -> Server UI
serveUI ul config _ alByron _alIcarus alShelley _alShared _spl _ntp bs =
    ph Wallets
        :<|> ph About
        :<|> ph Network
        :<|> ph Wallet
        :<|> ph Wallets
        :<|> ph Addresses
        :<|> ph Settings
        :<|> sessioning (renderHtml . networkInfoH showTime <$> getNetworkInformation nid nl mode)
        :<|> (\v -> wsl (\l -> postWallet l alShelley alert ok v))
        :<|> (\c -> sessioning $ renderHtml . mnemonicH <$> liftIO (pickMnemonic 15 c))
        :<|> sessioning . pure . renderHtml . postWalletForm
        :<|> wsl (\l -> listWallets l alShelley (fmap renderHtml . walletListH))
        :<|> wsl (\l -> getWallet l alShelley alert (renderHtml . walletElementH showTime))
        :<|> wsl (\l -> listAddresses l alShelley alert (renderHtml . addressesH))
        :<|> wsl (\l -> deleteWallet l alShelley alert ok)
        :<|> wsl (\l -> getState l (renderHtml . settingsStateH))
        :<|> wsl (\l -> toggleSSE l $> RawHtml "")
        :<|> (\w -> wsl (\l -> selectWallet l w $> RawHtml ""))
        :<|> withSessionLayerRead (sse . sseConfig)
        :<|> serveFavicon
  where
    ph = pageHandler ul config
    ok _ = renderHtml . rogerH @Text $ "ok"
    alert = renderHtml . alertH
    nl = netLayer alByron
    nid = networkIdVal (sNetworkId @n)
    mode = case bs of
        NodeSource{} -> Node
    _ = networkInfoH
    wsl = withSessionLayer ul
    withSessionLayerRead :: (SessionLayer -> Handler a) -> Maybe RequestCookies -> Handler a
    withSessionLayerRead f = withSessionRead $ \k -> do
        s <- liftIO $ sessions ul k
        f s

withSessionLayer :: UILayer -> (SessionLayer -> Handler a) -> Maybe RequestCookies -> Handler (CookieResponse a)
withSessionLayer ulayer f = withSession $ \k -> do
    s <- liftIO $ sessions ulayer k
    f s

serveFavicon :: Handler BL.ByteString
serveFavicon = do
    file <- liftIO $ getDataFileName "data/images/icon.png"
    liftIO $ BL.readFile file
