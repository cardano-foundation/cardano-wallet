{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Deposit.Server
    ( serveUI
    ) where

import Prelude

import Cardano.Wallet.Api.Http.Server.Handlers.NetworkInformation
    ( getNetworkInformation
    )
import Cardano.Wallet.Api.Types
    ( ApiWalletMode (..)
    )
import Cardano.Wallet.Deposit.IO
    ( WalletBootEnv
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResource
    )
import Cardano.Wallet.Network
    ( NetworkLayer
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId (..)
    , SNetworkId
    , networkIdVal
    )
import Cardano.Wallet.Shelley.BlockchainSource
    ( BlockchainSource (..)
    )
import Cardano.Wallet.UI.Common.Handlers.Session
    ( withSessionLayer
    , withSessionLayerRead
    )
import Cardano.Wallet.UI.Common.Handlers.Settings
    ( toggleSSE
    )
import Cardano.Wallet.UI.Common.Handlers.SSE
    ( Message
    , sse
    )
import Cardano.Wallet.UI.Common.Handlers.State
    ( getState
    )
import Cardano.Wallet.UI.Common.Html.Html
    ( RawHtml (..)
    )
import Cardano.Wallet.UI.Common.Html.Pages.Network
    ( networkInfoH
    )
import Cardano.Wallet.UI.Common.Html.Pages.Settings
    ( settingsStateH
    )
import Cardano.Wallet.UI.Common.Html.Pages.Template.Head
    ( PageConfig
    )
import Cardano.Wallet.UI.Common.Layer
    ( SessionLayer (..)
    , UILayer (..)
    )
import Cardano.Wallet.UI.Cookies
    ( CookieResponse
    , RequestCookies
    , sessioning
    )
import Cardano.Wallet.UI.Deposit.API
    ( UI
    , settingsSseToggleLink
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( walletPresence
    )
import Cardano.Wallet.UI.Deposit.Html.Common
    ( showTimeSecs
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Page
    ( Page (..)
    , headerElementH
    , page
    )
import Cardano.Wallet.UI.Deposit.Server.Addresses
    ( serveAddressesPage
    , serveCustomerHistory
    , serveGetAddress
    )
import Cardano.Wallet.UI.Deposit.Server.Lib
    ( renderSmoothHtml
    )
import Cardano.Wallet.UI.Deposit.Server.Wallet
    ( serveDeleteWallet
    , serveDeleteWalletModal
    , serveMnemonic
    , servePostMnemonicWallet
    , servePostXPubWallet
    , serveWalletPage
    )
import Control.Monad.Trans
    ( MonadIO (..)
    )
import Control.Tracer
    ( Tracer (..)
    )
import Data.Functor
    ( ($>)
    )
import Paths_cardano_wallet_ui
    ( getDataFileName
    )
import Servant
    ( Handler
    , Server
    , (:<|>) (..)
    )
import Servant.Types.SourceT
    ( SourceT
    )

import qualified Cardano.Read.Ledger.Block.Block as Read
import qualified Data.ByteString.Lazy as BL

serveUI
    :: forall n
     . HasSNetworkId n
    => Tracer IO String
    -> UILayer WalletResource
    -> WalletBootEnv IO
    -> FilePath
    -> PageConfig
    -> SNetworkId n
    -> NetworkLayer IO Read.ConsensusBlock
    -> BlockchainSource
    -> Server UI
serveUI tr ul env dbDir config nid nl bs =
    serveTabPage ul config Wallet
        :<|> serveTabPage ul config About
        :<|> serveTabPage ul config Network
        :<|> serveTabPage ul config Settings
        :<|> serveTabPage ul config Wallet
        :<|> serveTabPage ul config Addresses
        :<|> serveNetworkInformation nid nl bs
        :<|> serveSSESettings ul
        :<|> serveToggleSSE ul
        :<|> serveSSE ul
        :<|> serveFavicon
        :<|> serveFakeDataBackground
        :<|> serveMnemonic
        :<|> serveWalletPage ul
        :<|> servePostMnemonicWallet tr env dbDir ul
        :<|> servePostXPubWallet tr env dbDir ul
        :<|> serveDeleteWallet ul dbDir
        :<|> serveDeleteWalletModal ul
        :<|> serveGetAddress ul
        :<|> serveAddressesPage ul
        :<|> serveNavigation ul
        :<|> serveCustomerHistory ul

serveTabPage
    :: UILayer s
    -> PageConfig
    -> Page
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveTabPage ul config p = withSessionLayer ul $ \_ -> pure $ page config p

serveNavigation
    :: UILayer WalletResource
    -> Maybe Page
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveNavigation ul mp = withSessionLayer ul $ \l -> do
    wp <- walletPresence l
    pure $ renderSmoothHtml $ headerElementH mp wp

serveFakeDataBackground :: Handler BL.ByteString
serveFakeDataBackground = do
    file <- liftIO $ getDataFileName "data/images/fake-data.png"
    liftIO $ BL.readFile file

serveFavicon :: Handler BL.ByteString
serveFavicon = do
    file <- liftIO $ getDataFileName "data/images/icon.png"
    liftIO $ BL.readFile file

serveNetworkInformation
    :: forall n
     . HasSNetworkId n
    => SNetworkId n
    -> NetworkLayer IO Read.ConsensusBlock
    -> BlockchainSource
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveNetworkInformation _ nl bs =
    sessioning
        $ renderSmoothHtml . networkInfoH showTimeSecs
            <$> getNetworkInformation nid nl mode
  where
    nid = networkIdVal (sNetworkId @n)
    mode = case bs of
        NodeSource{} -> Node
    _ = networkInfoH

serveSSESettings
    :: UILayer WalletResource
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveSSESettings ul = withSessionLayer ul $ \l -> do
    getState l (renderSmoothHtml . settingsStateH settingsSseToggleLink)

serveToggleSSE
    :: UILayer WalletResource
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveToggleSSE ul = withSessionLayer ul $ \l -> do
    toggleSSE l $> RawHtml ""

serveSSE
    :: UILayer s
    -> Maybe RequestCookies
    -> Handler (SourceT IO Message)
serveSSE ul = withSessionLayerRead ul (sse . sseConfig)
