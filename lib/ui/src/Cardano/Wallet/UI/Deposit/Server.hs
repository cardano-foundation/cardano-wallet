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
    ( sse
    )
import Cardano.Wallet.UI.Common.Handlers.State
    ( getState
    )
import Cardano.Wallet.UI.Common.Html.Html
    ( RawHtml (..)
    , renderHtml
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( alertH
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
    ( sessioning
    )
import Cardano.Wallet.UI.Deposit.API
    ( UI
    , settingsSseToggleLink
    )
import Cardano.Wallet.UI.Deposit.Handlers.Addresses
    ( getAddresses
    , getCustomerAddress
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( walletPresence
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Addresses
    ( addressElementH
    , customerAddressH
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Page
    ( Page (..)
    , headerElementH
    , page
    )
import Cardano.Wallet.UI.Deposit.Server.Lib
    ( showTime
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
import Lucid
    ( class_
    , div_
    )
import Paths_cardano_wallet_ui
    ( getDataFileName
    )
import Servant
    ( Handler
    , Server
    , (:<|>) (..)
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
serveUI tr ul env dbDir config _ nl bs =
    ph Wallet
        :<|> ph About
        :<|> ph Network
        :<|> ph Settings
        :<|> ph Wallet
        :<|> ph Addresses
        :<|> sessioning
            ( renderSmoothHtml . networkInfoH showTime
                <$> getNetworkInformation nid nl mode
            )
        :<|> wsl
            ( \l ->
                getState l (renderSmoothHtml . settingsStateH settingsSseToggleLink)
            )
        :<|> wsl (\l -> toggleSSE l $> RawHtml "")
        :<|> withSessionLayerRead ul (sse . sseConfig)
        :<|> serveFavicon
        :<|> serveMnemonic
        :<|> serveWalletPage ul
        :<|> servePostMnemonicWallet tr env dbDir ul
        :<|> servePostXPubWallet tr env dbDir ul
        :<|> serveDeleteWallet ul dbDir
        :<|> serveDeleteWalletModal ul
        :<|> ( \c ->
                wsl
                    ( \l -> getCustomerAddress l (renderSmoothHtml . customerAddressH) alert c
                    )
             )
        :<|> wsl (\l -> getAddresses l (renderSmoothHtml . addressElementH alertH))
        :<|> serveNavigation
  where
    serveNavigation mp = wsl $ \l -> do
        wp <- walletPresence l

        pure $ renderSmoothHtml $ headerElementH mp wp
    ph p = wsl $ \_ -> pure $ page config p
    alert = renderHtml . alertH
    nid = networkIdVal (sNetworkId @n)
    mode = case bs of
        NodeSource{} -> Node
    _ = networkInfoH
    wsl f = withSessionLayer ul $ \l -> f l
    renderSmoothHtml response = renderHtml $ div_ [class_ "smooth"] response

serveFavicon :: Handler BL.ByteString
serveFavicon = do
    file <- liftIO $ getDataFileName "data/images/icon.png"
    liftIO $ BL.readFile file
