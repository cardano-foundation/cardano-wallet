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
    , deleteWallet
    , initXPubWallet
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
import Cardano.Wallet.UI.Common.Handlers.Wallet
    ( pickMnemonic
    )
import Cardano.Wallet.UI.Common.Html.Html
    ( RawHtml (..)
    , renderHtml
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( alertH
    , rogerH
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
import Cardano.Wallet.UI.Common.Html.Pages.Wallet
    ( mnemonicH
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
import Cardano.Wallet.UI.Deposit.Handlers.Wallet
    ( deleteWalletHandler
    , getCustomerAddress
    , getWallet
    , postMnemonicWallet
    , postXPubWallet
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Page
    ( Page (..)
    , page
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Wallet
    ( customerAddressH
    , deleteWalletModalH
    , walletElementH
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
import Data.Text
    ( Text
    )
import Data.Time
    ( UTCTime
    , defaultTimeLocale
    , formatTime
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

showTime :: UTCTime -> String
showTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

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
        :<|> sessioning (renderSmoothHtml . networkInfoH showTime <$> getNetworkInformation nid nl mode)
        :<|> wsl (\l -> getState l (renderSmoothHtml . settingsStateH settingsSseToggleLink))
        :<|> wsl (\l -> toggleSSE l $> RawHtml "")
        :<|> withSessionLayerRead ul (sse . sseConfig)
        :<|> serveFavicon
        :<|> (\c -> sessioning $ renderSmoothHtml . mnemonicH <$> liftIO (pickMnemonic 15 c))
        :<|> wsl (\l -> getWallet l (renderSmoothHtml . walletElementH alertH))
        :<|> (\v -> wsl (\l -> postMnemonicWallet l initWallet alert ok v))
        :<|> (\v -> wsl (\l -> postXPubWallet l initWallet alert ok v))
        :<|> wsl (\l -> deleteWalletHandler l (deleteWallet dbDir) alert ok)
        :<|> wsl (\_l -> pure $ renderSmoothHtml deleteWalletModalH)
        :<|> (\c -> wsl (\l -> getCustomerAddress l (renderSmoothHtml . customerAddressH) alert c))
  where
    ph p = wsl $ \_ -> pure $ page config p
    ok _ = renderHtml . rogerH @Text $ "ok"
    alert = renderHtml . alertH
    nid = networkIdVal (sNetworkId @n)
    mode = case bs of
        NodeSource{} -> Node
    _ = networkInfoH
    wsl f = withSessionLayer ul $ \l -> f l
    initWallet = initXPubWallet tr env dbDir
    renderSmoothHtml response = renderHtml $ div_ [class_ "smooth"] response

serveFavicon :: Handler BL.ByteString
serveFavicon = do
    file <- liftIO $ getDataFileName "data/images/icon.png"
    liftIO $ BL.readFile file
