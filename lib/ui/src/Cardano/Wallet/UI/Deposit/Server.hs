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
import Cardano.Wallet.Deposit.IO.Network.Mock
    ( originTime
    )
import Cardano.Wallet.Deposit.IO.Network.Type
    ( NetworkEnv
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
    ( CookieResponse
    , RequestCookies
    , sessioning
    )
import Cardano.Wallet.UI.Deposit.API
    ( DepositsParams (depositsViewStart)
    , TransactionHistoryParams
    , UI
    , settingsSseToggleLink
    )
import Cardano.Wallet.UI.Deposit.Handlers.Addresses
    ( getAddresses
    , getCustomerAddress
    )
import Cardano.Wallet.UI.Deposit.Handlers.Addresses.Transactions
    ( getCustomerHistory
    )
import Cardano.Wallet.UI.Deposit.Handlers.Deposits
    ( depositsHistoryHandler
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( walletPresence
    )
import Cardano.Wallet.UI.Deposit.Handlers.Wallet
    ( deleteWalletHandler
    , getWallet
    , postMnemonicWallet
    , postXPubWallet
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Addresses
    ( addressElementH
    , customerAddressH
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Addresses.Transactions
    ( customerHistoryH
    , transactionsElementH
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Deposits
    ( depositsElementH
    , depositsHistoryH
    , depositsPartsH
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Page
    ( Page (..)
    , headerElementH
    , page
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Wallet
    ( deleteWalletModalH
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
    , getCurrentTime
    )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime
    )
import Lucid
    ( Html
    , ToHtml (..)
    , class_
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
    :: forall n x
     . HasSNetworkId n
    => Tracer IO String
    -> NetworkEnv IO x
    -> UILayer WalletResource
    -> WalletBootEnv IO
    -> FilePath
    -> PageConfig
    -> SNetworkId n
    -> NetworkLayer IO Read.ConsensusBlock
    -> BlockchainSource
    -> Server UI
serveUI tr network ul env dbDir config _ nl bs =
    ph Wallet
        :<|> ph About
        :<|> ph Network
        :<|> ph Settings
        :<|> ph Wallet
        :<|> ph Addresses
        :<|> ph Deposits
        :<|> sessioning (renderSmoothHtml . networkInfoH showTime <$> getNetworkInformation nid nl mode)
        :<|> wsl (\l -> getState l (renderSmoothHtml . settingsStateH settingsSseToggleLink))
        :<|> wsl (\l -> toggleSSE l $> RawHtml "")
        :<|> withSessionLayerRead ul (sse . sseConfig)
        :<|> serveFavicon
        :<|> serveFakeDataBackground
        :<|> (\c -> sessioning $ renderSmoothHtml . mnemonicH <$> liftIO (pickMnemonic 15 c))
        :<|> wsl (\l -> getWallet l (renderSmoothHtml . walletElementH alertH))
        :<|> (\v -> wsl (\l -> postMnemonicWallet l initWallet alert ok v))
        :<|> (\v -> wsl (\l -> postXPubWallet l initWallet alert ok v))
        :<|> wsl (\l -> deleteWalletHandler l (deleteWallet dbDir) alert ok)
        :<|> wsl (\_l -> pure $ renderSmoothHtml deleteWalletModalH)
        :<|> (\c -> wsl (\l -> getCustomerAddress l (renderSmoothHtml . customerAddressH) alert c))
        :<|> wsl (\l -> getAddresses l (\now -> renderSmoothHtml . addressElementH now origin alertH))
        :<|> serveNavigation
        :<|> serveTransactions ul
        :<|> serveCustomerHistory network ul
        :<|> serveDeposits ul
        :<|> serveDepositsHistory network ul
        :<|> serveDepositsHistoryExtension network ul
  where
    serveNavigation mp = wsl $ \l -> do
        wp <- walletPresence l

        pure $ renderSmoothHtml $ headerElementH mp wp
    ph p = wsl $ \_ -> pure $ page config p
    ok _ = renderHtml . rogerH @Text $ "ok"
    nid = networkIdVal (sNetworkId @n)
    mode = case bs of
        NodeSource{} -> Node
    _ = networkInfoH
    wsl f = withSessionLayer ul $ \l -> f l
    initWallet = initXPubWallet tr env dbDir

alert :: ToHtml a => a -> RawHtml
alert = renderHtml . alertH

serveFakeDataBackground :: Handler BL.ByteString
serveFakeDataBackground = do
    file <- liftIO $ getDataFileName "data/images/fake-data.png"
    liftIO $ BL.readFile file

serveCustomerHistory
    :: NetworkEnv IO a
    -> UILayer WalletResource
    -> TransactionHistoryParams
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveCustomerHistory network ul params = do
    withSessionLayer ul $ \layer ->
        renderSmoothHtml
            <$> getCustomerHistory
                network
                layer
                customerHistoryH
                alertH
                params

renderSmoothHtml :: Html () -> RawHtml
renderSmoothHtml response =
    renderHtml
        $ div_ [class_ "smooth"]
        $ toHtml response

serveFavicon :: Handler BL.ByteString
serveFavicon = do
    file <- liftIO $ getDataFileName "data/images/icon.png"
    liftIO $ BL.readFile file

serveTransactions
    :: UILayer WalletResource
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveTransactions ul =
    withSessionLayer ul
        $ \_ -> do
            now <- liftIO getCurrentTime
            pure
                $ renderSmoothHtml
                $ transactionsElementH now origin

origin :: UTCTime
origin = posixSecondsToUTCTime $ fromIntegral originTime

serveDeposits
    :: UILayer WalletResource
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveDeposits ul = withSessionLayer ul $ \layer -> do
    wp <- walletPresence layer
    pure
        $ renderSmoothHtml
        $ depositsElementH alertH wp

serveDepositsHistory
    :: NetworkEnv IO a
    -> UILayer WalletResource
    -> DepositsParams
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveDepositsHistory network ul params = withSessionLayer ul $ \layer -> do
    renderSmoothHtml
        <$> depositsHistoryHandler
            network
            layer
            (depositsHistoryH params)
            alertH
            params{depositsViewStart = Nothing}

serveDepositsHistoryExtension
    :: NetworkEnv IO a
    -> UILayer WalletResource
    -> DepositsParams
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveDepositsHistoryExtension network ul params = withSessionLayer ul $ \layer -> do
    renderHtml <$> depositsHistoryHandler
        network
        layer
        (depositsPartsH params)
        alertH
        params
