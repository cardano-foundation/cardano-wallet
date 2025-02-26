{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.UI.Deposit.Server
    ( serveUI
    ) where

import Prelude

import Cardano.Wallet.Deposit.IO
    ( WalletBootEnv (networkEnv)
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResource
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
    )
import Cardano.Wallet.UI.Deposit.API
    ( UI
    , settingsSseToggleLink
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( walletPresence
    )
import Cardano.Wallet.UI.Deposit.Html.Common
    ( modalElementH
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
import Cardano.Wallet.UI.Deposit.Server.Deposits.Customers
    ( serveDepositsCustomerPagination
    , serveDepositsCustomers
    )
import Cardano.Wallet.UI.Deposit.Server.Deposits.Page
    ( serveDepositsPage
    )
import Cardano.Wallet.UI.Deposit.Server.Deposits.Times
    ( serveDeposits
    , serveDepositsPagination
    )
import Cardano.Wallet.UI.Deposit.Server.Deposits.TxIds
    ( serveDepositsCustomersTxIds
    , serveDepositsCustomersTxIdsPagination
    )
import Cardano.Wallet.UI.Deposit.Server.Lib
    ( renderSmoothHtml
    )
import Cardano.Wallet.UI.Deposit.Server.Payments.Page
    ( servePaymentsBalanceAvailable
    , servePaymentsDeleteReceiver
    , servePaymentsNewReceiver
    , servePaymentsPage
    , servePaymentsReceiverAddressValidation
    , servePaymentsReceiverAmountValidation
    , servePaymentsReset
    , servePaymentsSign
    , servePaymentsSubmit
    )
import Cardano.Wallet.UI.Deposit.Server.Wallet
    ( serveDeleteWallet
    , serveDeleteWalletModal
    , serveMnemonic
    , servePostMnemonicWallet
    , servePostXPubWallet
    , serveWalletPage
    , serveWalletStatus
    )
import Cardano.Wallet.UI.Static
    ( favicon
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
import Servant
    ( Handler
    , Server
    , (:<|>) (..)
    )
import Servant.Types.SourceT
    ( SourceT
    )

serveUI
    :: Tracer IO ()
    -- ^ Tracer for wallet tip changes
    -> Tracer IO String
    -- ^ Tracer for logging
    -> UILayer WalletResource
    -- ^ UI layer
    -> WalletBootEnv IO
    -- ^ Wallet boot environment
    -> FilePath
    -- ^ Database directory
    -> PageConfig
    -- ^ Page configuration
    -> Server UI
serveUI wtc tr ul env dbDir config =
    serveTabPage ul config Wallet
        :<|> serveTabPage ul config About
        :<|> serveTabPage ul config Settings
        :<|> serveTabPage ul config Wallet
        :<|> serveTabPage ul config Addresses
        :<|> serveTabPage ul config Deposits
        :<|> serveTabPage ul config Payments
        :<|> serveSSESettings ul
        :<|> serveToggleSSE ul
        :<|> serveSSE ul
        :<|> pure favicon
        :<|> serveMnemonic
        :<|> serveWalletPage ul
        :<|> servePostMnemonicWallet wtc tr env dbDir ul
        :<|> servePostXPubWallet wtc tr env dbDir ul
        :<|> serveDeleteWallet ul dbDir
        :<|> serveDeleteWalletModal ul
        :<|> serveGetAddress ul
        :<|> serveAddressesPage ul
        :<|> serveNavigation ul
        :<|> serveCustomerHistory ul
        :<|> serveDepositsPage ul
        :<|> serveDeposits ul
        :<|> serveDepositsPagination ul
        :<|> serveDepositsCustomers ul
        :<|> serveDepositsCustomerPagination ul
        :<|> serveDepositsCustomersTxIds ul
        :<|> serveDepositsCustomersTxIdsPagination ul
        :<|> servePaymentsPage ul
        :<|> servePaymentsNewReceiver ul
        :<|> servePaymentsDeleteReceiver ul
        :<|> servePaymentsBalanceAvailable ul
        :<|> servePaymentsReceiverAddressValidation ul
        :<|> servePaymentsReceiverAmountValidation ul
        :<|> serveModal ul
        :<|> servePaymentsSign ul
        :<|> servePaymentsSubmit ul
        :<|> servePaymentsReset ul
        :<|> serveWalletStatus (networkEnv env) ul

serveModal
    :: UILayer WalletResource
    -> Maybe Text
    -> Maybe Text
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveModal ul mtitle mbody = withSessionLayer ul $ \_ ->
    pure
        $ renderSmoothHtml
        $ modalElementH mtitle mbody

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
