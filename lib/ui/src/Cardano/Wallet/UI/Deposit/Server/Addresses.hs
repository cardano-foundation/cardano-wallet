module Cardano.Wallet.UI.Deposit.Server.Addresses
    ( serveCustomerHistory
    , serveTransactions
    , serveGetAddress
    , serveAddressesPage
    )
where

import Prelude

import Cardano.Wallet.Deposit.IO.Network.Type
    ( NetworkEnv
    )
import Cardano.Wallet.Deposit.Pure
    ( Customer
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResource
    )
import Cardano.Wallet.UI.Common.Handlers.Session
    ( withSessionLayer
    )
import Cardano.Wallet.UI.Common.Html.Html
    ( RawHtml (..)
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( WithCopy (..)
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( alertH
    )
import Cardano.Wallet.UI.Common.Layer
    ( UILayer (..)
    )
import Cardano.Wallet.UI.Cookies
    ( CookieResponse
    , RequestCookies
    )
import Cardano.Wallet.UI.Deposit.API
    ( TransactionHistoryParams
    )
import Cardano.Wallet.UI.Deposit.Handlers.Addresses
    ( getAddresses
    , getCustomerAddress
    )
import Cardano.Wallet.UI.Deposit.Handlers.Addresses.Transactions
    ( getCustomerHistory
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Addresses
    ( addressElementH
    , customerAddressH
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Addresses.Transactions
    ( customerHistoryH
    , transactionsElementH
    )
import Cardano.Wallet.UI.Deposit.Server.Lib
    ( alert
    , origin
    , renderSmoothHtml
    )
import Control.Monad.Trans
    ( MonadIO (..)
    )
import Data.Time
    ( getCurrentTime
    )
import Servant
    ( Handler
    )

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

serveGetAddress
    :: UILayer WalletResource
    -> Customer
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveGetAddress ul c = withSessionLayer ul $ \l -> do
    getCustomerAddress l (renderSmoothHtml . customerAddressH WithCopy) alert c

serveAddressesPage
    :: UILayer WalletResource
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveAddressesPage ul = withSessionLayer ul $ \l -> do
    getAddresses l
        $ \now -> renderSmoothHtml . addressElementH now origin alertH
