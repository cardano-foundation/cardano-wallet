module Cardano.Wallet.UI.Deposit.Server.Addresses
    ( serveCustomerHistory
    , serveAddressesPage
    , serveGetAddress
    )
where

import Prelude

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
import Cardano.Wallet.UI.Deposit.API.Addresses.Transactions
    ( TransactionHistoryParams
    )
import Cardano.Wallet.UI.Deposit.Handlers.Addresses
    ( getAddresses
    , getCustomerAddress
    )
import Cardano.Wallet.UI.Deposit.Handlers.Addresses.Transactions
    ( getCustomerHistory
    )
import Cardano.Wallet.UI.Deposit.Html.Common
    ( addressH
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Addresses
    ( addressElementH
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Addresses.Transactions
    ( customerHistoryH
    )
import Cardano.Wallet.UI.Deposit.Server.Lib
    ( alert
    , origin
    , renderSmoothHtml
    )
import Servant
    ( Handler
    )

serveCustomerHistory
    :: UILayer WalletResource
    -> TransactionHistoryParams
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveCustomerHistory ul params = do
    withSessionLayer ul $ \layer ->
        renderSmoothHtml
            <$> getCustomerHistory
                layer
                customerHistoryH
                alertH
                params

serveGetAddress
    :: UILayer WalletResource
    -> Customer
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveGetAddress ul c = withSessionLayer ul $ \l -> do
    getCustomerAddress
        l
        (renderSmoothHtml . addressH WithCopy)
        alert
        c

serveAddressesPage
    :: UILayer WalletResource
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveAddressesPage ul = withSessionLayer ul $ \l -> do
    getAddresses l
        $ \now -> renderSmoothHtml . addressElementH now origin alertH
