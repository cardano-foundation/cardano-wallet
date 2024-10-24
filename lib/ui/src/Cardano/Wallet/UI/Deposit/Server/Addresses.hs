module Cardano.Wallet.UI.Deposit.Server.Addresses
    ( serveCustomerHistory
    , serveAddressesPage
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
import Cardano.Wallet.UI.Deposit.Handlers.Addresses
    ( getAddresses
    , getCustomerAddress
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Addresses
    ( addressElementH
    , customerAddressH
    )
import Cardano.Wallet.UI.Deposit.Server.Lib
    ( alert
    , renderSmoothHtml
    )
import Servant
    ( Handler
    )

serveCustomerHistory
    :: UILayer WalletResource
    -> Customer
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveCustomerHistory ul customer = do
    withSessionLayer ul $ \layer ->
        getCustomerAddress layer
            (renderSmoothHtml . customerAddressH) alert customer

serveAddressesPage
    :: UILayer WalletResource
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveAddressesPage ul = withSessionLayer ul $ \l -> do
    getAddresses l
        $ renderSmoothHtml . addressElementH alertH
