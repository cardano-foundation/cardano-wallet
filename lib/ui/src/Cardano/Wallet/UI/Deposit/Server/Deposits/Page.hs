module Cardano.Wallet.UI.Deposit.Server.Deposits.Page
    ( serveDepositsPage
    )
where

import Prelude

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
import Cardano.Wallet.UI.Deposit.API
    ( depositsTimesLink
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( walletPresence
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Deposits.Page
    ( depositsElementH
    )
import Cardano.Wallet.UI.Deposit.Server.Lib
    ( renderSmoothHtml
    )
import Servant
    ( Handler
    )

serveDepositsPage
    :: UILayer WalletResource
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveDepositsPage ul = withSessionLayer ul $ \layer -> do
    wp <- walletPresence layer
    pure
        $ renderSmoothHtml
        $ depositsElementH depositsTimesLink alertH wp
