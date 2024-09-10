module Cardano.Wallet.UI.Deposit.Handlers.Page
where

import Prelude

import Cardano.Wallet.Deposit.REST
    ( WalletResource
    , walletExists
    , walletPublicIdentity
    )
import Cardano.Wallet.UI.Common.Handlers.Session
    ( withSessionLayer
    )
import Cardano.Wallet.UI.Common.Html.Html
    ( RawHtml (..)
    )
import Cardano.Wallet.UI.Common.Html.Pages.Template.Head
    ( PageConfig
    )
import Cardano.Wallet.UI.Common.Layer
    ( UILayer (..)
    )
import Cardano.Wallet.UI.Cookies
    ( CookieResponse
    , RequestCookies
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( catchRunWalletResourceM
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Page
    ( Page (..)
    , page
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Wallet
    ( WalletPresent (..)
    )
import Servant
    ( Handler
    )

pageHandler
    :: UILayer WalletResource
    -- ^ The deposit UI layer
    -> FilePath
    -- ^ The directory where the wallet data is stored
    -> PageConfig
    -- ^ The page configuration
    -> Page
    -- ^ The page to render
    -> Maybe RequestCookies
    -- ^ The request cookies
    -> Handler (CookieResponse RawHtml)
pageHandler layer dir config x =
    withSessionLayer layer $ \session -> do
        w <- catchRunWalletResourceM session $ do
            test <- walletExists dir
            if test
                then do
                    WalletPresent <$> walletPublicIdentity
                else pure WalletAbsent
        pure $ page config x w
