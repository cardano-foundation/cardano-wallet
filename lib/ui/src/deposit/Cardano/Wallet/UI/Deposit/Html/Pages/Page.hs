{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.UI.Deposit.Html.Pages.Page
    ( Page (..)
    , page
    , headerElementH
    )
where

import Prelude

import Cardano.Wallet.UI.Common.Html.Html
    ( RawHtml (..)
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( imageOverlay
    )
import Cardano.Wallet.UI.Common.Html.Modal
    ( modalsH
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( sseH
    )
import Cardano.Wallet.UI.Common.Html.Pages.Settings
    ( settingsPageH
    )
import Cardano.Wallet.UI.Common.Html.Pages.Template.Body
    ( bodyH
    )
import Cardano.Wallet.UI.Common.Html.Pages.Template.Head
    ( PageConfig (..)
    , pageFromBodyH
    )
import Cardano.Wallet.UI.Common.Html.Pages.Template.Navigation
    ( Navigation (..)
    , navigationH
    )
import Cardano.Wallet.UI.Deposit.API
    ( Page (..)
    , _About
    , _Addresses
    , _Deposits
    , _Payments
    , _Settings
    , _Wallet
    , aboutPageLink
    , addressesPageLink
    , depositPageLink
    , depositsLink
    , faviconLink
    , homePageLink
    , navigationLink
    , paymentsLink
    , paymentsPageLink
    , settingsGetLink
    , settingsPageLink
    , sseLink
    , walletPageLink
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.About
    ( aboutH
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Addresses
    ( addressesH
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Deposits.Page
    ( depositsH
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Payments.Page
    ( paymentsH
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Wallet
    ( WalletPresent
    , isPresent
    , walletH
    )
import Cardano.Wallet.UI.Type
    ( WalletType (..)
    , runWHtml
    )
import Control.Lens
    ( _Just
    )
import Control.Lens.Extras
    ( is
    )
import Lucid
    ( HtmlT
    , renderBS
    )

page
    :: PageConfig
    -- ^ Page configuration
    -> Page
    -- ^ Current page
    -> RawHtml
page c p = RawHtml
    $ renderBS
    $ runWHtml Deposit
    $ pageFromBodyH faviconLink c
    $ do
        bodyH sseLink (headerH p)
            $ do
                modalsH
                imageOverlay
                case p of
                    About -> aboutH
                    Settings -> settingsPageH settingsGetLink
                    Wallet -> walletH
                    Addresses -> addressesH
                    Deposits -> depositsH depositsLink
                    Payments -> paymentsH paymentsLink

headerH :: Monad m => Page -> HtmlT m ()
headerH p = sseH (navigationLink $ Just p) "header" ["wallet"]

headerElementH :: Maybe Page -> WalletPresent -> Monad m => HtmlT m ()
headerElementH p wp =
    navigationH
        mempty
        Navigation
            { navigationHomePage = homePageLink
            , navigationTitle = "Cardano Deposit Wallet"
            , navigationFavicon = faviconLink
            }
        $ [(is' _Wallet, walletPageLink, "Wallet")]
            <> [ (is' _Addresses, addressesPageLink, "Addresses")
               | isPresent wp
               ]
            <> [ (is' _Deposits, depositPageLink, "Deposits")
               | isPresent wp
               ]
            <> [ (is' _Payments, paymentsPageLink, "Payments")
               | isPresent wp
               ]
            <> [ (is' _Settings, settingsPageLink, "Settings")
               , (is' _About, aboutPageLink, "About")
               ]
  where
    is' l = is (_Just . l) p
