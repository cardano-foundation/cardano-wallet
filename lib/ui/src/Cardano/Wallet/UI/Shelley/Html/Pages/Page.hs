{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.UI.Shelley.Html.Pages.Page
    ( Page (..)
    , page
    )
where

import Prelude

import Cardano.Wallet.UI.Common.Html.Html
    ( RawHtml (..)
    )
import Cardano.Wallet.UI.Common.Html.Pages.Network
    ( networkH
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
    ( navigationH
    )
import Cardano.Wallet.UI.Shelley.API
    ( aboutPageLink
    , addressesPageLink
    , faviconLink
    , networkInfoLink
    , networkPageLink
    , settingsGetLink
    , settingsPageLink
    , sseLink
    , walletPageLink
    , walletsPageLink
    )
import Cardano.Wallet.UI.Shelley.Html.Pages.About
    ( aboutH
    )
import Cardano.Wallet.UI.Shelley.Html.Pages.Addresses
    ( addressesPageH
    )
import Cardano.Wallet.UI.Shelley.Html.Pages.Wallet
    ( WalletPresent
    , walletH
    )
import Cardano.Wallet.UI.Shelley.Html.Pages.Wallets
    ( walletsH
    )
import Control.Lens.Extras
    ( is
    )
import Control.Lens.TH
    ( makePrisms
    )
import Data.Text
    ( Text
    )
import Lucid
    ( Html
    , renderBS
    )

data Page
    = About
    | Network
    | Wallets
    | Wallet
    | Settings
    | Addresses

makePrisms ''Page

page
    :: PageConfig
    -- ^ Page configuration
    -> Page
    -- ^ Page to render
    -> WalletPresent
    -- ^ If a wallet was selected
    -> RawHtml
page c@PageConfig{..} p wp = RawHtml
    $ renderBS
    $ pageFromBodyH faviconLink c
    $ bodyH (headerH prefix p)
    $ case p of
        About -> aboutH
        Network -> networkH sseLink networkInfoLink
        Wallets -> walletsH
        Wallet -> walletH wp
        Addresses -> addressesPageH
        Settings -> settingsPageH sseLink settingsGetLink

headerH :: Text -> Page -> Html ()
headerH prefix p =
    navigationH
        prefix
        [ (is _About p, aboutPageLink, "About")
        , (is _Network p, networkPageLink, "Network")
        , (is _Wallets p, walletsPageLink, "List")
        , (is _Wallet p, walletPageLink, "Wallet")
        , (is _Addresses p, addressesPageLink, "Addresses")
        , (is _Settings p, settingsPageLink, "Settings")
        ]
