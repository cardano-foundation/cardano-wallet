{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.UI.Html.Pages.Page
    ( Page (..)
    , page
    )
where

import Prelude

import Cardano.Wallet.UI.API
    ( aboutPageLink
    , addressesPageLink
    , networkPageLink
    , settingsPageLink
    , walletPageLink
    , walletsPageLink
    )
import Cardano.Wallet.UI.Html.Html
    ( RawHtml (..)
    )
import Cardano.Wallet.UI.Html.Pages.About
    ( aboutH
    )
import Cardano.Wallet.UI.Html.Pages.Addresses
    ( addressesPageH
    )
import Cardano.Wallet.UI.Html.Pages.Network
    ( networkH
    )
import Cardano.Wallet.UI.Html.Pages.Settings
    ( settingsPageH
    )
import Cardano.Wallet.UI.Html.Pages.Template.Footer
    ( footerH
    )
import Cardano.Wallet.UI.Html.Pages.Template.Head
    ( pageFromBodyH
    )
import Cardano.Wallet.UI.Html.Pages.Template.Navigation
    ( navigationH
    )
import Cardano.Wallet.UI.Html.Pages.Wallet
    ( WalletPresent
    , walletH
    )
import Cardano.Wallet.UI.Html.Pages.Wallets
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
    , class_
    , div_
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
    :: Text
    -- ^ Prefix to prepend to all links
    -> Page
    -- ^ Page to render
    -> WalletPresent
    -- ^ If a wallet was selected
    -> RawHtml
page prefix p wp = RawHtml
    $ renderBS
    $ pageFromBodyH
    $ bodyH prefix p
    $ case p of
        About -> aboutH
        Network -> networkH
        Wallets -> walletsH
        Wallet -> walletH wp
        Addresses -> addressesPageH
        Settings -> settingsPageH

bodyH
    :: Text
    -- ^ Prefix
    -> Page
    -- ^ Current page
    -> Html ()
    -- ^ Body content
    -> Html ()
bodyH prefix p body = do
    headerH prefix p
    div_ [class_ "container-fluid"] $ do
        div_ [class_ "main"] body
        div_
            [class_ "footer"]
            footerH

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
