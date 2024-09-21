{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.UI.Deposit.Html.Pages.Page
    ( Page (..)
    , page
    )
where

import Prelude

import Cardano.Wallet.UI.Common.Html.Html
    ( RawHtml (..)
    )
import Cardano.Wallet.UI.Common.Html.Modal
    ( modalsH
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
import Cardano.Wallet.UI.Deposit.API
    ( aboutPageLink
    , addressesPageLink
    , faviconLink
    , networkInfoLink
    , networkPageLink
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
import Cardano.Wallet.UI.Deposit.Html.Pages.Wallet
    ( WalletPresent
    , isPresent
    , walletH
    )
import Cardano.Wallet.UI.Type
    ( WalletType (..)
    , runWHtml
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
    ( HtmlT
    , renderBS
    )

data Page
    = About
    | Network
    | Settings
    | Wallet
    | Addresses

makePrisms ''Page

page
    :: PageConfig
    -- ^ Page configuration
    -> Page
    -- ^ Current page
    -> WalletPresent
    -- ^ Wallet present
    -> RawHtml
page c@PageConfig{..} p wp = RawHtml
    $ renderBS
    $ runWHtml Deposit
    $ pageFromBodyH faviconLink c
    $ do
        bodyH sseLink (headerH prefix p wp)
            $ do
                modalsH
                case p of
                    About -> aboutH
                    Network -> networkH networkInfoLink
                    Settings -> settingsPageH settingsGetLink
                    Wallet -> walletH
                    Addresses -> addressesH

headerH :: Text -> Page -> WalletPresent -> Monad m => HtmlT m ()
headerH prefix p wp =
    navigationH
        prefix $
        [ (is _Wallet p, walletPageLink, "Wallet")
        ]
        <>
        [(is _Addresses p, addressesPageLink, "Addresses") | isPresent wp]
        <>
        [ (is _Network p, networkPageLink, "Network")
        , (is _Settings p, settingsPageLink, "Settings")
        , (is _About p, aboutPageLink, "About")
        ]
