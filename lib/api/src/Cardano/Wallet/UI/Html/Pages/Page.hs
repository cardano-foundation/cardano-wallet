{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.UI.Html.Pages.Page where

import Prelude

import Cardano.Wallet.UI.Html.Html
    ( RawHtml (..)
    )
import Cardano.Wallet.UI.Html.Htmx
    ( useHtmx
    )
import Cardano.Wallet.UI.Html.Pages.About
    ( aboutH
    )
import Cardano.Wallet.UI.Html.Pages.Lib
    ( sseInH
    )
import Cardano.Wallet.UI.Html.Pages.Network
    ( networkH
    )
import Cardano.Wallet.UI.Html.Pages.Settings
    ( settingsPageH
    )
import Cardano.Wallet.UI.Html.Pages.Wallet
    ( walletH
    , walletNewH
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
    ( Attribute
    , Html
    , a_
    , body_
    , charset_
    , class_
    , content_
    , crossorigin_
    , div_
    , head_
    , header_
    , href_
    , html_
    , integrity_
    , li_
    , link_
    , meta_
    , name_
    , rel_
    , renderBS
    , span_
    , src_
    , style_
    , term
    , title_
    , ul_
    )

data Page = About | Network | WalletNew | Wallets | Wallet | Settings

makePrisms ''Page

page
    :: Text
    -> Page
    -> RawHtml
page prefix p = RawHtml $ renderBS $ pageH prefix p $ case p of
    About -> aboutH
    Network -> networkH
    WalletNew -> walletNewH
    Wallets -> walletsH
    Wallet -> walletH
    Settings -> settingsPageH

bootstrapLink :: Html ()
bootstrapLink =
    link_
        [ rel_ "stylesheet"
        , href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"
        , integrity_
            "sha384-9ndCyUaIbzAi2FUVXJi0CjmCapSmO7SnpJef0486qhLnuZ2cdeRhO02iuK6FUUVM"
        , crossorigin_ "anonymous"
        ]

bootstrapScript :: Html ()
bootstrapScript =
    term
        "script"
        [ src_
            "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js"
        , integrity_
            "sha384-geWF76RCwLtnZ8qwWowPQNguL3RmwHVBC9FhGdlKrxdiJJigb/j/68SIy3Te4Bkz"
        , crossorigin_ "anonymous"
        ]
        $ pure ()

pageH :: Text -> Page -> Html () -> Html ()
pageH prefix p body = html_ [term "data-bs-theme" "dark"]
    $ do
        head_ $ do
            title_ "Cardano Wallet UI"
            meta_ [charset_ "utf-8"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
            bootstrapLink
            bootstrapScript
            useHtmx
            pure ()
        body_ $ do
            headH p prefix
            div_ [class_ "container-fluid"] $ do
                div_
                    [class_ "main"]
                    body
                div_
                    [class_ "footer"]
                    footerH

activePageH :: Bool -> [Attribute] -> [Attribute]
activePageH c =
    if c
        then (<> [class_ "nav-link active", term "aria-current" "page"])
        else (<> [class_ "nav-link"])

tabOf :: Text -> Bool -> Text -> Html () -> Html ()
tabOf prefix c p t =
    li_ [class_ "nav-item"]
        $ a_ (activePageH c [href_ $ prefix <> p]) t

headH :: Page -> Text -> Html ()
headH p prefix = do
    header_ [class_ "d-flex justify-content-center py-3"] $ do
        ul_ [class_ "nav nav-pills"] $ do
            tabOf prefix (is _About p) "/about" "About"
            tabOf prefix (is _Network p) "/network" "Network"
            tabOf prefix (is _WalletNew p) "/wallet/new" "New"
            tabOf prefix (is _Wallets p) "/wallets" "List"
            tabOf prefix (is _Wallet p) "/wallet" "Wallet"
            tabOf prefix (is _Settings p) "/settings" "Settings"
        sseInH ["time"]

githubLinkH :: Html ()
githubLinkH =
    a_
        [href_ "https://github.com/cardano-foundation/cardano-wallet"]
        "GitHub"

footerH :: Html ()
footerH =
    term
        "footer_"
        [ class_
            "text-center text-muted bg-secondary"
        ]
        $ do
            div_ [class_ "row d-md-flex align-items-center"]
                $ do
                    ul_ [class_ "nav flex-column"] $ do
                        li_
                            [class_ "nav-item mb-2"]
                            "© 2024 Cardano Foundation, HAL team"
                        li_ [class_ "nav-item mb-2"] $ do
                            span_ "Source code on "
                            githubLinkH

            div_ [class_ "row d-md-flex align-items-center"]
                $ div_
                    [class_ "mb-3 mb-md-0 text-body-secondary"]
                    "Powered by Haskell, Servant, Lucid, Bootstrap"

bgcolor_ :: Text -> Attribute
bgcolor_ color = style_ $ "background: " <> color <> ";"
