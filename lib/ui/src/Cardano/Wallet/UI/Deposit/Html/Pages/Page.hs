{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.UI.Deposit.Html.Pages.Page
    ( Page (..)
    , page
    , PageConfig (..)
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
import Cardano.Wallet.UI.Common.Html.Pages.Template.Footer
    ( footerH
    )
import Cardano.Wallet.UI.Common.Html.Pages.Template.Head
    ( HeadConfig
    , pageFromBodyH
    )
import Cardano.Wallet.UI.Common.Html.Pages.Template.Navigation
    ( navigationH
    )
import Cardano.Wallet.UI.Deposit.API
    ( aboutPageLink
    , faviconLink
    , networkInfoLink
    , networkPageLink
    , settingsGetLink
    , settingsPageLink
    , sseLink
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.About
    ( aboutH
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
    | Settings

makePrisms ''Page

data PageConfig = PageConfig
    { prefix :: Text
    -- ^ Prefix to prepend to all links
    , headConfig :: HeadConfig
    -- ^ Head configuration
    }

page
    :: PageConfig
    -- ^ Page configuration
    -> Page
    -- ^ If a wallet was selected
    -> RawHtml
page PageConfig{..} p  = RawHtml
    $ renderBS
    $ pageFromBodyH faviconLink headConfig
    $ bodyH prefix p
    $ case p of
        About -> aboutH
        Network -> networkH sseLink networkInfoLink
        Settings -> settingsPageH sseLink settingsGetLink

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
        , (is _Settings p, settingsPageLink, "Settings")
        ]
