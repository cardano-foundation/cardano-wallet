{-# LANGUAGE RecordWildCards #-}
module Cardano.Wallet.UI.Personal.Html.Pages.Template.Head
    ( pageFromBodyH
    , HeadConfig (..)
    )
where

import Prelude

import Cardano.Wallet.UI.Common.Html.Htmx
    ( useHtmxVersion
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( linkText
    )
import Cardano.Wallet.UI.Personal.API
    ( faviconLink
    )
import qualified Data.Text as T
import Lucid
    ( Html
    , ToHtml (..)
    , body_
    , charset_
    , content_
    , crossorigin_
    , head_
    , href_
    , html_
    , integrity_
    , link_
    , meta_
    , name_
    , rel_
    , src_
    , term
    , title_
    )
import Servant
    ( Link
    )

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

bootstrapIcons :: Html ()
bootstrapIcons =
    link_
        [ rel_ "stylesheet"
        , href_ "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css"
        , integrity_
            "sha384-XI+Zz5ooq0QtjZVWisDbzKhZHpDvojmuW5fxM3Z1NY3VHU2hlI7c1o4TmYH72yK"
        , crossorigin_ "anonymous"
        ]

popperScript :: Html ()
popperScript =
    term
        "script"
        [ src_
            "https://cdn.jsdelivr.net/npm/@popperjs/core@2.9.2/dist/umd/popper.min.js"
        , integrity_
            "sha384-UOdGjl+2WYrdV0fJ9xJJ4TLEkH4WcJs1SXmeaZ7uwy/ZPwmYupt0VyrKjqqhd8q8"
        , crossorigin_ "anonymous"
        ]
        $ pure ()

favicon :: Link -> Html ()
favicon path =
    link_
        [ rel_ "icon"
        , href_ $ linkText path
        ]

newtype HeadConfig = HeadConfig
    { title :: T.Text
    }

pageFromBodyH :: HeadConfig -> Html () -> Html ()
pageFromBodyH HeadConfig{..} body = html_ [term "data-bs-theme" "dark"]
    $ do
        head_ $ do
            title_ $ toHtml title
            meta_ [charset_ "utf-8"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
            bootstrapLink
            bootstrapScript
            bootstrapIcons
            popperScript
            favicon faviconLink
            useHtmxVersion (1,9,12)
        body_ body
