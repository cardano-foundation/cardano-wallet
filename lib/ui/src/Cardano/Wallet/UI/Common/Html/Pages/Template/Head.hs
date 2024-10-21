{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Common.Html.Pages.Template.Head
    ( pageFromBodyH
    , PageConfig (..)
    )
where

import Prelude

import Cardano.Wallet.UI.Common.Html.Copy
    ( offscreenCss
    )
import Cardano.Wallet.UI.Common.Html.Htmx
    ( useHtmxExtension
    , useHtmxVersion
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( linkText
    , monospaced
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( fadeInId
    )
import Data.Text
    ( Text
    )
import Lucid
    ( HtmlT
    , ToHtml (..)
    , body_
    , charset_
    , class_
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
    , style_
    , term
    , title_
    )
import Servant
    ( Link
    )

bootstrapLink :: Monad m => HtmlT m ()
bootstrapLink =
    link_
        [ rel_ "stylesheet"
        , href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css"
        , integrity_
            "sha384-QWTKZyjpPEjISv5WaRU9OFeRpok6YctnYmDr5pNlyT2bRjXh0JMhjY6hW+ALEwIH"
        , crossorigin_ "anonymous"
        ]

bootstrapScript :: Monad m => HtmlT m ()
bootstrapScript =
    term
        "script"
        [ src_
            "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js"
        , integrity_
            "sha384-YvpcrYf0tY3lHB60NNkmXc5s9fDVZLESaAA55NDzOxhy9GkcIdslK1eN7N6jIeHz"
        , crossorigin_ "anonymous"
        ]
        $ pure ()

bootstrapIcons :: Monad m => HtmlT m ()
bootstrapIcons =
    link_
        [ rel_ "stylesheet"
        , href_ "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css"
        , integrity_
            "sha384-XGjxtQfXaH2tnPFa9x+ruJTuLE3Aa6LhHSWRr1XeTyhezb4abCG4ccI5AkVDxqC+"
        , crossorigin_ "anonymous"
        ]

popperScript :: Monad m => HtmlT m ()
popperScript =
    term
        "script"
        [ src_
            "https://cdn.jsdelivr.net/npm/@popperjs/core@2.11.8/dist/umd/popper.min.js"
        , integrity_
            "sha384-I7E8VVD/ismYTF4hNIPjVp/Zjvgyol6VFvRkX/vR+Vc4jQkC+hVqc2pM8ODewa9r"
        , crossorigin_ "anonymous"
        ]
        $ pure ()

clipboardScript :: Monad m => HtmlT m ()
clipboardScript =
    term
        "script"
        [ src_
            "https://cdn.jsdelivr.net/npm/clipboard@2.0.11/dist/clipboard.min.js"
        , integrity_
            "sha384-J08i8An/QeARD9ExYpvphB8BsyOj3Gh2TSh1aLINKO3L0cMSH2dN3E22zFoXEi0Q"
        , crossorigin_ "anonymous"
        ]
        $ pure ()

flatPickrScript :: Monad m => HtmlT m ()
flatPickrScript =
    term
        "script"
        [ src_
            "https://cdn.jsdelivr.net/npm/flatpickr/dist/flatpickr.min.js"
        , integrity_
            "sha384-5JqMv4L/Xa0hfvtF06qboNdhvuYXUku9ZrhZh3bSk8VXF0A/RuSLHpLsSV9Zqhl6"
        , crossorigin_ "anonymous"
        ]
        $ pure ()

flatPickrCSSDark :: Monad m => HtmlT m ()
flatPickrCSSDark =
    link_
        [ rel_ "stylesheet"
        , href_ "https://cdn.jsdelivr.net/npm/flatpickr/dist/themes/dark.css"
        , integrity_
            "sha384-heUUtXw0Djj2DElfLOVPlASSWFKNL3JDY6s3FEqQD03GYyWOeophY5DsfEG40sYX"
        , crossorigin_ "anonymous"
        ]

-- | Render a favicon link.
favicon :: Link -> Monad m => HtmlT m ()
favicon path =
    link_
        [ rel_ "icon"
        , href_ $ linkText path
        ]

-- make the body centered and have a max-width
bodyCss :: Monad m => HtmlT m ()
bodyCss =
    style_ []
        $ toHtml @Text
            "html {max-width:1200px; margin: 0 auto;};"

-- https://stackoverflow.com/questions/11088938/is-this-the-best-way-to-make-the-body-max-width-and-centered
-- this is for modals to appear at the center of the screen even on big screens
-- where the body is centered and has a max-width
modalCssWorkaround :: Monad m => HtmlT m ()
modalCssWorkaround =
    style_ []
        $ toHtml @Text
            ".modal {padding-right: 0px!important;}\
            \.modal-open {padding-right: 0px!important;}"

truncatedTdTextWorkaround :: Monad m => HtmlT m ()
truncatedTdTextWorkaround =
    style_ []
        $ toHtml @Text
            ".table { table-layout: fixed; }"

pageFromBodyH :: Monad m => Link -> PageConfig -> HtmlT m () -> HtmlT m ()
pageFromBodyH faviconLink PageConfig{..} body =
    html_ [term "data-bs-theme" "dark", class_ "p-1"]
        $ do
            head_ $ do
                title_ $ toHtml title
                meta_ [charset_ "utf-8"]
                meta_
                    [ name_ "viewport"
                    , content_ "width=device-width, initial-scale=1.0"
                    ]
                popperScript
                bootstrapLink
                bootstrapScript
                bootstrapIcons
                clipboardScript
                flatPickrScript
                favicon faviconLink
                useHtmxVersion (1, 9, 12)
                useHtmxExtension "json-enc"
                bodyCss
                modalCssWorkaround
                truncatedTdTextWorkaround
                offscreenCss
                flatPickrCSSDark
                monospaced
            body_ $ do
                fadeInId
                body

data PageConfig = PageConfig
    { prefix :: Text
    -- ^ Prefix to prepend to all links
    , title :: Text
    -- ^ Title of the page
    }
