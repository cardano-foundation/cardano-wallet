{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.UI.Common.Html.Pages.Template.Head
    ( pageFromBodyH
    , PageConfig (..)
    )
where

import Prelude

import Cardano.Wallet.UI.Common.Html.Htmx
    ( useHtmxExtension
    , useHtmxVersion
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( linkText
    )
import Data.Text
    ( Text
    )
import Lucid
    ( HtmlT
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

-- | Render a favicon link.
favicon :: Link -> Monad m => HtmlT m ()
favicon path =
    link_
        [ rel_ "icon"
        , href_ $ linkText path
        ]

pageFromBodyH :: Monad m => Link -> PageConfig ->  HtmlT m () -> HtmlT m ()
pageFromBodyH faviconLink PageConfig{..} body =
    html_ [term "data-bs-theme" "dark"]
        $ do
            head_ $ do
                title_ $ toHtml title
                meta_ [charset_ "utf-8"]
                meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
                popperScript
                bootstrapLink
                bootstrapScript
                bootstrapIcons
                favicon faviconLink
                useHtmxVersion (1, 9, 12)
                useHtmxExtension "json-enc"
            body_ body

data PageConfig = PageConfig
    { prefix :: Text
    -- ^ Prefix to prepend to all links
    , title :: Text
    -- ^ Title of the page
    }
