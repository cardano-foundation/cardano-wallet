module Cardano.Wallet.UI.Html.Pages.Template.Head
    ( pageFromBodyH
    )
where

import Prelude

import Cardano.Wallet.UI.Html.Htmx
    ( useHtmx
    )
import Lucid
    ( Html
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

pageFromBodyH :: Html () -> Html ()
pageFromBodyH body = html_ [term "data-bs-theme" "dark"]
    $ do
        head_ $ do
            title_ "Cardano Wallet UI"
            meta_ [charset_ "utf-8"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
            bootstrapLink
            bootstrapScript
            useHtmx
        body_ body
