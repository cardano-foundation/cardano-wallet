module Cardano.Wallet.UI.Html.Pages.Template.Footer
    ( footerH
    )
where

import Prelude

import Lucid
    ( Html
    , a_
    , class_
    , div_
    , href_
    , li_
    , span_
    , term
    , ul_
    )

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
