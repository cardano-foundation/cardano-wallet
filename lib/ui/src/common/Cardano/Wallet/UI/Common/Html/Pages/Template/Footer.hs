module Cardano.Wallet.UI.Common.Html.Pages.Template.Footer
    ( footerH
    )
where

import Prelude

import Lucid
    ( HtmlT
    , a_
    , class_
    , div_
    , href_
    , li_
    , span_
    , term
    , ul_
    )

-- | A link to the GitHub repository of the project.
githubLinkH :: Monad m => HtmlT m ()
githubLinkH =
    a_
        [href_ "https://github.com/cardano-foundation/cardano-wallet"]
        "GitHub"

-- | The footer of a page.
footerH :: Monad m => HtmlT m ()
footerH =
    term
        "footer_"
        [ class_
            "text-center text-muted bg-secondary fs-6"
        ]
        $ do
            div_ [class_ "row d-md-flex align-items-center"]
                $ do
                    ul_ [class_ "nav flex-column"] $ do
                        li_
                            [class_ "nav-item"]
                            "© 2024 Cardano Foundation, HAL team"
                        li_ [class_ "nav-item"] $ do
                            span_ "Source code on "
                            githubLinkH

            div_ [class_ "row d-md-flex align-items-center"]
                $ div_
                    [class_ "mb-md-0 text-body-secondary"]
                    "Powered by Haskell, Htmx, Servant, Lucid, Bootstrap"
