module Cardano.Wallet.UI.Common.Html.Pages.Template.Body
    ( bodyH
    )
where

import Prelude

import Cardano.Wallet.UI.Common.Html.Pages.Template.Footer
    ( footerH
    )
import Lucid
    ( Html
    , class_
    , div_
    )

bodyH
    :: Html ()
    -- ^ Header
    -> Html ()
    -- ^ Body content
    -> Html ()
bodyH header body = do
    header
    div_ [class_ "container-fluid"] $ do
        div_ [class_ "main"] body
        div_
            [class_ "footer"]
            footerH
