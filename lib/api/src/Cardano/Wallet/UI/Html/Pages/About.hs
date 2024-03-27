module Cardano.Wallet.UI.Html.Pages.About where

import Lucid
    ( Html
    , p_
    )

aboutH :: Html ()
aboutH = do
    p_
        "This is the new builtin Cardano Wallet web UI, pre-alpha version"
