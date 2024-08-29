module Cardano.Wallet.UI.Deposit.Html.Pages.About where

import Lucid
    ( Html
    , p_
    )

aboutH :: Html ()
aboutH = do
    p_
        "This is the new builtin Cardano Deposit Wallet web UI, pre-alpha version"
