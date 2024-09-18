module Cardano.Wallet.UI.Deposit.Html.Pages.About where

import Prelude

import Lucid
    ( HtmlT
    , p_
    )

aboutH :: Monad m => HtmlT m ()
aboutH = do
    p_
        "This is the new builtin Cardano Deposit Wallet web UI, pre-alpha version"
