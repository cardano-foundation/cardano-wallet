module Cardano.Wallet.UI.Shelley.Html.Pages.About where

import Prelude

import Lucid
    ( HtmlT
    , p_
    )

aboutH :: Monad m => HtmlT m ()
aboutH = do
    p_
        "This is the new builtin Cardano Wallet web UI, pre-alpha version"
