module Cardano.Wallet.UI.Shelley.Html.Pages.About where

import Lucid
    ( HtmlT
    , p_
    )
import Prelude

aboutH :: Monad m => HtmlT m ()
aboutH = do
    p_
        "This is the new builtin Cardano Wallet web UI, pre-alpha version"
