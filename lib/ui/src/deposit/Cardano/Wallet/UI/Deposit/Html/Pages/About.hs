module Cardano.Wallet.UI.Deposit.Html.Pages.About where

import Prelude

import Cardano.Wallet.UI.Common.Html.Pages.Template.Footer
    ( footerH
    )
import Lucid
    ( HtmlT
    , p_
    )

aboutH :: Monad m => HtmlT m ()
aboutH = do
    p_ "Cardano Deposit Wallet web UI, pre-alpha version"
    footerH
