{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.UI.Common.Html.Pages.Template.Body
    ( bodyH
    )
where

import Prelude

import Cardano.Wallet.UI.Common.Html.Pages.Template.Footer
    ( footerH
    )
import Lucid
    ( HtmlT
    , class_
    , div_
    )

-- | The body of a page.
bodyH
    :: Monad m
    => HtmlT m ()
    -- ^ Header
    -> HtmlT m ()
    -- ^ Body content
    -> HtmlT m ()
bodyH header body = do
    header
    div_ [class_ "container-fluid"] $ do
        div_ [class_ "main"] body
        div_
            [class_ "footer"]
            footerH
