{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.UI.Common.Html.Pages.Template.Body
    ( bodyH
    )
where

import Prelude

import Cardano.Wallet.UI.Common.Html.Htmx
    ( hxSse_
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( linkText
    )
import Cardano.Wallet.UI.Common.Html.Pages.Template.Footer
    ( footerH
    )
import Data.Text
    ( Text
    )
import Lucid
    ( HtmlT
    , class_
    , div_
    )
import Servant
    ( Link
    )
-- | A value attribute to use to connect to an SSE endpoint.
sseConnectFromLink :: Link -> Text
sseConnectFromLink sse = "connect:" <> linkText sse

-- | The body of a page.
bodyH
    :: Monad m
    => Link
    -> HtmlT m ()
    -- ^ Header
    -> HtmlT m ()
    -- ^ Body content
    -> HtmlT m ()
bodyH sseLink header body = do
    header
    div_ [hxSse_ $ sseConnectFromLink sseLink] $
        div_ [class_ "container-fluid"] $ do
            div_ [class_ "main"] body
            div_
                [class_ "footer mt-5"]
                footerH
