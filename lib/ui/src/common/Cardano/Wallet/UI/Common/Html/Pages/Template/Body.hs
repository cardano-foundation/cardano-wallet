{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.UI.Common.Html.Pages.Template.Body
    ( bodyH
    )
where

import Prelude

import Cardano.Wallet.UI.Common.Html.Copy
    ( initClipboardScript
    )
import Cardano.Wallet.UI.Common.Html.Htmx
    ( hxSse_
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( linkText
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
    div_ [hxSse_ $ sseConnectFromLink sseLink] $ do
        header
        div_ [class_ "container-fluid p-0"] $ do
            div_ [class_ "main"] body
    initClipboardScript
