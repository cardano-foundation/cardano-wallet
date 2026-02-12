{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.Wallet.UI.Common.Html.Html where

import Lucid
    ( Html
    , renderBS
    )
import Network.HTTP.Media
    ( (//)
    , (/:)
    )
import Servant
    ( Accept (contentType)
    , MimeRender (..)
    )
import Prelude

import qualified Data.ByteString.Lazy as BL

-- | HTML content type for servant endpoints.
data HTML

-- | Raw HTML content.
newtype RawHtml = RawHtml {unRaw :: BL.ByteString}

instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
    mimeRender _ = unRaw

-- | Render lucid HTML content to raw HTML to be served by servant.
renderHtml :: Html () -> RawHtml
renderHtml = RawHtml . renderBS
