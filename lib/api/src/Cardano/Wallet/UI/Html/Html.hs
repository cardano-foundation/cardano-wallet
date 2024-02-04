{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.Wallet.UI.Html.Html where

import Prelude

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

import qualified Data.ByteString.Lazy as BL

data HTML = HTML

newtype RawHtml = RawHtml {unRaw :: BL.ByteString}

instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
    mimeRender _ = unRaw

renderHtml :: Html () -> RawHtml
renderHtml = RawHtml . renderBS
