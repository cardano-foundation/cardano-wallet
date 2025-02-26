{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.UI.Common.API where

import Prelude

import Cardano.Wallet.UI.Common.Html.Html
    ( HTML
    , RawHtml (..)
    )
import Cardano.Wallet.UI.Cookies
    ( Cookied
    )
import Network.HTTP.Media
    ( (//)
    )
import Servant
    ( Accept (..)
    , FromHttpApiData (..)
    , MimeRender (..)
    , ToHttpApiData (..)
    , (:<|>) (..)
    , (:>)
    )

import qualified Data.ByteString.Lazy as BL

data Visible = Visible | Hidden

instance FromHttpApiData Visible where
    parseQueryParam "visible" = Right Visible
    parseQueryParam "hidden" = Right Hidden
    parseQueryParam _ = Left "Invalid value for visibility"

instance ToHttpApiData Visible where
    toQueryParam Visible = "visible"
    toQueryParam Hidden = "hidden"

type SessionedHtml b = Cookied (b '[HTML]) RawHtml

infixr 5 |>

-- | Prepend a path segment to every path
type family f |> xs where
    q |> (f :<|> g) = (q :> f) :<|> q |> g
    q |> f = q :> f

infixr 4 |>>

-- | Append a paths
type family xs |>> ys where
    (x :<|> xs) |>> ys = x :<|> xs |>> ys
    x |>> ys = x :<|> ys

-- | Image mime type
data Image

instance Accept Image where
    contentType _ = "image" // "png"

instance MimeRender Image BL.ByteString where
    mimeRender _ = id
