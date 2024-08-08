{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.UI.API where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( WalletId (..)
    )
import Cardano.Wallet.UI.Cookies
    ( CookieRequest
    , Cookied
    )
import Cardano.Wallet.UI.Handlers.SSE
    ( SSE
    )
import Cardano.Wallet.UI.Html.Html
    ( HTML
    , RawHtml (..)
    )
import Data.Aeson
    ( Value
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText (..)
    )
import Network.HTTP.Media
    ( (//)
    )
import Servant
    ( Accept (..)
    , Capture
    , FromHttpApiData (..)
    , Get
    , JSON
    , Link
    , MimeRender (..)
    , Post
    , Proxy (..)
    , QueryParam
    , ReqBody
    , ToHttpApiData (..)
    , allLinks
    , linkURI
    , (:<|>) (..)
    , (:>)
    )

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

data Visible = Visible | Hidden

instance FromHttpApiData Visible where
    parseQueryParam "visible" = Right Visible
    parseQueryParam "hidden" = Right Hidden
    parseQueryParam _ = Left "Invalid value for visibility"

instance ToHttpApiData Visible where
    toQueryParam Visible = "visible"
    toQueryParam Hidden = "hidden"

instance ToHttpApiData WalletId where
    toQueryParam = toText

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

-- | Pages endpoints
type Pages =
    "about" :> SessionedHtml Get
        :<|> "network" :> SessionedHtml Get
        :<|> "wallet" :> SessionedHtml Get
        :<|> "wallets" :> SessionedHtml Get
        :<|> "addresses" :> SessionedHtml Get
        :<|> "settings" :> SessionedHtml Get

-- | Image mime type
data Image

instance Accept Image where
    contentType _ = "image" // "png"

instance MimeRender Image BL.ByteString where
    mimeRender _ = id

-- | Data endpoints
type Data =
    "network" :> "info" :> SessionedHtml Get
        :<|> "wallet" :> ReqBody '[JSON] Value :> SessionedHtml Post
        :<|> "wallet"
            :> "mnemonic"
            :> QueryParam "clean" Bool
            :> SessionedHtml Get
        :<|> "wallet"
            :> "post"
            :> "form"
            :> QueryParam "visible" Visible
            :> SessionedHtml Get
        :<|> "wallets" :> "list" :> SessionedHtml Get
        :<|> "wallet" :> SessionedHtml Get
        :<|> "wallet" :> "addresses" :> SessionedHtml Get
        :<|> "wallet" :> "delete" :> SessionedHtml Post
        :<|> "settings" :> SessionedHtml Get
        :<|> "settings" :> "sse" :> "toggle" :> SessionedHtml Post
        :<|> "wallets"
            :> "select"
            :> Capture "id" WalletId
            :> SessionedHtml Post
        :<|> "sse" :> (CookieRequest :> SSE)
        :<|> "favicon.ico" :> Get '[Image] BL.ByteString

type Home = SessionedHtml Get

-- | UI endpoints
type UI =
    Home
        :<|> "page"
            |> Pages
            |>> "data"
            |> Data

homePageLink :: Link
aboutPageLink :: Link
networkPageLink :: Link
walletPageLink :: Link
walletsPageLink :: Link
addressesPageLink :: Link
settingsPageLink :: Link
networkInfoLink :: Link
walletPostLink :: Link
walletMnemonicLink :: Maybe Bool -> Link
walletPostFormLink :: Maybe Visible -> Link
walletsListLink :: Link
walletLink :: Link
walletAddressesLink :: Link
walletDeleteLink :: Link
settingsGetLink :: Link
settingsSseToggleLink :: Link
settingsWalletSelectLink :: WalletId -> Link
sseLink :: Link
faviconLink :: Link

homePageLink
    :<|> aboutPageLink
    :<|> networkPageLink
    :<|> walletPageLink
    :<|> walletsPageLink
    :<|> addressesPageLink
    :<|> settingsPageLink
    :<|> networkInfoLink
    :<|> walletPostLink
    :<|> walletMnemonicLink
    :<|> walletPostFormLink
    :<|> walletsListLink
    :<|> walletLink
    :<|> walletAddressesLink
    :<|> walletDeleteLink
    :<|> settingsGetLink
    :<|> settingsSseToggleLink
    :<|> settingsWalletSelectLink
    :<|> sseLink
    :<|> faviconLink =
        allLinks (Proxy @UI)

linkText :: Link -> Text
linkText = T.pack . ('/' :) . show . linkURI
