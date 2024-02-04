{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
import qualified Data.Text as T
import Data.Text.Class
    ( ToText (..)
    )
import Servant
    ( Capture
    , FromHttpApiData (..)
    , Get
    , JSON
    , Link
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

type family f |> xs where
    q |> (f :<|> g) = (q :> f) :<|> q |> g
    q |> f = q :> f

infixr 4 |>>

type family xs |>> ys where
    (x :<|> xs) |>> ys = x :<|> xs |>> ys
    x |>> ys = x :<|> ys

type Pages =
        "about" :> SessionedHtml Get
        :<|> "network" :> SessionedHtml Get
        :<|> "wallet" :> SessionedHtml Get
        :<|> "wallets" :> SessionedHtml Get
        :<|> "addresses" :> SessionedHtml Get
        :<|> "settings" :> SessionedHtml Get

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
        :<|> "settings"
            :> "wallet"
            :> "select"
            :> Capture "id" WalletId
            :> SessionedHtml Post
        :<|> "sse" :> (CookieRequest :> SSE)

type Home = SessionedHtml Get

type UI' = Home :<|> "page" |> Pages |>> "data" |> Data

type UI = "ui" |> UI'

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
-- walletsListLink = undefined
-- walletLink = undefined
-- walletAddressesLink = undefined
-- walletDeleteLink = undefined
-- settingsGetLink = undefined
-- settingsSseToggleLink = undefined
-- settingsWalletSelectLink = undefined
-- sseLink = undefined

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
    =
        allLinks (Proxy @UI)

linkText :: Link -> Text
linkText = T.pack . ('/' :) .  show . linkURI
