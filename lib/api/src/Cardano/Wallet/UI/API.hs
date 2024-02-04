{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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

type UI =
    SessionedHtml Get
        :<|> "about" :> SessionedHtml Get
        :<|> "network" :> SessionedHtml Get
        :<|> "wallet" :> "new" :> SessionedHtml Get
        :<|> "wallet" :> SessionedHtml Get
        :<|> "wallets" :> SessionedHtml Get
        :<|> "settings" :> SessionedHtml Get
        :<|> "network" :> "info" :> SessionedHtml Get
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
        :<|> "wallet" :> "get" :> SessionedHtml Get
        :<|> "settings" :> "get" :> SessionedHtml Get
        :<|> "settings" :> "sse" :> "toggle" :> SessionedHtml Post
        :<|> "settings"
            :> "wallet"
            :> "select"
            :> Capture "id" WalletId
            :> SessionedHtml Post
        :<|> "sse" :> CookieRequest :> SSE

homeLink :: Link
aboutLink :: Link
networkLink :: Link
walletNewLink :: Link
walletLink :: Link
walletsLink :: Link
settingsLink :: Link
networkInfoLink :: Link
walletPostLink :: Link
walletMnemonicLink :: Maybe Bool -> Link
walletPostFormLink :: Maybe Visible -> Link
walletsListLink :: Link
walletGetLink :: Link
settingsGetLink :: Link
settingsSseToggleLink :: Link
settingsWalletSelectLink :: WalletId -> Link
sseLink :: Link

homeLink
    :<|> aboutLink
    :<|> networkLink
    :<|> walletNewLink
    :<|> walletLink
    :<|> walletsLink
    :<|> settingsLink
    :<|> networkInfoLink
    :<|> walletPostLink
    :<|> walletMnemonicLink
    :<|> walletPostFormLink
    :<|> walletsListLink
    :<|> walletGetLink
    :<|> settingsGetLink
    :<|> settingsSseToggleLink
    :<|> settingsWalletSelectLink
    :<|> sseLink =
        allLinks (Proxy @UI)

linkText :: Link -> String
linkText = show .  linkURI
