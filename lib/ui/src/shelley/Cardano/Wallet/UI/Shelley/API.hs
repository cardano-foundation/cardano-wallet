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

module Cardano.Wallet.UI.Shelley.API where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( WalletId (..)
    )
import Cardano.Wallet.UI.Common.API
    ( Image
    , SessionedHtml
    , type (|>)
    , type (|>>)
    )
import Cardano.Wallet.UI.Common.Handlers.SSE
    ( SSE
    )
import Cardano.Wallet.UI.Cookies
    ( CookieRequest
    )
import Data.Aeson
    ( Value
    )
import Servant
    ( Capture
    , Get
    , JSON
    , Link
    , Post
    , Proxy (..)
    , QueryParam
    , ReqBody
    , allLinks
    , (:<|>) (..)
    , (:>)
    )

import qualified Data.ByteString.Lazy as BL

-- | Pages endpoints
type Pages =
    "about" :> SessionedHtml Get
        :<|> "network" :> SessionedHtml Get
        :<|> "wallet" :> SessionedHtml Get
        :<|> "wallets" :> SessionedHtml Get
        :<|> "addresses" :> SessionedHtml Get
        :<|> "settings" :> SessionedHtml Get
-- | Data endpoints
type Data =
    "network" :> "info" :> SessionedHtml Get
        :<|> "wallet" :> ReqBody '[JSON] Value :> SessionedHtml Post
        :<|> "wallet"
            :> "mnemonic"
            :> QueryParam "clean" Bool
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
