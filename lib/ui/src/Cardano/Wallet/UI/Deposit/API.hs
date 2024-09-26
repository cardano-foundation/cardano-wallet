{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.UI.Deposit.API where

import Prelude

import Cardano.Wallet.Deposit.Pure
    ( Customer
    )
import Cardano.Wallet.Deposit.REST.Wallet.Create
    ( PostWalletViaMenmonic
    , PostWalletViaXPub
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
import Control.Lens
    ( makePrisms
    )
import Servant
    ( Delete
    , FormUrlEncoded
    , FromHttpApiData (..)
    , Get
    , Link
    , Post
    , Proxy (..)
    , QueryParam
    , ReqBody
    , ToHttpApiData (..)
    , allLinks
    , (:<|>) (..)
    , (:>)
    )
import Web.FormUrlEncoded
    ( FromForm (..)
    , parseUnique
    )

import qualified Data.ByteString.Lazy as BL

instance FromForm PostWalletViaMenmonic

instance FromForm PostWalletViaXPub

data Page
    = About
    | Network
    | Settings
    | Wallet
    | Addresses

makePrisms ''Page

instance ToHttpApiData Page where
    toUrlPiece About = "about"
    toUrlPiece Network = "network"
    toUrlPiece Settings = "settings"
    toUrlPiece Wallet = "wallet"
    toUrlPiece Addresses = "addresses"

instance FromHttpApiData Page where
    parseUrlPiece "about" = Right About
    parseUrlPiece "network" = Right Network
    parseUrlPiece "settings" = Right Settings
    parseUrlPiece "wallet" = Right Wallet
    parseUrlPiece "addresses" = Right Addresses
    parseUrlPiece _ = Left "Invalid page"

-- | Pages endpoints
type Pages =
    "about" :> SessionedHtml Get
        :<|> "network" :> SessionedHtml Get
        :<|> "settings" :> SessionedHtml Get
        :<|> "wallet" :> SessionedHtml Get
        :<|> "addresses" :> SessionedHtml Get

-- | Data endpoints
type Data =
    "network" :> "info" :> SessionedHtml Get
        :<|> "settings" :> SessionedHtml Get
        :<|> "settings" :> "sse" :> "toggle" :> SessionedHtml Post
        :<|> "sse" :> (CookieRequest :> SSE)
        :<|> "favicon.ico" :> Get '[Image] BL.ByteString
        :<|> "wallet"
            :> "mnemonic"
            :> QueryParam "clean" Bool
            :> SessionedHtml Get
        :<|> "wallet" :> SessionedHtml Get
        :<|> "wallet"
            :> "mnemonic"
            :> ReqBody '[FormUrlEncoded] PostWalletViaMenmonic
            :> SessionedHtml Post
        :<|> "wallet"
            :> "xpub"
            :> ReqBody '[FormUrlEncoded] PostWalletViaXPub
            :> SessionedHtml Post
        :<|> "wallet" :> SessionedHtml Delete
        :<|> "wallet" :> "delete" :> "modal" :> SessionedHtml Get
        :<|> "customer"
            :> "address"
            :> ReqBody '[FormUrlEncoded] Customer
            :> SessionedHtml Post
        :<|> "addresses" :> SessionedHtml Get
        :<|> "navigation" :> QueryParam "page" Page :> SessionedHtml Get

instance FromForm Customer where
    fromForm form = fromIntegral @Int <$> parseUnique "customer" form

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
settingsPageLink :: Link
addressesPageLink :: Link
networkInfoLink :: Link
settingsGetLink :: Link
settingsSseToggleLink :: Link
sseLink :: Link
faviconLink :: Link
walletMnemonicLink :: Maybe Bool -> Link
walletPageLink :: Link
walletLink :: Link
walletPostMnemonicLink :: Link
walletPostXPubLink :: Link
walletDeleteLink :: Link
walletDeleteModalLink :: Link
customerAddressLink :: Link
addressesLink :: Link
navigationLink :: Maybe Page -> Link
homePageLink
    :<|> aboutPageLink
    :<|> networkPageLink
    :<|> settingsPageLink
    :<|> walletPageLink
    :<|> addressesPageLink
    :<|> networkInfoLink
    :<|> settingsGetLink
    :<|> settingsSseToggleLink
    :<|> sseLink
    :<|> faviconLink
    :<|> walletMnemonicLink
    :<|> walletLink
    :<|> walletPostMnemonicLink
    :<|> walletPostXPubLink
    :<|> walletDeleteLink
    :<|> walletDeleteModalLink
    :<|> customerAddressLink
    :<|> addressesLink
    :<|> navigationLink =
        allLinks (Proxy @UI)
