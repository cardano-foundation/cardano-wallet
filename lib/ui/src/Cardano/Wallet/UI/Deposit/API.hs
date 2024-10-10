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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Wallet.UI.Deposit.API where

import Prelude

import Cardano.Wallet.Deposit.Pure
    ( Customer
    )
import Cardano.Wallet.Deposit.Read
    ( WithOrigin (..)
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
import Cardano.Wallet.UI.Lib.TimeWindow
    ( Direction (..)
    )
import Control.Lens
    ( makePrisms
    )
import Data.Hashable
    ( Hashable
    )
import Data.Maybe
    ( isJust
    )
import Data.Text.Class
    ( ToText (..)
    )
import Data.Time
    ( DayOfWeek
    , UTCTime
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
    , lookupMaybe
    , parseMaybe
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
    | Deposits

makePrisms ''Page

instance ToHttpApiData Page where
    toUrlPiece About = "about"
    toUrlPiece Network = "network"
    toUrlPiece Settings = "settings"
    toUrlPiece Wallet = "wallet"
    toUrlPiece Addresses = "addresses"
    toUrlPiece Deposits = "deposits"

instance FromHttpApiData Page where
    parseUrlPiece "about" = Right About
    parseUrlPiece "network" = Right Network
    parseUrlPiece "settings" = Right Settings
    parseUrlPiece "wallet" = Right Wallet
    parseUrlPiece "addresses" = Right Addresses
    parseUrlPiece "deposits" = Right Deposits
    parseUrlPiece _ = Left "Invalid page"

-- | Pages endpoints
type Pages =
    "about" :> SessionedHtml Get
        :<|> "network" :> SessionedHtml Get
        :<|> "settings" :> SessionedHtml Get
        :<|> "wallet" :> SessionedHtml Get
        :<|> "addresses" :> SessionedHtml Get
        :<|> "deposits" :> SessionedHtml Get

-- | Data endpoints
type Data =
    "network" :> "info" :> SessionedHtml Get
        :<|> "settings" :> SessionedHtml Get
        :<|> "settings" :> "sse" :> "toggle" :> SessionedHtml Post
        :<|> "sse" :> (CookieRequest :> SSE)
        :<|> "favicon.ico" :> Get '[Image] BL.ByteString
        :<|> "images"
            :> "fake-data.png"
            :> Get '[Image] BL.ByteString
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
        :<|> "transactions" :> SessionedHtml Get
        :<|> "customer"
            :> "transactions"
            :> "history"
            :> ReqBody '[FormUrlEncoded] TransactionHistoryParams
            :> SessionedHtml Post
        :<|> "deposits" :> SessionedHtml Get
        :<|> "deposits"
            :> "history"
            :> ReqBody '[FormUrlEncoded] DepositsParams
            :> SessionedHtml Post
        :<|> "deposits"
            :> "history"
            :> "extend"
            :> ReqBody '[FormUrlEncoded] DepositsParams
            :> SessionedHtml Post
        :<|> "deposits"
            :> "history"
            :> "window"
            :> ReqBody '[FormUrlEncoded] DepositsParams
            :> SessionedHtml Post
        :<|> "emptiness" :> SessionedHtml Get

instance FromHttpApiData Direction where
    parseUrlPiece "asc" = Right Asc
    parseUrlPiece "desc" = Right Desc
    parseUrlPiece _ = Left "Invalid sorting direction"

data TransactionHistoryParams = TransactionHistoryParams
    { txHistoryCustomer :: Customer
    , txHistoryUTC :: Bool
    , txHistorySlot :: Bool
    , txHistorySpent :: Bool
    , txHistoryReceived :: Bool
    , txHistorySorting :: Direction
    , txHistoryStartYear :: Int
    , txHistoryStartMonth :: Int
    }

instance FromForm Customer where
    fromForm form = fromIntegral @Int <$> parseUnique "customer" form

instance FromForm TransactionHistoryParams where
    fromForm form = do
        utc <- isJust <$> lookupMaybe "utc" form
        customer <- fromIntegral @Int <$> parseUnique "customer" form
        slot <- isJust <$> lookupMaybe "slot" form
        spent <- isJust <$> lookupMaybe "spent" form
        received <- isJust <$> lookupMaybe "received" form
        sorting <- parseUnique "sorting" form
        year <- parseUnique "start-year" form
        month <- parseUnique "start-month" form
        pure
            $ TransactionHistoryParams
                customer
                utc
                slot
                spent
                received
                sorting
                year
                month

data Window
    = Minute5
    | Minute10
    | Minute15
    | Minute30
    | Hour1
    | Hour2
    | Hour4
    | Hour6
    | Hour12
    | Day
    | Week
    | Month
    | Year
    deriving (Eq, Show, Enum, Bounded)

instance ToText Window where
    toText Minute5 = "5 minutes"
    toText Minute10 = "10 minutes"
    toText Minute15 = "15 minutes"
    toText Minute30 = "30 minutes"
    toText Hour1 = "1 hour"
    toText Hour2 = "2 hours"
    toText Hour4 = "4 hours"
    toText Hour6 = "6 hours"
    toText Hour12 = "12 hours"
    toText Day = "1 day"
    toText Week = "1 week"
    toText Month = "1 month"
    toText Year = "1 year"

instance FromHttpApiData Window where
    parseUrlPiece "5m" = Right Minute5
    parseUrlPiece "10m" = Right Minute10
    parseUrlPiece "15m" = Right Minute15
    parseUrlPiece "30m" = Right Minute30
    parseUrlPiece "1h" = Right Hour1
    parseUrlPiece "2h" = Right Hour2
    parseUrlPiece "4h" = Right Hour4
    parseUrlPiece "6h" = Right Hour6
    parseUrlPiece "12h" = Right Hour12
    parseUrlPiece "1d" = Right Day
    parseUrlPiece "1w" = Right Week
    parseUrlPiece "1M" = Right Month
    parseUrlPiece "1y" = Right Year
    parseUrlPiece _ = Left "Invalid time window"

instance ToHttpApiData Window where
    toUrlPiece Minute5 = "5m"
    toUrlPiece Minute10 = "10m"
    toUrlPiece Minute15 = "15m"
    toUrlPiece Minute30 = "30m"
    toUrlPiece Hour1 = "1h"
    toUrlPiece Hour2 = "2h"
    toUrlPiece Hour4 = "4h"
    toUrlPiece Hour6 = "6h"
    toUrlPiece Hour12 = "12h"
    toUrlPiece Day = "1d"
    toUrlPiece Week = "1w"
    toUrlPiece Month = "1M"
    toUrlPiece Year = "1y"

data DepositsParams = DepositsParams
    { depositsSlot :: Bool
    , depositsWindow :: Window
    , depositsFirstWeekDay :: DayOfWeek
    , depositsFakeData :: Bool
    , depositsViewStart :: Maybe (WithOrigin UTCTime)
    , depositsWindowOpen :: Maybe (WithOrigin UTCTime)
    }
    deriving (Eq, Show)

instance FromHttpApiData (WithOrigin UTCTime) where
    parseUrlPiece "Origin" = pure Origin
    parseUrlPiece t = At <$> parseUrlPiece t

instance ToHttpApiData (WithOrigin UTCTime) where
    toUrlPiece Origin = "Origin"
    toUrlPiece (At t) = toUrlPiece t
instance FromForm DepositsParams where
    fromForm form = do
        slot <- isJust <$> lookupMaybe "slot" form
        window <- parseUnique "window" form
        firstWeekDay <- parseUnique "first-week-day" form
        fake <- isJust <$> lookupMaybe "fake-data" form
        viewStart <- parseMaybe "view-start" form
        windowOpen <- parseMaybe "window-open" form
        pure $ DepositsParams slot window firstWeekDay fake viewStart windowOpen

type Home = SessionedHtml Get

deriving instance Hashable (WithOrigin UTCTime)

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
depositPageLink :: Link
networkInfoLink :: Link
settingsGetLink :: Link
settingsSseToggleLink :: Link
sseLink :: Link
faviconLink :: Link
fakeDataBackgroundLink :: Link
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
transactionsLink :: Link
customerHistoryLink :: Link
depositsLink :: Link
depositsHistoryLink :: Link
depositsHistoryExtendLink :: Link
depositsHistoryWindowLink :: Link
emptinessLink :: Link
homePageLink
    :<|> aboutPageLink
    :<|> networkPageLink
    :<|> settingsPageLink
    :<|> walletPageLink
    :<|> addressesPageLink
    :<|> depositPageLink
    :<|> networkInfoLink
    :<|> settingsGetLink
    :<|> settingsSseToggleLink
    :<|> sseLink
    :<|> faviconLink
    :<|> fakeDataBackgroundLink
    :<|> walletMnemonicLink
    :<|> walletLink
    :<|> walletPostMnemonicLink
    :<|> walletPostXPubLink
    :<|> walletDeleteLink
    :<|> walletDeleteModalLink
    :<|> customerAddressLink
    :<|> addressesLink
    :<|> navigationLink
    :<|> transactionsLink
    :<|> customerHistoryLink
    :<|> depositsLink
    :<|> depositsHistoryLink
    :<|> depositsHistoryExtendLink
    :<|> depositsHistoryWindowLink
    :<|> emptinessLink
    =
        allLinks (Proxy @UI)
