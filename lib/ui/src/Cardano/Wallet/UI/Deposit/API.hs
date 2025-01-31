{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.UI.Deposit.API where

import Prelude

import Cardano.Wallet.Deposit.Pure
    ( Customer
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    , TxId
    )
import Cardano.Wallet.Deposit.REST.Wallet.Create
    ( PostWalletViaMnemonic
    , PostWalletViaXPub
    )
import Cardano.Wallet.Read
    ( WithOrigin (..)
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
import Cardano.Wallet.UI.Deposit.API.Addresses.Transactions
    ( TransactionHistoryParams
    )
import Cardano.Wallet.UI.Deposit.API.Common
    ( Expand
    )
import Cardano.Wallet.UI.Deposit.API.Deposits.Deposits
    ( DepositsParams
    )
import Cardano.Wallet.UI.Deposit.API.Payments
    ( AddReceiverForm
    , NewReceiverValidation
    , SignatureForm
    )
import Control.Lens
    ( makePrisms
    )
import Data.Text
    ( Text
    )
import Data.Time
    ( UTCTime
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
    )

import qualified Cardano.Wallet.UI.Deposit.API.Payments as Payment
import qualified Data.ByteString.Lazy as BL

instance FromForm PostWalletViaMnemonic

instance FromForm PostWalletViaXPub

data Page
    = About
    | Settings
    | Wallet
    | Addresses
    | Deposits
    | Payments

makePrisms ''Page

instance ToHttpApiData Page where
    toUrlPiece About = "about"
    toUrlPiece Settings = "settings"
    toUrlPiece Wallet = "wallet"
    toUrlPiece Addresses = "addresses"
    toUrlPiece Deposits = "deposits"
    toUrlPiece Payments = "payments"

instance FromHttpApiData Page where
    parseUrlPiece "about" = Right About
    parseUrlPiece "settings" = Right Settings
    parseUrlPiece "wallet" = Right Wallet
    parseUrlPiece "addresses" = Right Addresses
    parseUrlPiece "deposits" = Right Deposits
    parseUrlPiece "payments" = Right Payments
    parseUrlPiece _ = Left "Invalid page"

-- | Pages endpoints
type Pages =
    "about" :> SessionedHtml Get
        :<|> "settings" :> SessionedHtml Get
        :<|> "wallet" :> SessionedHtml Get
        :<|> "addresses" :> SessionedHtml Get
        :<|> "deposits" :> SessionedHtml Get
        :<|> "payments" :> SessionedHtml Get

-- | Data endpoints
type Data =
    "settings" :> SessionedHtml Get
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
            :> ReqBody '[FormUrlEncoded] PostWalletViaMnemonic
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
        :<|> "customer"
            :> "transactions"
            :> "history"
            :> ReqBody '[FormUrlEncoded] TransactionHistoryParams
            :> SessionedHtml Post
        :<|> "deposits" :> SessionedHtml Get
        :<|> "deposits"
            :> "times"
            :> ReqBody '[FormUrlEncoded] DepositsParams
            :> SessionedHtml Post
        :<|> "deposits"
            :> "times"
            :> "page"
            :> ReqBody '[FormUrlEncoded] DepositsParams
            :> QueryParam "index" (WithOrigin UTCTime)
            :> SessionedHtml Post
        :<|> "deposits"
            :> "history"
            :> "customers"
            :> ReqBody '[FormUrlEncoded] DepositsParams
            :> QueryParam "time" (WithOrigin UTCTime)
            :> QueryParam "expand" Expand
            :> SessionedHtml Post
        :<|> "deposits"
            :> "history"
            :> "customers"
            :> "page"
            :> ReqBody '[FormUrlEncoded] DepositsParams
            :> QueryParam "time" (WithOrigin UTCTime)
            :> QueryParam "customer" Customer
            :> SessionedHtml Post
        :<|> "deposits"
            :> "history"
            :> "customers"
            :> "tx-ids"
            :> ReqBody '[FormUrlEncoded] DepositsParams
            :> QueryParam "time" (WithOrigin UTCTime)
            :> QueryParam "customer" Customer
            :> QueryParam "expand" Expand
            :> SessionedHtml Post
        :<|> "deposits"
            :> "history"
            :> "customers"
            :> "tx-ids"
            :> "page"
            :> ReqBody '[FormUrlEncoded] DepositsParams
            :> QueryParam "time" (WithOrigin UTCTime)
            :> QueryParam "customer" Customer
            :> QueryParam "tx-id" TxId
            :> SessionedHtml Post
        :<|> "payments" :> SessionedHtml Get
        :<|> "payments"
            :> "receiver"
            :> ReqBody '[FormUrlEncoded] AddReceiverForm
            :> SessionedHtml Post
        :<|> "payments"
            :> "receiver"
            :> "delete"
            :> ReqBody '[FormUrlEncoded] Payment.State
            :> QueryParam "receiver-number" Address
            :> SessionedHtml Post
        :<|> "payments"
            :> "balance"
            :> "available"
            :> SessionedHtml Get
        :<|> "payments"
            :> "receiver"
            :> "address"
            :> "validation"
            :> ReqBody '[FormUrlEncoded] NewReceiverValidation
            :> SessionedHtml Post
        :<|> "payments"
            :> "receiver"
            :> "amount"
            :> "validation"
            :> ReqBody '[FormUrlEncoded] NewReceiverValidation
            :> SessionedHtml Post
        :<|> "modal"
            :> "info"
            :> QueryParam "title" Text
            :> QueryParam "text" Text
            :> SessionedHtml Get
        :<|> "payments"
            :> "sign"
            :> ReqBody '[FormUrlEncoded] SignatureForm
            :> SessionedHtml Post
        :<|> "payments"
            :> "submit"
            :> ReqBody '[FormUrlEncoded] Payment.State
            :> SessionedHtml Post
        :<|> "payments"
            :> "reset"
            :> SessionedHtml Post
        :<|> "wallet"
            :> "status"
            :> SessionedHtml Get

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
settingsPageLink :: Link
walletPageLink :: Link
addressesPageLink :: Link
depositPageLink :: Link
paymentsPageLink :: Link
settingsGetLink :: Link
settingsSseToggleLink :: Link
sseLink :: Link
faviconLink :: Link
walletMnemonicLink :: Maybe Bool -> Link
walletLink :: Link
walletPostMnemonicLink :: Link
walletPostXPubLink :: Link
walletDeleteLink :: Link
walletDeleteModalLink :: Link
customerAddressLink :: Link
addressesLink :: Link
navigationLink :: Maybe Page -> Link
customerHistoryLink :: Link
depositsLink :: Link
depositsTimesLink :: Link
depositsTimesPaginatingLink
    :: Maybe (WithOrigin UTCTime) -> Link
depositsCustomersLink
    :: Maybe (WithOrigin UTCTime) -> Maybe Expand -> Link
depositsCustomersPaginatingLink
    :: Maybe (WithOrigin UTCTime) -> Maybe Customer -> Link
depositsTxIdsLink
    :: Maybe (WithOrigin UTCTime) -> Maybe Customer -> Maybe Expand -> Link
depositsTxIdsPaginatingLink
    :: Maybe (WithOrigin UTCTime) -> Maybe Customer -> Maybe TxId -> Link
paymentsLink :: Link
paymentsNewReceiverLink :: Link
paymentsDeleteReceiverLink :: Maybe Address -> Link
paymentsBalanceAvailableLink :: Link
paymentsReceiverAddressValidationLink :: Link
paymentsReceiverAmountValidationLink :: Link
modalLink :: Maybe Text -> Maybe Text -> Link
paymentsSignLink :: Link
paymentsSubmitLink :: Link
paymentsResetLink :: Link
walletStatusLink :: Link
homePageLink
    :<|> aboutPageLink
    :<|> settingsPageLink
    :<|> walletPageLink
    :<|> addressesPageLink
    :<|> depositPageLink
    :<|> paymentsPageLink
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
    :<|> navigationLink
    :<|> customerHistoryLink
    :<|> depositsLink
    :<|> depositsTimesLink
    :<|> depositsTimesPaginatingLink
    :<|> depositsCustomersLink
    :<|> depositsCustomersPaginatingLink
    :<|> depositsTxIdsLink
    :<|> depositsTxIdsPaginatingLink
    :<|> paymentsLink
    :<|> paymentsNewReceiverLink
    :<|> paymentsDeleteReceiverLink
    :<|> paymentsBalanceAvailableLink
    :<|> paymentsReceiverAddressValidationLink
    :<|> paymentsReceiverAmountValidationLink
    :<|> modalLink
    :<|> paymentsSignLink
    :<|> paymentsSubmitLink
    :<|> paymentsResetLink
    :<|> walletStatusLink =
        allLinks (Proxy @UI)
