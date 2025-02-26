{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.UI.Deposit.API.Addresses.Transactions
    ( TransactionHistoryParams (..)
    )
where

import Prelude

import Cardano.Wallet.Deposit.Pure
    ( Customer
    )
import Cardano.Wallet.UI.Lib.Time.Direction
    ( Direction (..)
    )
import Data.Maybe
    ( isJust
    )
import Servant
    ( FromHttpApiData (..)
    )
import Web.FormUrlEncoded
    ( FromForm (..)
    , lookupMaybe
    , parseUnique
    )

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
