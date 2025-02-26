{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.UI.Deposit.API.Deposits.Deposits where

import Prelude

import Cardano.Wallet.Deposit.Pure
    ( Customer
    )
import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( DownTime
    )
import Cardano.Wallet.Deposit.Read
    ( Slot
    , TxId
    )
import Cardano.Wallet.Read
    ( WithOrigin (..)
    )
import Cardano.Wallet.UI.Deposit.API.Common
    ()
import Cardano.Wallet.UI.Lib.Discretization
    ( Window (..)
    )
import Data.Maybe
    ( isJust
    )
import Data.Ord
    ( Down (..)
    )
import Data.Set
    ( Set
    )
import Data.Time
    ( DayOfWeek
    , UTCTime
    )
import Servant
    ( FromHttpApiData (..)
    , ToHttpApiData (..)
    )
import Web.FormUrlEncoded
    ( FromForm (..)
    , lookupMaybe
    , parseAll
    , parseMaybe
    , parseUnique
    )

import qualified Data.Set as Set
import qualified Data.Text as T

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
    , depositsViewStart :: Maybe (WithOrigin UTCTime)
    , depositsWindowOpen :: Maybe (WithOrigin UTCTime)
    , depositsSpent :: Bool
    , depositsCustomers :: Set (WithOrigin UTCTime)
    , depositsPages :: Set DownTime
    , depositsCustomersPages :: Set Customer
    , depositsCustomersTxIdsPages :: Set TxId
    }
    deriving (Eq, Show)

instance FromHttpApiData (Customer, Slot) where
    parseUrlPiece t = do
        case T.splitOn "-" t of
            [c, s] -> (,) <$> parseUrlPiece c <*> parseUrlPiece s
            _ -> Left "Invalid customer/slot pair"

instance FromForm DepositsParams where
    fromForm form = do
        slot <- isJust <$> lookupMaybe "slot" form
        window <- parseUnique "window" form
        firstWeekDay <- parseUnique "first-week-day" form
        viewStart <- parseMaybe "view-start" form
        windowOpen <- parseMaybe "window-open" form
        spent <- isJust <$> lookupMaybe "spent" form
        customers <- Set.fromList <$> parseAll "customers" form
        pageTimes <- Set.fromList . fmap Down <$>
            parseAll "times-paginating-presence" form
        pageCustomers <-
            Set.fromList <$> parseAll "customers-paginating-presence" form
        pageTxIds <- Set.fromList <$> parseAll "tx-ids-paginating-presence" form
        pure
            $ DepositsParams
                slot
                window
                firstWeekDay
                viewStart
                windowOpen
                spent
                customers
                pageTimes
                pageCustomers
                pageTxIds
