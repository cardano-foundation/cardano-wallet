{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.UI.Deposit.Handlers.Addresses.Transactions
where

import Prelude hiding
    ( lookup
    )

import Cardano.Wallet.Deposit.Pure
    ( ValueTransfer (..)
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    , Slot
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResource
    , customerAddress
    )
import Cardano.Wallet.Read
    ( TxId
    , WithOrigin (..)
    )
import Cardano.Wallet.UI.Common.Layer
    ( SessionLayer (..)
    )
import Cardano.Wallet.UI.Deposit.API
    ( TransactionHistoryParams (..)
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( catchRunWalletResourceHtml
    )
import Cardano.Wallet.UI.Lib.Time.Direction
    ( Match (..)
    , filterByDirection
    , sortByDirection
    , utcTimeByDirection
    )

import Cardano.Wallet.Deposit.Map
    ( Map (..)
    , W
    , forgetPatch
    , lookup
    , openMap
    , unPatch
    )
import Data.Foldable
    ( fold
    )
import Data.Monoid
    ( First (..)
    )
import Data.Ord
    ( Down (..)
    )
import Servant
    ( Handler
    )

import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( DownTime
    , TxHistory (..)
    )
import Cardano.Wallet.UI.Deposit.Handlers.Deposits.Mock
    ( getMockHistory
    )
import Data.Bifunctor
    ( first
    )
import Data.Time
    ( UTCTime
    )

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Monoidal.Strict as MonoidalMap

getCustomerHistory
    :: SessionLayer WalletResource
    -> ( Bool
         -> TransactionHistoryParams
         -> [(WithOrigin UTCTime, (Slot, TxId, ValueTransfer))]
         -> html
       )
    -> (BL.ByteString -> html)
    -> TransactionHistoryParams
    -> Handler html
getCustomerHistory
    layer
    render
    alert
    params@TransactionHistoryParams{..} = do
        catchRunWalletResourceHtml layer alert id $ do
            r <- customerAddress txHistoryCustomer
            case r of
                Nothing -> pure $ alert "Address not discovered"
                Just _ -> do
                    h <- byCustomer <$> getMockHistory
                    pure
                        $ render True params
                        $ filterByParams params
                        $ convert
                        $ lookup txHistoryCustomer h

convert
    :: Maybe
        (Map '[W (First Address) DownTime, W (First Slot) TxId] ValueTransfer)
    -> [(DownTime, (Slot, TxId, ValueTransfer))]
convert = concatMap f . MonoidalMap.assocs . openMap . forgetPatch . fold
    where
        f :: (DownTime, Map '[W (First Slot) TxId] ValueTransfer)
                -> [(DownTime, (Slot, TxId, ValueTransfer))]
        f (time, txs) = do
            (txId, Value (First (Just slot) , value) )
                <- MonoidalMap.assocs . openMap . unPatch $ txs
            pure (time, (slot, txId, value))

filterByParams
    :: TransactionHistoryParams
    -> [(DownTime, (Slot, TxId, ValueTransfer))]
    -> [(WithOrigin UTCTime, (Slot, TxId, ValueTransfer))]
filterByParams TransactionHistoryParams{..} =
    sortByDirection txHistorySorting fst
        . filterByDirection
            txHistorySorting
            startTime
            matchUTCTime
        . fmap (first getDown)
        . filterByTransfer
  where
    startTime =
        utcTimeByDirection
            txHistorySorting
            txHistoryStartYear
            txHistoryStartMonth
    matchUTCTime (time, (_, _, _)) =
        do
            case time of
                At t -> Match t
                Origin -> DirectionMatch
    filterByTransfer = case (txHistoryReceived, txHistorySpent) of
        (True, False) ->
            filter
                ( \(_, (_, _, ValueTransfer{received})) ->
                    received /= mempty
                )
        (False, True) ->
            filter
                ( \(_, (_, _, ValueTransfer{spent})) ->
                    spent /= mempty
                )
        _ -> id
