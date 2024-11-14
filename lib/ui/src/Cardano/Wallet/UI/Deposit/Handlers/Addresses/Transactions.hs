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

import Cardano.Wallet.Deposit.Map
    ( F
    , Map (..)
    , W
    , lookupMap
    , unPatch
    , value
    )
import Cardano.Wallet.Deposit.Map.Timed
    ( Timed (..)
    )
import Cardano.Wallet.Deposit.Pure
    ( ValueTransfer (..)
    )
import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( DownTime
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    , Slot
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResource
    , customerAddress
    , getTxHistoryByCustomer
    )
import Cardano.Wallet.Read
    ( TxId
    , WithOrigin (..)
    )
import Cardano.Wallet.UI.Common.Layer
    ( SessionLayer (..)
    )
import Cardano.Wallet.UI.Deposit.API.Addresses.Transactions
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
import Data.Bifunctor
    ( first
    )
import Data.Monoid
    ( First (..)
    , Last (..)
    )
import Data.Ord
    ( Down (..)
    )
import Data.Time
    ( UTCTime
    )
import Servant
    ( Handler
    )

import qualified Cardano.Wallet.Deposit.Map.Timed as TimedSeq
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Monoidal.Strict as MonoidalMap

getCustomerHistory
    :: SessionLayer WalletResource
    -> ( TransactionHistoryParams
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
                    h <- getTxHistoryByCustomer
                    pure
                        $ render params
                        $ filterByParams params
                        $ convert
                        $ snd <$> lookupMap txHistoryCustomer h

convert
    :: Maybe
        (Map [F (First Address) DownTime, W (First Slot) TxId] ValueTransfer)
    -> [(DownTime, (Slot, TxId, ValueTransfer))]
convert Nothing = []
convert (Just mtxs) = concatMap f $ TimedSeq.toList $ value mtxs
  where
    f
        :: Timed DownTime (Map '[W (First Slot) TxId] ValueTransfer)
        -> [(DownTime, (Slot, TxId, ValueTransfer))]
    f (Timed (Last (Just time)) txs) = do
        (txId, Value (First (Just slot), v)) <-
            MonoidalMap.assocs . value . unPatch $ txs
        pure (time, (slot, txId, v))
    f _ = []

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
        case time of
            At t -> Match t
            Origin -> DirectionMatch
    filterByTransfer = case (txHistoryReceived, txHistorySpent) of
        (True, False) ->
            filter
                ( \(_, (_, _, ValueTransferC{received})) ->
                    received /= mempty
                )
        (False, True) ->
            filter
                ( \(_, (_, _, ValueTransferC{spent})) ->
                    spent /= mempty
                )
        _ -> id
