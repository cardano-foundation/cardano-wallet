{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Cardano.Wallet.UI.Deposit.Handlers.Deposits.TxIds
    ( depositCustomersTxIdsPaginateM
    , depositCustomersTxIdsHandler
    , AtTimeAtCustomerByTxId
    )
where

import Prelude hiding
    ( lookup
    )

import Cardano.Wallet.Deposit.Map
    ( Map (..)
    , W
    , forgetPatch
    , lookupFinger
    , lookupMap
    , value
    )
import Cardano.Wallet.Deposit.Pure
    ( Customer
    , ValueTransfer (..)
    )
import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( ByTime
    , DownTime
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    , WithOrigin (..)
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResource
    , getTxHistoryByTime
    )
import Cardano.Wallet.Read
    ( TxId
    )
import Cardano.Wallet.UI.Common.Layer
    ( SessionLayer
    )
import Cardano.Wallet.UI.Deposit.API.Deposits.Deposits
    ( DepositsParams (..)
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( catchRunWalletResourceHtml
    )
import Cardano.Wallet.UI.Lib.Discretization
    ( nextDiscretizedTime
    )
import Cardano.Wallet.UI.Lib.Pagination.Map
    ( Paginate (..)
    , mkStrictMapPaginate
    )
import Cardano.Wallet.UI.Lib.Pagination.Type
    ( PaginateM
    )
import Control.Monad.Trans
    ( lift
    )
import Control.Monad.Trans.Maybe
    ( MaybeT (..)
    , hoistMaybe
    )
import Data.Foldable
    ( Foldable (..)
    )
import Data.Map.Monoidal.Strict
    ( MonoidalMap (..)
    )
import Data.Monoid
    ( First (..)
    )
import Data.Ord
    ( Down (..)
    )
import Data.Time
    ( UTCTime (..)
    )
import Servant
    ( Handler
    )

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map

type AtTimeAtCustomerByTxId =
    Map
        '[ W (First Address) TxId
         ]
        ValueTransfer

depositCustomersTxIdsPaginateM
    :: forall m
     . Monad m
    => DepositsParams
    -> m ByTime
    -> DownTime
    -> Customer
    -> Int
    -> PaginateM
        m
        TxId
        (Map.Map TxId ValueTransfer)
depositCustomersTxIdsPaginateM
    depositsParams
    retrieveByTime
    time
    customer
    rows =
        Paginate
            { previousIndex = \k -> runMaybeT $ do
                Paginate{previousIndex} <- history
                hoistMaybe $ previousIndex k
            , nextIndex = \k -> runMaybeT $ do
                Paginate{nextIndex} <- history
                hoistMaybe $ nextIndex k
            , minIndex = runMaybeT $ do
                Paginate{minIndex} <- history
                hoistMaybe minIndex
            , pageAtIndex = \k -> runMaybeT $ do
                Paginate{pageAtIndex} <- history
                hoistMaybe $ do
                    (n, x) <- pageAtIndex k
                    pure (n, fmap fold x)
            }
      where
        history =
            mkStrictMapPaginate rows
                . getMonoidalMap
                . value
                . forgetPatch
                <$> retrieveAtTimeAtCustomerByTxId
                    retrieveByTime
                    depositsParams
                    time
                    customer

retrieveAtTimeAtCustomerByTxId
    :: Monad m
    => m ByTime
    -> DepositsParams
    -> DownTime
    -> Customer
    -> MaybeT m AtTimeAtCustomerByTxId
retrieveAtTimeAtCustomerByTxId
    retrieveByTime
    DepositsParams{depositsFirstWeekDay, depositsWindow}
    tStart
    customerStart =
        do
            transfers' <- lift retrieveByTime
            let tEnd =
                    fmap
                        (nextDiscretizedTime depositsFirstWeekDay depositsWindow)
                        <$> tStart
            customers <-
                hoistMaybe
                    $ fmap snd
                    $ lookupFinger tEnd tStart transfers'
            hoistMaybe
                $ fmap snd
                $ lookupMap customerStart
                $ forgetPatch customers

depositCustomersTxIdsHandler
    :: SessionLayer WalletResource
    -> (AtTimeAtCustomerByTxId -> html)
    -> (BL.ByteString -> html)
    -> DepositsParams
    -> WithOrigin UTCTime
    -> Customer
    -> Handler html
depositCustomersTxIdsHandler
    layer
    render
    alert
    params
    start
    customer = catchRunWalletResourceHtml layer alert id $ do
        transfers <-
            runMaybeT
                $ retrieveAtTimeAtCustomerByTxId
                    getTxHistoryByTime
                    params
                    (Down start)
                    customer
        pure $ case transfers of
            Just txIds -> render txIds
            Nothing ->
                alert
                    "No deposits found for that time period and customer"
