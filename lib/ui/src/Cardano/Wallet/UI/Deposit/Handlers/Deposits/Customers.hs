{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Cardano.Wallet.UI.Deposit.Handlers.Deposits.Customers
    ( depositCustomersPaginateM
    , depositCustomersHandler
    , AtTimeByCustomer
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
    , unPatch
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
    , Slot
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
import Data.Bifunctor
    ( first
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

type AtTimeByCustomer =
    Map
        '[ W (First Slot) Customer
         , W (First Address) TxId
         ]
        ValueTransfer

depositCustomersPaginateM
    :: forall m
     . Monad m
    => DepositsParams
    -> m ByTime
    -> DownTime
    -> Int
    -> PaginateM
        m
        Customer
        (Map.Map Customer (Maybe Address, ValueTransfer))
depositCustomersPaginateM
    depositsParams
    retrieveByTime
    time
    rows =
        Paginate
            { previousIndex = \customer -> runMaybeT $ do
                Paginate{previousIndex} <- history
                hoistMaybe $ previousIndex customer
            , nextIndex = \customer -> runMaybeT $ do
                Paginate{nextIndex} <- history
                hoistMaybe $ nextIndex customer
            , minIndex = runMaybeT $ do
                Paginate{minIndex} <- history
                hoistMaybe minIndex
            , pageAtIndex = \k -> runMaybeT $ do
                Paginate{pageAtIndex} <- history
                hoistMaybe
                    $ fmap (fmap (first getFirst . fold . unPatch))
                        <$> pageAtIndex k
            }
      where
        history =
            mkStrictMapPaginate rows
                . getMonoidalMap
                . value
                . forgetPatch
                <$> retrieveAtTimeByCustomer
                    retrieveByTime
                    depositsParams
                    time

retrieveAtTimeByCustomer
    :: Monad m
    => m ByTime
    -> DepositsParams
    -> DownTime
    -> MaybeT m AtTimeByCustomer
retrieveAtTimeByCustomer
    retrieveByTime
    DepositsParams{depositsFirstWeekDay, depositsWindow}
    tStart = do
            transfers' <- lift retrieveByTime
            let tEnd =
                    fmap
                        (nextDiscretizedTime depositsFirstWeekDay depositsWindow)
                        <$> tStart
            hoistMaybe
                $ fmap snd
                $ lookupFinger tEnd tStart transfers'

depositCustomersHandler
    :: SessionLayer WalletResource
    -> (AtTimeByCustomer -> html)
    -> (BL.ByteString -> html)
    -> DepositsParams
    -> WithOrigin UTCTime
    -> Handler html
depositCustomersHandler
    layer
    render
    alert
    params
    start = catchRunWalletResourceHtml layer alert id $ do
        transfers <-
            runMaybeT
                $ retrieveAtTimeByCustomer
                    getTxHistoryByTime
                    params
                    (Down start)
        pure $ case transfers of
            Just customers -> render customers
            Nothing ->
                alert
                    "No deposits found for that time period"
