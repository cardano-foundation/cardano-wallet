{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    , lookup
    , openMap
    , unPatch
    )
import Cardano.Wallet.Deposit.Pure
    ( Customer
    , ValueTransfer (..)
    )
import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( ByTime
    , DownTime
    , byTime
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    , Slot
    , WithOrigin (..)
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResource
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
import Cardano.Wallet.UI.Deposit.Handlers.Deposits.Mock
    ( getMockHistory
    )
import Cardano.Wallet.UI.Deposit.Handlers.Deposits.Times
    ( discretizeAByTime
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( catchRunWalletResourceHtml
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
import qualified Data.ByteString.Lazy.Char8 as BL8
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
    DepositsParams{depositsFirstWeekDay, depositsWindow}
    newDepositsHistory
    time
    rows =
        Paginate
            { previousIndex = \customer -> runMaybeT $ do
                Paginate{previousIndex} <- paginate <$> newDepositCustomers time
                hoistMaybe $ previousIndex customer
            , nextIndex = \customer -> runMaybeT $ do
                Paginate{nextIndex} <- paginate <$> newDepositCustomers time
                hoistMaybe $ nextIndex customer
            , minIndex = runMaybeT $ do
                Paginate{minIndex} <- paginate <$> newDepositCustomers time
                hoistMaybe minIndex
            , pageAtIndex = \k -> runMaybeT $ do
                Paginate{pageAtIndex} <- paginate <$> newDepositCustomers time
                hoistMaybe
                    $ fmap (fmap (first getFirst . fold . unPatch))
                        <$> pageAtIndex k
            }
      where
        paginate =
            mkStrictMapPaginate rows
                . getMonoidalMap
                . openMap
                . forgetPatch
        newDepositCustomers :: DownTime -> MaybeT m AtTimeByCustomer
        newDepositCustomers t = do
            transfers' <- lift newDepositsHistory
            let transfers'' =
                    discretizeAByTime
                        depositsFirstWeekDay
                        depositsWindow
                        transfers'
            hoistMaybe $ lookup t transfers''

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
    DepositsParams{depositsFirstWeekDay, depositsWindow, depositsFakeData}
    start = do
        catchRunWalletResourceHtml layer alert id
            $ do
                transfers <-
                    if depositsFakeData
                        then byTime <$> getMockHistory
                        else error "depositsHistoryWindowHandler: real data not implemented"
                let transfers' =
                        discretizeAByTime
                            depositsFirstWeekDay
                            depositsWindow
                            transfers
                pure $ case lookup (Down start) transfers' of
                    Just window -> render window
                    Nothing ->
                        alert
                            $ "No deposits found for that time period"
                                <> " "
                                <> BL8.pack (show start)
