{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.UI.Deposit.Handlers.Deposits.Customers
    ( depositCustomersPaginationHandlers
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
    , lookup
    , openPatched
    , unPatch
    , withPatched
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
    )
import Cardano.Wallet.Read
    ( TxId
    )
import Cardano.Wallet.UI.Common.Layer
    ( SessionLayer
    )
import Cardano.Wallet.UI.Deposit.API
    ( DepositsParams (..)
    )
import Cardano.Wallet.UI.Deposit.Handlers.Deposits.Deposits
    ( discretizeAByTime
    )
import Cardano.Wallet.UI.Deposit.Handlers.Deposits.Fake
    ( getFakeDepositsHistory
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( catchRunWalletResourceHtml
    )
import Cardano.Wallet.UI.Deposit.Handlers.Pagination
    ( PaginationHandlers (..)
    )
import Cardano.Wallet.UI.Lib.Pagination
    ( minKey
    , next
    , nextPage
    , previous
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

depositCustomersPaginationHandlers
    :: forall m
     . Monad m
    => DepositsParams
    -> m ByTime
    -> DownTime
    -> Int
    -> PaginationHandlers
        m
        Customer
        (Map.Map Customer (Maybe Address, ValueTransfer))
depositCustomersPaginationHandlers
    DepositsParams{depositsFirstWeekDay, depositsWindow}
    newDepositsHistory
    time
    rows =
        PaginationHandlers
            { previousPageIndex = \k -> runMaybeT $ do
                m <- newDepositCustomers time
                hoistMaybe
                    $ withPatched m
                    $ \_ (MonoidalMap r) ->
                        previous rows r k
            , nextPageIndex = \k -> runMaybeT $ do
                m <- newDepositCustomers time
                hoistMaybe
                    $ withPatched m
                    $ \_ (MonoidalMap r) ->
                        next rows r k
            , startingIndex = runMaybeT $ do
                m <- newDepositCustomers time
                hoistMaybe
                    $ withPatched m
                    $ \_ (MonoidalMap q) -> minKey q
            , retrievePage = \k -> fmap fold $ runMaybeT $ do
                m <- newDepositCustomers time
                pure
                    $ let
                        (_w, r) = openPatched m
                        p = nextPage rows k $ getMonoidalMap r
                      in
                        first getFirst . fold . unPatch <$> p
            }
      where
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
                        then getFakeDepositsHistory
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