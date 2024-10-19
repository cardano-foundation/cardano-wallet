{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Cardano.Wallet.UI.Deposit.Handlers.Deposits.Customers
    ( depositCustomersPageHandler
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
    , forPatched
    , lookup
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
    ( PageHandler (..)
    )
import Cardano.Wallet.UI.Lib.Discretization
    ( minKey
    )
import Cardano.Wallet.UI.Lib.Pagination
    ( next
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
import Data.Foldable
    ( Foldable (..)
    )
import Data.Map.Monoidal.Strict
    ( MonoidalMap (..)
    )
import Data.Map.Strict
    ()
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

type AtTimeByCustomer =
    Map
        '[ W (First Slot) Customer
         , W (First Address) TxId
         ]
        ValueTransfer

depositCustomersPageHandler
    :: forall m
     . Monad m
    => DepositsParams
    -> m ByTime
    -> DownTime
    -> Int
    -> PageHandler m (DownTime, Customer) AtTimeByCustomer
depositCustomersPageHandler
    DepositsParams{depositsFirstWeekDay, depositsWindow}
    newDepositsHistory
    time
    rows =
        PageHandler
            { pagePrevious = \(t, k) -> runMaybeT $ do
                m <- newDepositCustomers t
                fmap (t,)
                    $ hoistMaybe
                    $ withPatched m
                    $ \_ (MonoidalMap r) ->
                        previous rows r k
            , pageNext = \(t, k) -> runMaybeT $ do
                m <- newDepositCustomers t
                fmap (t,)
                    $ hoistMaybe
                    $ withPatched m
                    $ \_ (MonoidalMap r) ->
                        next rows r k
            , page = \(t, k) -> fmap fold $ runMaybeT $ do
                m <- newDepositCustomers t
                pure
                    $ forPatched m
                    $ \_ (MonoidalMap r) ->
                        MonoidalMap $ nextPage rows k r
            , start = runMaybeT $ do
                m <- newDepositCustomers time
                hoistMaybe
                    $ fmap (time,)
                    $ withPatched m
                    $ \_ -> minKey
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
