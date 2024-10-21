{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.UI.Deposit.Handlers.Deposits.TxIds
    ( depositCustomersTxIdsPaginationHandlers
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
    , lookup
    , lookupPatched
    , openMap
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

type AtTimeAtCustomerByTxId =
    Map
        '[ W (First Address) TxId
         ]
        ValueTransfer

depositCustomersTxIdsPaginationHandlers
    :: forall m
     . Monad m
    => DepositsParams
    -> m ByTime
    -> DownTime
    -> Customer
    -> Int
    -> PaginationHandlers
        m
        TxId
        (Map.Map TxId ValueTransfer)
depositCustomersTxIdsPaginationHandlers
    DepositsParams{depositsFirstWeekDay, depositsWindow}
    newDepositsHistory
    time
    customer
    rows =
        PaginationHandlers
            { previousPageIndex = \k -> runMaybeT $ do
                m <- newDepositTxIds
                hoistMaybe
                    $ withPatched m
                    $ \_ (MonoidalMap r) ->
                        previous rows r k
            , nextPageIndex = \k -> runMaybeT $ do
                m <- newDepositTxIds
                hoistMaybe
                    $ withPatched m
                    $ \_ (MonoidalMap r) ->
                        next rows r k
            , startingIndex = runMaybeT $ do
                m <- newDepositTxIds
                hoistMaybe
                    $ withPatched m
                    $ \_ (MonoidalMap q) -> minKey q
            , retrievePage = \k -> fmap fold $ runMaybeT $ do
                m <- newDepositTxIds
                pure
                    $ let
                        r = openMap $ forgetPatch m
                        p = nextPage rows k $ getMonoidalMap r
                      in
                        fold <$> p
            }
      where
        newDepositTxIds
            :: MaybeT m AtTimeAtCustomerByTxId
        newDepositTxIds = do
            transfers' <- lift newDepositsHistory
            let transfers'' =
                    discretizeAByTime
                        depositsFirstWeekDay
                        depositsWindow
                        transfers'
            hoistMaybe
                $ lookup time transfers''
                    >>= lookup customer . forgetPatch

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
    DepositsParams{depositsFirstWeekDay, depositsWindow, depositsFakeData}
    start
    customer
    = do
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
                    Just window -> case lookupPatched customer window of
                        Just (_ , txIds) -> render txIds
                        Nothing ->
                            alert
                                "No deposits found for that time period and customer"
                    Nothing ->
                        alert
                            $ "No deposits found for that time period"
                                <> " "
                                <> BL8.pack (show start)
