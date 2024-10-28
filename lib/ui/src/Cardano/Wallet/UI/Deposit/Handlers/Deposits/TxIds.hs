{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    , lookup
    , lookupPatched
    , openMap
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
    DepositsParams{depositsFirstWeekDay, depositsWindow}
    newDepositsHistory
    time
    customer
    rows =
        Paginate
            { previousIndex = \k -> runMaybeT $ do
                Paginate{previousIndex} <- paginate <$> newDepositTxIds
                hoistMaybe $ previousIndex k
            , nextIndex = \k -> runMaybeT $ do
                Paginate{nextIndex} <- paginate <$> newDepositTxIds
                hoistMaybe $ nextIndex k
            , minIndex = runMaybeT $ do
                Paginate{minIndex} <- paginate <$> newDepositTxIds
                hoistMaybe minIndex
            , pageAtIndex = \k -> runMaybeT $ do
                Paginate{pageAtIndex} <- paginate <$> newDepositTxIds
                hoistMaybe $ do
                    (n, x) <- pageAtIndex k
                    pure (n, fmap fold x)
            }
      where
        paginate = mkStrictMapPaginate rows . getMonoidalMap . openMap . forgetPatch
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
    customer =
        do
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
                        Just window -> case lookupPatched customer window of
                            Just (_, txIds) -> render txIds
                            Nothing ->
                                alert
                                    "No deposits found for that time period and customer"
                        Nothing ->
                            alert
                                $ "No deposits found for that time period"
                                    <> " "
                                    <> BL8.pack (show start)
