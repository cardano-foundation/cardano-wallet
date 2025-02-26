{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.UI.Deposit.Handlers.Deposits.Times
    ( depositsPaginateM
    )
where

import Prelude hiding
    ( lookup
    )

import Cardano.Wallet.Deposit.Map
    ( unPatch
    , value
    )
import Cardano.Wallet.Deposit.Map.Timed
    ( Timed (..)
    )
import Cardano.Wallet.Deposit.Pure
    ( ValueTransfer
    )
import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( ByTime
    , DownTime
    )
import Cardano.Wallet.Deposit.Read
    ( Slot
    )
import Cardano.Wallet.UI.Deposit.API.Deposits.Deposits
    ( DepositsParams (..)
    )
import Cardano.Wallet.UI.Lib.Discretization
    ( discretizeTime
    )
import Cardano.Wallet.UI.Lib.Pagination.Map
    ( Paginate (..)
    )
import Cardano.Wallet.UI.Lib.Pagination.TimedSeq
    ( mkTimedSeqPaginate
    )
import Cardano.Wallet.UI.Lib.Pagination.Type
    ( PaginateM
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Monad.Trans
    ( lift
    )
import Control.Monad.Trans.Maybe
    ( MaybeT (..)
    , hoistMaybe
    )
import Data.Bifunctor
    ( Bifunctor (..)
    , first
    )
import Data.Foldable
    ( Foldable (..)
    )
import Data.Monoid
    ( First (..)
    , Last (..)
    )
import Data.Ord
    ( Down (..)
    )

import qualified Cardano.Wallet.Deposit.Map.Timed as TimedSeq
import qualified Data.Map.Strict as Map

depositsPaginateM
    :: MonadIO m
    => DepositsParams
    -> m ByTime
    -> Int
    -> PaginateM
        m
        DownTime
        (Map.Map DownTime (Maybe Slot, ValueTransfer))
depositsPaginateM
    DepositsParams{depositsFirstWeekDay, depositsWindow}
    retrieveByTime
    rows =
        Paginate
            { previousIndex = \t -> runMaybeT $ do
                Paginate{previousIndex} <- lift history
                hoistMaybe $ previousIndex t
            , nextIndex = \t -> runMaybeT $ do
                Paginate{nextIndex} <- lift history
                hoistMaybe $ nextIndex t
            , pageAtIndex = \t -> do
                Paginate{pageAtIndex} <- history
                pure
                    $ second
                        ( Map.fromList
                            . concatMap fromTimed
                            . TimedSeq.toList
                        )
                        <$> pageAtIndex t
            , minIndex = do
                Paginate{minIndex} <- paginate <$> retrieveByTime
                pure minIndex
            }
      where
        discretize = discretizeTime depositsFirstWeekDay depositsWindow
        paginate = mkTimedSeqPaginate bucket rows . id . value
        history = paginate <$> retrieveByTime
        bucket (Down t) = Down $ fmap discretize t
        fromTimed (Timed (Last (Just t)) x) =
            [(bucket t, first getFirst $ fold $ unPatch x)]
        fromTimed _ = []
