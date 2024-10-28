{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.UI.Deposit.Handlers.Deposits.Times
    ( depositsPaginateM
    , discretizeAByTime
    )
where

import Prelude hiding
    ( lookup
    )

import Cardano.Slotting.Slot
    ( WithOrigin (..)
    )
import Cardano.Wallet.Deposit.Map
    ( Map (Map)
    , openMap
    , unPatch
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
    ( Window
    , discretizeTime
    , nextDiscretizedTime
    )
import Cardano.Wallet.UI.Lib.Pagination.Map
    ( Paginate (..)
    , mkStrictMapPaginate
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
    ( fold
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
    ( DayOfWeek
    )

import qualified Data.Map.Monoidal.Strict as MonoidalMap
import qualified Data.Map.Strict as Map

discretizeAMonoidalMap
    :: Monoid a
    => DayOfWeek
    -> Window
    -> MonoidalMap DownTime a
    -> MonoidalMap DownTime a
discretizeAMonoidalMap fdk w mm = case MonoidalMap.lookupMin mm of
    Just ((Down (At t)), _) ->
        let
            t' = discretizeTime fdk w t
            nt = Down $ At $ nextDiscretizedTime fdk w t'
            (before, match, after) = MonoidalMap.splitLookup nt mm
            after' =
                maybe
                    after
                    (\v -> MonoidalMap.insert nt v after)
                    match
        in
            MonoidalMap.singleton (Down (At t')) (fold before)
                <> discretizeAMonoidalMap fdk w after'
    Just (Down Origin, _) ->
        let
            (before, match, after) = MonoidalMap.splitLookup (Down Origin) mm
            before' =
                maybe
                    before
                    (\v -> MonoidalMap.insert (Down Origin) v before)
                    match
        in
            MonoidalMap.singleton (Down Origin) (fold before')
                <> discretizeAMonoidalMap fdk w after
    Nothing -> MonoidalMap.empty

discretizeAByTime :: DayOfWeek -> Window -> ByTime -> ByTime
discretizeAByTime fdk w (Map mm) = Map $ discretizeAMonoidalMap fdk w mm

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
    newDepositsHistory
    rows =
        Paginate
            { previousIndex = \t -> runMaybeT $ do
                Paginate{previousIndex} <- paginate <$> lift discretizedHistory
                hoistMaybe $ previousIndex t
            , nextIndex = \t -> runMaybeT $ do
                Paginate{nextIndex} <- paginate <$> lift discretizedHistory
                hoistMaybe $ nextIndex t
            , pageAtIndex = \t -> do
                Paginate{pageAtIndex} <- paginate <$> discretizedHistory
                pure
                    $ fmap (fmap (first getFirst . fold . unPatch))
                    <$> pageAtIndex t
            , minIndex = do
                Paginate{minIndex} <- paginate <$> discretizedHistory
                pure minIndex
            }
      where
        paginate = mkStrictMapPaginate rows . getMonoidalMap . openMap
        discretizedHistory = do
            discretizeAByTime
                depositsFirstWeekDay
                depositsWindow
                <$> newDepositsHistory
