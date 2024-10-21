{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.UI.Deposit.Handlers.Deposits.Deposits
    ( depositsPaginationHandlers
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
    , withMap
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
import Cardano.Wallet.UI.Deposit.API
    ( DepositsParams (..)
    , Window (..)
    )
import Cardano.Wallet.UI.Deposit.Handlers.Pagination
    ( PaginationHandlers (..)
    )
import Cardano.Wallet.UI.Lib.Discretization
    ( discretizeTime
    , nextDiscretizedTime
    )
import Cardano.Wallet.UI.Lib.Pagination
    ( minKey
    , next
    , nextPage
    , previous
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
    ( first
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

depositsPaginationHandlers
    :: MonadIO m
    => DepositsParams
    -> m ByTime
    -> Int
    -> PaginationHandlers m DownTime
        (Map.Map DownTime (Maybe Slot, ValueTransfer))
depositsPaginationHandlers
    DepositsParams{depositsFirstWeekDay, depositsWindow}
    newDepositsHistory
    rows =
        PaginationHandlers
            { previousPageIndex = \t -> runMaybeT $ do
                m <- lift newDepositsHistory'
                hoistMaybe
                    $ withMap m
                    $ \(MonoidalMap transfers) -> previous rows transfers t
            , nextPageIndex = \t -> runMaybeT $ do
                m <- lift newDepositsHistory'
                hoistMaybe
                    $ withMap m
                    $ \(MonoidalMap transfers) -> next rows transfers t
            , retrievePage = \t -> do
                m <- newDepositsHistory'
                pure
                    $ fmap (first getFirst . fold . unPatch)
                    $ nextPage rows t
                    $ getMonoidalMap
                    $ openMap m
            , startingIndex = do
                m <- newDepositsHistory'
                pure $ withMap m $ \(MonoidalMap q) -> minKey q
            }
      where
        newDepositsHistory' =
            discretizeAByTime
                depositsFirstWeekDay
                depositsWindow
                <$> newDepositsHistory
