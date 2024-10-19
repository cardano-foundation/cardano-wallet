{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.UI.Deposit.Handlers.Deposits.Deposits
    ( depositsPageHandler
    , quantizeByTime
    )
where

import Prelude hiding
    ( lookup
    )

import Cardano.Wallet.Deposit.Map
    ( Map (Map)
    , forMap
    , openMap
    , withMap
    )
import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( ByTime
    , DownTime
    )
import Cardano.Wallet.UI.Deposit.API
    ( DepositsParams (..)
    , Window (..)
    )
import Cardano.Wallet.UI.Deposit.Handlers.Pagination
    ( PageHandler (..)
    )
import Cardano.Wallet.UI.Lib.Discretization
    ( minKey
    , quantizeByTime'
    )
import Cardano.Wallet.UI.Lib.Pagination
    ( next
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
import Data.Map.Monoidal.Strict
    ( MonoidalMap (..)
    )
import Data.Time
    ( DayOfWeek
    )

import qualified Data.Map.Monoidal.Strict as MonoidalMap

quantizeByTime :: DayOfWeek -> Window -> ByTime -> ByTime
quantizeByTime fdk w (Map mm) = Map $ quantizeByTime' fdk w mm

depositsPageHandler
    :: MonadIO m
    => DepositsParams
    -> m ByTime
    -> Int
    -> PageHandler m DownTime ByTime
depositsPageHandler
    DepositsParams{depositsFirstWeekDay, depositsWindow}
    newDepositsHistory
    rows =
        PageHandler
            { pagePrevious = \t -> runMaybeT $ do
                m <- lift newDepositsHistory'
                hoistMaybe
                    $ withMap m
                    $ \(MonoidalMap transfers) -> previous rows transfers t
            , pageNext = \t -> runMaybeT $ do
                m <- lift newDepositsHistory'
                hoistMaybe
                    $ withMap m
                    $ \(MonoidalMap transfers) -> next rows transfers t
            , page = \t -> do
                m <- newDepositsHistory'
                liftIO $ print $ MonoidalMap.keys $ openMap m
                pure
                    $ forMap m
                    $ \(MonoidalMap transfers) ->
                        MonoidalMap $ nextPage rows t transfers
            , start = do
                m <- newDepositsHistory'
                pure $ withMap m minKey
            }
      where
        newDepositsHistory' =
            quantizeByTime
                depositsFirstWeekDay
                depositsWindow
                <$> newDepositsHistory
