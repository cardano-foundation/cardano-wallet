{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Deposit.Pure.State.TxHistory
    ( getTxHistoryByCustomer
    , getTxHistoryByTime
    , getEraSlotOfBlock
    , getCustomerDeposits
    , getAllDeposits
    ) where

import Prelude hiding
    ( lookup
    )

import Cardano.Wallet.Deposit.Map
    ( Map
    , W
    , lookupMap
    , value
    )
import Cardano.Wallet.Deposit.Map.Timed
    ( Timed
    , TimedSeq
    , extractInterval
    , monoid
    )
import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( ByCustomer
    , ByTime
    , DownTime
    , TxHistory (..)
    )
import Cardano.Wallet.Deposit.Pure.State.Type
    ( Customer
    , WalletState (..)
    )
import Cardano.Wallet.Deposit.Pure.UTxO.ValueTransfer
    ( ValueTransfer (..)
    )
import Cardano.Wallet.Deposit.Read
    ( TxId
    , WithOrigin (..)
    , getEraSlotOfBlock
    )
import Data.FingerTree
    ( Measured (..)
    )
import Data.Foldable
    ( Foldable (..)
    , fold
    )
import Data.Map.Monoidal.Strict
    ( MonoidalMap (..)
    )
import Data.Ord
    ( Down (..)
    )
import Data.Time
    ( UTCTime
    )

import qualified Data.Map.Strict as Map

getTxHistoryByCustomer :: WalletState -> ByCustomer
getTxHistoryByCustomer state = byCustomer $ txHistory state

getTxHistoryByTime :: WalletState -> ByTime
getTxHistoryByTime state = byTime $ txHistory state

getCustomerDeposits
    :: Customer
    -> Maybe (WithOrigin UTCTime, WithOrigin UTCTime)
    -> WalletState
    -> Map.Map TxId ValueTransfer
getCustomerDeposits c interval s = fold $ do
    fmap (wonders interval . value . snd)
        $ lookupMap c
        $ getTxHistoryByCustomer s

getAllDeposits
    :: Maybe (WithOrigin UTCTime, WithOrigin UTCTime)
    -> WalletState
    -> Map.Map Customer ValueTransfer
getAllDeposits interval s =
    wonders interval
        $ value
        $ getTxHistoryByTime s

wonders
    :: (Ord k, Monoid w, Foldable (Map xs), Monoid (Map xs ValueTransfer))
    => Maybe (WithOrigin UTCTime, WithOrigin UTCTime)
    -> TimedSeq DownTime (Map (W w k : xs) ValueTransfer)
    -> Map.Map k ValueTransfer
wonders interval =
    getMonoidalMap
        . monoid
        . fmap (fmap fold . value)
        . extractInterval' interval
  where
    extractInterval'
        :: Monoid a
        => Maybe (WithOrigin UTCTime, WithOrigin UTCTime)
        -> TimedSeq (DownTime) a
        -> Timed (DownTime) a
    extractInterval' Nothing = measure
    extractInterval' (Just (t1, t2)) = extractInterval (Down t1) (Down t2)
