{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( ByCustomer
    , ByTime
    , DownTime
    , ResolveAddress
    , LookupTimeFromSlot
    , TxHistory (..)
    , firstJust
    , transfers
    )
where

import Prelude

import Cardano.Wallet.Deposit.Map
    ( F
    , Map (..)
    , W
    )

import Cardano.Wallet.Deposit.Pure.Address
    ( Customer
    )
import Cardano.Wallet.Deposit.Pure.UTxO.ValueTransfer
    ( ValueTransfer
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    )
import Cardano.Wallet.Read
    ( Slot
    , TxId
    , WithOrigin
    )
import Data.Foldable
    ( Foldable (..)
    )
import Data.Monoid
    ( First (..)
    )
import Data.Ord
    ( Down (..)
    )
import Data.Time
    ( UTCTime
    )

firstJust :: a -> First a
firstJust = First . Just

transfers :: Foldable (Map xs) => Map xs ValueTransfer -> ValueTransfer
transfers = fold

type DownTime = Down (WithOrigin UTCTime)

type ByCustomer =
    Map
        '[ W () Customer
         , F (First Address) DownTime
         , W (First Slot) TxId
         ]
        ValueTransfer

type ByTime =
    Map
        '[ F () DownTime
         , W (First Slot) Customer
         , W (First Address) TxId
         ]
        ValueTransfer

data TxHistory = TxHistory
    { byCustomer :: ByCustomer
    , byTime :: ByTime
    }

instance Semigroup TxHistory where
    TxHistory a1 b1 <> TxHistory a2 b2 = TxHistory (a1 <> a2) (b1 <> b2)

instance Monoid TxHistory where
    mempty = TxHistory mempty mempty

type ResolveAddress = Address -> Maybe Customer
type LookupTimeFromSlot = Slot -> Maybe DownTime
