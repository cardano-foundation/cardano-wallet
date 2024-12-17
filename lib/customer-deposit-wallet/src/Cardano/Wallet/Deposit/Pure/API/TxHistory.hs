{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( ByCustomer
    , ByTime
    , DownTime
    , ResolveAddress
    , LookupTimeFromSlot
    , TxHistory (..)
    , firstJust
    , transfers
    , rollForward
    , rollBackward
    )
where

import Prelude

import Cardano.Wallet.Deposit.Map
    ( F
    , Map (..)
    , W
    , onFinger
    , onMap
    , singletonFinger
    , singletonMap
    )
import Cardano.Wallet.Deposit.Map.Timed
    ( TimedSeq
    , dropBefore
    )
import Cardano.Wallet.Deposit.Pure.Address
    ( Customer
    )
import Cardano.Wallet.Deposit.Pure.Balance
    ( ValueTransferMap
    )
import Cardano.Wallet.Deposit.Pure.UTxO.ValueTransfer
    ( ValueTransfer
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    , WithOrigin (..)
    )
import Cardano.Wallet.Deposit.Time
    ( LookupTimeFromSlot
    )
import Cardano.Wallet.Read
    ( Slot
    , TxId
    )
import Data.Foldable
    ( Foldable (..)
    )
import Data.Maybe
    ( maybeToList
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

import qualified Data.Map.Monoidal.Strict as MonoidalMap

firstJust :: a -> First a
firstJust = First . Just

transfers
    :: Foldable (Map xs) => Map xs ValueTransfer -> ValueTransfer
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

rollForward
    :: ValueTransferMap
    -> ResolveAddress
    -> LookupTimeFromSlot
    -> Slot
    -> TxHistory
    -> TxHistory
rollForward valueTransferMap resolveAddress timeFromSlot slot =
    (txHistory' <>)
  where
    txHistory' =
        blockToTxHistory valueTransferMap resolveAddress timeFromSlot slot

blockToTxHistory
    :: ValueTransferMap
    -> ResolveAddress
    -> LookupTimeFromSlot
    -> Slot
    -> TxHistory
blockToTxHistory valueTransferMap resolveAddress timeFromSlot slot =
    fold $ do
        time <- fmap Down $ maybeToList $ timeFromSlot slot
        (address, valueTransferByTxId) <- MonoidalMap.toList valueTransferMap
        (txId, valueTransfer) <- MonoidalMap.toList valueTransferByTxId
        customer <- maybeToList $ resolveAddress address
        let byTime =
                singletonFinger () time
                    $ singletonMap (First $ Just slot) customer
                    $ singletonMap (First $ Just address) txId
                    $ Value valueTransfer
        let byCustomer =
                singletonMap () customer
                    $ singletonFinger (First $ Just address) time
                    $ singletonMap (First $ Just slot) txId
                    $ Value valueTransfer
        pure $ TxHistory{byCustomer, byTime}

-- | Roll backward the transaction history to a given slot. This function
-- relies on the TxHistory to be sorted by time both on the time and
-- customer views.
rollBackward
    :: LookupTimeFromSlot
    -> Slot
    -> TxHistory
    -> TxHistory
rollBackward timeFromSlot slot TxHistory{byCustomer, byTime} =
    TxHistory
        { byCustomer =
            onMap byCustomer
                $ cleanNulls . fmap (`onFinger` takeToSlot)
        , byTime = onFinger byTime takeToSlot
        }
  where
    takeToSlot :: Monoid a => TimedSeq DownTime a -> TimedSeq DownTime a
    takeToSlot x = maybe x (`forgetAfter` x) $ timeFromSlot slot
    forgetAfter t = dropBefore (Down t)
    cleanNulls = MonoidalMap.filter (not . null)
