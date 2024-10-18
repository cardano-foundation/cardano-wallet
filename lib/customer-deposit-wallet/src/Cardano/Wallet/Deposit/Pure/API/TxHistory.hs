{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardano.Wallet.Deposit.Pure.API.TxHistory
 where

import Prelude

import Cardano.Wallet.Deposit.Map
    ( K
    , Map (..)
    , W
    , singletonMap
    , singletonPatched
    )
import Cardano.Wallet.Deposit.Pure
    ( Customer
    , ValueTransfer
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
import Data.Maps.PairMap
    ( PairMap (..)
    )
import Data.Maps.Timeline
    ( Timeline (..)
    )
import Data.Maybe
    ( maybeToList
    )
import Data.Monoid
    ( First (..)
    )
import Data.Time
    ( UTCTime
    )

import qualified Cardano.Wallet.Deposit.Pure.TxHistory as H
import qualified Data.Map.Strict as Map
import Data.Ord
    ( Down (..)
    )
import qualified Data.Set as Set

firstJust :: a -> First a
firstJust = First . Just

transfers :: Foldable (Map xs) => Map xs ValueTransfer -> ValueTransfer
transfers = fold

type DownTime = Down (WithOrigin UTCTime)

type ByCustomer =
    Map
        '[ K Customer
         , W (First Address) DownTime
         , W (First Slot) TxId
         ]
        ValueTransfer

type ByTime =
    Map
        '[ K DownTime
         , W (First Slot) Customer
         , W (First Address) TxId
         ]
        ValueTransfer

data TxHistory = TxHistory
    { byCustomer :: ByCustomer
    , bySlot :: ByTime
    }

inefficientlyMkTxHistory
    :: ResolveAddress
    -> ResolveSlot
    -> H.TxHistory
    -> TxHistory
inefficientlyMkTxHistory resolveAddress resolveSlot h =
    TxHistory
        { byCustomer = inefficientByCustomer resolveAddress resolveSlot h
        , bySlot = inefficientBySlot resolveAddress resolveSlot h
        }

type ResolveAddress = Address -> Maybe Customer
type ResolveSlot = Slot -> Maybe DownTime

inefficientBySlot
    :: ResolveAddress
    -> ResolveSlot
    -> H.TxHistory
    -> ByTime
inefficientBySlot resolveAddress resolveSlots history = fold $ do
    (slot, txIds) <- Map.assocs $ eventsByTime (H.txIds history)
    time <- maybeToList $ resolveSlots slot
    txId <- Set.toList txIds
    addressToTransaction <-
        maybeToList
            $ Map.lookup txId
            $ mab (H.txTransfers history)
    (address, transaction) <- Map.assocs addressToTransaction
    customer <- maybeToList $ resolveAddress address
    pure
        $ singletonMap time
        $ singletonPatched (firstJust slot) customer
        $ singletonPatched (firstJust address) txId
        $ Value transaction

inefficientByCustomer
    :: ResolveAddress
    -> ResolveSlot
    -> H.TxHistory
    -> ByCustomer
inefficientByCustomer resolveAddress resolveSlot history = fold $ do
    (a, txIds) <- Map.assocs $ mba $ H.txTransfers history
    customer <- maybeToList $ resolveAddress a
    (txId, (slot, value)) <-
        Map.assocs
            $ Map.intersectionWith (,) (events (H.txIds history)) txIds
    time <- maybeToList $ resolveSlot slot
    pure
        $ singletonMap customer
        $ singletonPatched (firstJust a) time
        $ singletonPatched (firstJust slot) txId
        $ Value value
