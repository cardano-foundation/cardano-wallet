{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use let" #-}

module Cardano.Wallet.Deposit.Testing.DSL.ByTime
    ( -- * ByTime
      ByTimeM
    , ByTimeMContext (..)
    , ByTime

      -- * At time
    , atBlock
    , atSlot
    , newByTime

      -- * For customer
    , forCustomer

      -- * In tx
    , inTx

      -- * Value transfer
    , deposited
    , withdrawn
    )
where

import Prelude

import Cardano.Wallet.Deposit.Map
    ( Map (Map, Value)
    , W
    , toFinger
    )
import Cardano.Wallet.Deposit.Pure
    ( Customer
    , ValueTransfer (received, spent)
    )
import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( ByTime
    , DownTime
    , firstJust
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    )
import Cardano.Wallet.Deposit.Testing.DSL.Types
    ( BlockI
    , TxI
    )
import Cardano.Wallet.Deposit.Write
    ( mkAda
    )
import Cardano.Wallet.Read
    ( Slot
    , TxId
    , WithOrigin (..)
    )
import Control.Monad.Reader
    ( Reader
    , asks
    )
import Control.Monad.State
    ( State
    , StateT
    , execState
    , execStateT
    , modify'
    )
import Control.Monad.Trans
    ( MonadTrans (..)
    )
import Data.Map.Monoidal.Strict
    ( MonoidalMap
    )
import Data.Monoid
    ( First
    )
import Data.Ord
    ( Down (..)
    )
import Data.Time
    ( UTCTime
    )

import qualified Data.Map.Monoidal.Strict as MonoidalMap

-- -------------------------------------------------------------------------------
-- -- AtTime
-- -------------------------------------------------------------------------------

data ByTimeMContext = ByTimeMContext
    { txIdOfTxI :: TxI -> TxId
    , addrOfCustomer :: Customer -> Address
    , timeOfSlot :: Slot -> WithOrigin UTCTime
    , slotOfBlock :: BlockI -> Slot
    }

type ByTimeM = Reader ByTimeMContext

atBlock
    :: BlockI
    -> StateT
        (MonoidalMap Customer (Map '[W (First Address) TxId] ValueTransfer))
        ByTimeM
        ()
    -> StateT
        ( MonoidalMap
            DownTime
            (Map '[W (First Slot) Customer, W (First Address) TxId] ValueTransfer)
        )
        ByTimeM
        ()
atBlock b v = do
    slotOf <- asks slotOfBlock
    atSlot (slotOf b) v

atSlot
    :: Slot
    -> StateT
        (MonoidalMap Customer (Map '[W (First Address) TxId] ValueTransfer))
        ByTimeM
        ()
    -> StateT
        ( MonoidalMap
            DownTime
            (Map '[W (First Slot) Customer, W (First Address) TxId] ValueTransfer)
        )
        ByTimeM
        ()
atSlot t v = do
    timeOf <- asks timeOfSlot
    txs <- lift $ newCustomers t v
    modify' $ MonoidalMap.insert (Down $ timeOf t) txs

newByTime
    :: StateT
        ( MonoidalMap
            DownTime
            (Map '[W (First Slot) Customer, W (First Address) TxId] ValueTransfer)
        )
        ByTimeM
        ()
    -> ByTimeM ByTime
newByTime v = toFinger . Map () <$> execStateT v mempty

-- -------------------------------------------------------------------------------
-- -- Customer
-- -------------------------------------------------------------------------------

forCustomer
    :: Customer
    -> StateT
        (MonoidalMap TxId (Map '[] ValueTransfer))
        ByTimeM
        ()
    -> StateT
        (MonoidalMap Customer (Map '[W (First Address) TxId] ValueTransfer))
        ByTimeM
        ()
forCustomer c v = do
    addrOf <- asks addrOfCustomer
    txs <- lift $ newTxIds (addrOf c) v
    modify' $ MonoidalMap.insert c txs

newCustomers
    :: Slot
    -> StateT
        (MonoidalMap Customer (Map '[W (First Address) TxId] ValueTransfer))
        ByTimeM
        ()
    -> ByTimeM
        (Map '[W (First Slot) Customer, W (First Address) TxId] ValueTransfer)
newCustomers slot v = Map (firstJust slot) <$> execStateT v mempty

-------------------------------------------------------------------------------
-- Tx
-------------------------------------------------------------------------------

inTx
    :: TxI
    -> State ValueTransfer ()
    -> StateT
        (MonoidalMap TxId (Map '[] ValueTransfer))
        ByTimeM
        ()
inTx tx v = do
    w <- pure $ newValueTransferP v
    txIdOf <- asks txIdOfTxI
    modify' $ MonoidalMap.insert (txIdOf tx) w

newTxIds
    :: Address
    -> StateT
        (MonoidalMap TxId (Map '[] ValueTransfer))
        ByTimeM
        ()
    -> ByTimeM (Map '[W (First Address) TxId] ValueTransfer)
newTxIds addr v = Map (firstJust addr) <$> execStateT v mempty

-------------------------------------------------------------------------------
-- Value transfer
-------------------------------------------------------------------------------

deposited :: Int -> State ValueTransfer ()
deposited n = modify' $ \s -> s{received = mkAda $ fromIntegral n}

withdrawn :: Int -> State ValueTransfer ()
withdrawn n = modify' $ \s -> s{spent = mkAda $ fromIntegral n}

newValueTransferP
    :: State ValueTransfer ()
    -> Map '[] ValueTransfer
newValueTransferP v = Value $ execState v mempty
