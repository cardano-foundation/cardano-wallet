{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Copyright: © 2022 IOHK
License: Apache-2.0

Data type 'Submissions' for storing a set of submitted transactions.

-}
module Cardano.Wallet.Submissions.Submissions
    ( Submissions (..)
    , mkEmpty
    , tip
    , finality
    , tipL
    , finalityL
    , transactionsL
    , transactions
    , TxStatusMeta (..)
    , txStatus
    , txStatusMeta
    )
    where

import Prelude

import Cardano.Wallet.Submissions.TxStatus
    ( HasTxId (TxId)
    , TxStatus
    , TxStatuses
    )
import Control.Lens
    ( makeLenses
    , view
    )
import Data.Map.Strict
    ( Map
    )

import qualified Data.Map as Map

data TxStatusMeta meta slot tx = TxStatusMeta
    { _txStatus :: TxStatus slot tx
    , _txStatusMeta :: meta
    }
    deriving (Show, Eq, Functor)

makeLenses ''TxStatusMeta

-- | Data type for keeping track of transactions, both pending and in ledger.
data Submissions meta slot tx = Submissions
    { -- | Tracked transactions with their status.
        _transactionsL :: Map (TxId tx) (TxStatusMeta meta slot tx),
        -- | Current finality slot.
        -- No transactions before this slot are in control.
        _finalityL :: slot,
        -- | Current tip.
        -- All transactions in Ledger should be accepted up to here.
        _tipL :: slot
    }

deriving instance
    (HasTxId tx, Show slot, Show tx, Show meta) =>
    (Show (Submissions meta slot tx))

deriving instance
    (Eq slot, HasTxId tx, Eq tx, Eq meta) =>
    (Eq (Submissions meta slot tx))

makeLenses ''Submissions

-- | Create an empty 'Submissions' table with the same slot for
-- tip and finality.
mkEmpty :: slot -> Submissions meta slot tx
mkEmpty slot = Submissions Map.empty slot slot

-- | Current slot tip.
tip :: Submissions meta slot tx -> slot
tip = view tipL

-- | Current finality tip.
finality :: Submissions meta slot tx -> slot
finality = view finalityL

transactions :: Submissions meta slot tx -> TxStatuses slot tx
transactions = fmap (view txStatus) . view transactionsL
