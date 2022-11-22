{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright: © 2022 IOHK
License: Apache-2.0

Data type 'Submissions' for storing a set of submitted transactions.

-}
module Cardano.Wallet.Submissions.Submissions
    ( Submissions (..)
    , tip
    , finality
    , tipL
    , finalityL
    , transactionsL
    , transactions)
    where

import Prelude

import Cardano.Wallet.Submissions.TxStatus
import Control.Lens
    ( makeLenses, view )
import Data.Map.Strict
    ( Map )

-- | Data type for keeping track of transactions, both pending and in ledger.
data Submissions slot tx = Submissions
    { -- | Tracked transactions with their status.
        _transactionsL :: Map (TxId tx) (TxStatus slot tx),
        -- | Current finality slot.
        -- No transactions before this slot are in control.
        _finalityL :: slot,
        -- | Current tip.
        -- All transactions in Ledger should be accepted up to here.
        _tipL :: slot
    }

deriving instance
    (Show slot, Show (TxId tx), Show tx) =>
    (Show (Submissions slot tx))

makeLenses ''Submissions

-- | Current slot tip.
tip :: Submissions slot tx -> slot
tip = view tipL

-- | Current finality tip.
finality :: Submissions slot tx -> slot
finality = view finalityL

transactions :: Submissions slot tx -> TxStatuses slot tx
transactions = view transactionsL
