{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
-- This module provides a public API for planning wallet migrations.
--
-- Use 'createPlan' to create a migration plan.
--
module Cardano.Wallet.Primitive.Migration
    (
    -- * Creating a migration plan
      createPlan
    , MigrationPlan (..)
    , RewardWithdrawal (..)
    , Selection (..)

    ) where

import Prelude

import Cardano.Wallet.Primitive.Migration.Selection
    ( RewardWithdrawal (..), Selection (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxConstraints (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO )
import Control.DeepSeq
    ( NFData )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.Migration.Planning as Planning

-- | Represents a plan for migrating a 'UTxO' set.
--
-- See 'createPlan' to create a migration plan.
--
data MigrationPlan = MigrationPlan
    { selections :: ![Selection (TxIn, TxOut)]
      -- ^ A list of generated selections: each selection is the basis for a
      -- single transaction.
    , unselected :: !UTxO
      -- ^ The portion of the UTxO that was not selected.
    , totalFee :: !Coin
      -- ^ The total fee payable: equal to the sum of the fees of the
      -- individual selections.
    }
    deriving (Eq, Generic, Show)

instance NFData MigrationPlan

-- | Creates a migration plan for the given UTxO set and reward withdrawal
--   amount.
--
-- See 'MigrationPlan'.
--
createPlan
    :: TxConstraints
    -> UTxO
    -> RewardWithdrawal
    -> MigrationPlan
createPlan constraints utxo reward = MigrationPlan
    { selections = view #selections plan
    , unselected = Planning.uncategorizeUTxO (view #unselected plan)
    , totalFee = view #totalFee plan
    }
  where
    plan = Planning.createPlan
        constraints (Planning.categorizeUTxO constraints utxo) reward
