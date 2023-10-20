-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
--
-- Internals of 'UTxOHistory'.
module Cardano.Wallet.DB.Store.UTxOHistory.Model.Internal
    ( UTxOHistory (..)
    , Pruned (..)
    )
where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( Slot
    , SlotNo
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn
    )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO
    )
import Data.Map.Strict
    ( Map
    )
import Data.Set
    ( Set
    )

-- | The finality of the UTxO history.
data Pruned = PrunedUpTo SlotNo | NotPruned
    deriving (Show, Eq)

-- | UTxO history. Abstract history of the UTxO. We keep track of the creation
-- and spending of slot of each TxIn. This allows us to rollback to a given slot
-- and prune the history to a given slot.
data UTxOHistory = UTxOHistory
    { history :: UTxO
    -- ^ All UTxO , spent and unspent.
    , creationSlots :: Map Slot (Set TxIn)
    -- ^ All TxIn, indexed by creation slot.
    , creationTxIns :: Map TxIn Slot
    -- ^ Reverse map of the `creationSlots` map
    , spentSlots :: Map SlotNo (Set TxIn)
    -- ^ All spent TxIn, indexed by spent slot.
    , spentTxIns :: Map TxIn SlotNo
    -- ^ Reverse map of the `spentSlots` map.
    , tip :: Slot
    -- ^ Current tip slot.
    , finality :: Pruned
    -- ^ Finality slot.
    , boot :: UTxO
    -- ^ UTxO created at genesis.
    }
    deriving (Show, Eq)
