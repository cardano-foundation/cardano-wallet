{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
-- Data type that represents a collection of checkpoints.
-- Each checkpoints is associated with a 'Slot'.

module Cardano.Wallet.DB.Checkpoints
    ( getPoint

    -- * Checkpoints  
    , Checkpoints
    , checkpoints
    , loadCheckpoints
    , fromGenesis
    , getLatest
    , findNearestPoint
    
    -- * Delta types
    , DeltaCheckpoints (..)
    , DeltaMap (..)
    ) where

import Prelude

import Data.Delta
    ( Delta (..) )
import Data.Generics.Internal.VL.Lens
    ( over, view )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.Model as W
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

{- NOTE [PointSlotNo]

'SlotNo' cannot represent the genesis point.

Historical hack. The DB layer can't represent 'Origin' in the database,
instead we have mapped it to 'SlotNo 0', which is wrong.

Rolling back to SlotNo 0 instead of Origin is fine for followers starting
from genesis (which should be the majority of cases). Other, non-trivial
rollbacks to genesis cannot occur on mainnet (genesis is years within
stable part, and there were no rollbacks in byron).

Could possibly be problematic in the beginning of a testnet without a
byron era. /Perhaps/ this is what is happening in the
>>> [cardano-wallet.pools-engine:Error:1293] [2020-11-24 10:02:04.00 UTC]
>>> Couldn't store production for given block before it conflicts with
>>> another block. Conflicting block header is:
>>> 5bde7e7b<-[f1b35b98-4290#2008]
errors observed in the integration tests.

The issue has been partially fixed in that 'rollbackTo' now takes
a 'Slot' as argument, which can represent the 'Origin'.
However, the database itself mostly stores slot numbers.

FIXME LATER during ADP-1043: As we move towards in-memory data,
all slot numbers in the DB file will either be replaced by
the 'Slot' type, or handled slightly differently when it
is clear that the data cannot exist at the genesis point
(e.g. for TxHistory).

-}

-- | Helper function: Get the 'Point' of a wallet state.
getPoint :: W.Wallet s -> W.Slot
getPoint =
    W.toSlot . W.chainPointFromBlockHeader . view #currentTip

{-------------------------------------------------------------------------------
    Checkpoints
-------------------------------------------------------------------------------}
{- HLINT ignore Checkpoints "Use newtype instead of data" -}
-- | Collection of checkpoints indexed by 'Slot'.
data Checkpoints a = Checkpoints
    { checkpoints :: Map W.Slot a
    -- ^ Map of checkpoints. Always contains the genesis checkpoint.
    } deriving (Eq,Show,Generic)
-- FIXME LATER during ADP-1043:
--  Use a more sophisticated 'Checkpoints' type that stores deltas.

-- | Turn the list of checkpoints into a map of checkpoints.
--
-- FIXME LATER during ADP-1043:
--   The database actually does not store the checkpoint at genesis,
--   but the checkpoint after that.
--   Hence, this function does not check whether the genesis checkpoint
--   is in the list of checkpoints.
loadCheckpoints :: [(W.Slot, a)] -> Checkpoints a
loadCheckpoints = Checkpoints . Map.fromList

-- | Begin with the genesis checkpoint.
fromGenesis :: a -> Checkpoints a
fromGenesis a = Checkpoints $ Map.singleton W.Origin a

-- | Get the checkpoint with the largest 'SlotNo'.
getLatest :: Checkpoints a -> (W.Slot, a)
getLatest = from . Map.lookupMax . view #checkpoints 
  where
    from = fromMaybe (error "getLatest: there should always be at least a genesis checkpoint")

-- | Find the nearest 'Checkpoint' that is either at the given point or before.
findNearestPoint :: Checkpoints a -> W.Slot -> Maybe W.Slot
findNearestPoint m key = fst <$> Map.lookupLE key (view #checkpoints m)

{-------------------------------------------------------------------------------
    Delta type for Checkpoints
-------------------------------------------------------------------------------}
data DeltaCheckpoints a
    = PutCheckpoint W.Slot a
    | RollbackTo W.Slot
        -- Rolls back to the latest checkpoint at or before this slot.
    | RestrictTo [W.Slot]
        -- ^ Restrict to the intersection of this list with
        -- the checkpoints that are already present.
        -- The genesis checkpoint will always be present.

instance Delta (DeltaCheckpoints a) where
    type Base (DeltaCheckpoints a) = Checkpoints a
    apply (PutCheckpoint pt a) = over #checkpoints $ Map.insert pt a
    apply (RollbackTo pt) = over #checkpoints $
        Map.filterWithKey (\k _ -> k <= pt)
    apply (RestrictTo pts) = over #checkpoints $ \m ->
        Map.restrictKeys m $ Set.fromList (W.Origin:pts)

{-------------------------------------------------------------------------------
    A Delta type for Maps
-------------------------------------------------------------------------------}
-- | Delta type for 'Map'.
data DeltaMap key da
    = Insert key (Base da)
    | Delete key
    | Adjust key da

instance (Ord key, Delta da) => Delta (DeltaMap key da) where
    type Base (DeltaMap key da) = Map key (Base da)
    apply (Insert key a) = Map.insert key a
    apply (Delete key) = Map.delete key
    apply (Adjust key da) = Map.adjust (apply da) key
