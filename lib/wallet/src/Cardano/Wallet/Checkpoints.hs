{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
-- Data type that represents a collection of checkpoints.
-- Each checkpoints is associated with a 'Slot'.

module Cardano.Wallet.Checkpoints
    ( -- * Checkpoints
      Checkpoints
    , checkpoints
    , loadCheckpoints
    , fromGenesis
    , getLatest
    , findNearestPoint

    -- * Delta types
    , DeltaCheckpoints (..)
    , DeltasCheckpoints

    -- * Checkpoint hygiene
    , BlockHeight
    , CheckpointPolicy
    , extendAndPrune
    ) where

import Prelude

import Cardano.Wallet.Checkpoints.Policy
    ( BlockHeight, CheckpointPolicy, keepWhereTip )
import Data.Delta
    ( Delta (..) )
import Data.Generics.Internal.VL.Lens
    ( over, view )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe )
import Fmt
    ( Buildable (..), listF )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.List.NonEmpty as NE
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

{-------------------------------------------------------------------------------
    Checkpoints
-------------------------------------------------------------------------------}
-- | Collection of checkpoints indexed by 'Slot'.
newtype Checkpoints a = Checkpoints
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
type DeltasCheckpoints a = [DeltaCheckpoints a]

data DeltaCheckpoints a
    = PutCheckpoint W.Slot a
        -- ^ Insert a checkpoint at a specified slot.
    | RollbackTo W.Slot
        -- ^ Rolls back to the latest checkpoint at or before this slot.
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

instance Buildable (DeltaCheckpoints a) where
    build (PutCheckpoint slot _) = "PutCheckpoint " <> build slot
    build (RollbackTo slot) = "RollbackTo " <> build slot
    build (RestrictTo slots) = "RestrictTo " <> listF slots

{-------------------------------------------------------------------------------
    Checkpoint hygiene
-------------------------------------------------------------------------------}

{- Note [extendAndPrune]

The function 'extendAndPrune' expects a list of new checkpoints that
are to be pruned and added to the existing checkpoints.

As a precondition, we assume that these new checkpoints
have been created at least at those block heights
specified by 'nextCheckpoint' from the 'CheckpointPolicy' argument.
Except for the most recent checkpoint,
the function 'extendAndPrune' will prune all checkpoints
whose block height does not align with the policy.
It's ok to supply a list of new checkpoints that is denser than required.
-}

-- | Extend the known checkpoints and prune unnecessary ones.
extendAndPrune
    :: (a -> W.Slot)
        -- ^ Convert checkpoint to slot.
    -> (a -> BlockHeight)
        -- ^ Convert checkpoint to block height.
    -> CheckpointPolicy
        -- ^ Policy to use for pruning checkpoints.
    -> BlockHeight
        -- ^ Current tip of the blockchain,
        -- which is *different* from block height of the latest checkpoint.
    -> NE.NonEmpty a
        -- ^ New checkpoints, ordered by increasing @Slot@.
        -- See Note [extendAndPrune].
    -> Checkpoints a
        -- ^ Current checkpoints.
    -> DeltasCheckpoints a
extendAndPrune getSlot getHeight policy nodeTip xs (Checkpoints cps) =
    prunes ++ additions
  where
    additions = reverse -- latest slot needs to be applied last
        [ PutCheckpoint (getSlot x) x | x <- new ]
    prunes = [ RestrictTo $ map getSlot (old ++ new) ]

    new = filter willKeep (NE.toList xs)
    old = filter willKeep (Map.elems cps)

    latest = NE.last xs
    isLatest x = getHeight x == getHeight latest

    willKeep x = isLatest x || keepWhereTip policy (getHeight x) nodeTip
        -- We must keep the most recent checkpoint or nothing will be extended
