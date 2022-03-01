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

module Cardano.Wallet.DB.Checkpoints
    ( -- * Checkpoints  
      Checkpoints
    , checkpoints
    , loadCheckpoints
    , fromGenesis
    , getLatest
    , findNearestPoint
    
    -- * Delta types
    , DeltaCheckpoints
    , DeltaCheckpoints1 (..)

    -- * Checkpoint hygiene
    , extendAndPrune

    -- * Internal / Testing
    , CheckpointPolicy (..)
    , gapSize
    , sparseArithmeticPolicy
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
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word32 )
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
type DeltaCheckpoints a = [DeltaCheckpoints1 a]

data DeltaCheckpoints1 a
    = PutCheckpoint W.Slot a
    | RollbackTo W.Slot
        -- Rolls back to the latest checkpoint at or before this slot.
    | RestrictTo [W.Slot]
        -- ^ Restrict to the intersection of this list with
        -- the checkpoints that are already present.
        -- The genesis checkpoint will always be present.

instance Delta (DeltaCheckpoints1 a) where
    type Base (DeltaCheckpoints1 a) = Checkpoints a
    apply (PutCheckpoint pt a) = over #checkpoints $ Map.insert pt a
    apply (RollbackTo pt) = over #checkpoints $
        Map.filterWithKey (\k _ -> k <= pt)
    apply (RestrictTo pts) = over #checkpoints $ \m ->
        Map.restrictKeys m $ Set.fromList (W.Origin:pts)

instance Buildable (DeltaCheckpoints1 a) where
    build (PutCheckpoint slot _) = "PutCheckpoint " <> build slot
    build (RollbackTo slot) = "RollbackTo " <> build slot
    build (RestrictTo slots) = "RestrictTo " <> listF slots

{-------------------------------------------------------------------------------
    Checkpoint hygiene
-------------------------------------------------------------------------------}
type BlockHeight = Quantity "block" Word32

-- | Extend the known checkpoints and prune past ones.
extendAndPrune
    :: (a -> W.Slot)
    -> (a -> BlockHeight)
        -- ^ Convert checkpoint to block height.
    -> BlockHeight
        -- ^ Epoch stability window
    -> BlockHeight
        -- ^ Current tip of the blockchain
    -> NE.NonEmpty a
        -- ^ Checkpoints, ordered by increasing @Slot@.
    -> Checkpoints a -> DeltaCheckpoints a
extendAndPrune getSlot getHeight epochStability tip xs (Checkpoints cps) =
    prunes ++ additions
  where
    additions = reverse -- largest slot needs to be applied last
        [ PutCheckpoint (getSlot x) x | x <- new ]
    prunes = [ RestrictTo $ map getSlot (old ++ new) ]

    new    = filter willKeep (NE.toList xs)
    old    = filter willKeep (Map.elems cps)

    latest = NE.last xs
    isLatest x = getHeight x == getHeight latest

    policy = sparseArithmeticPolicy epochStability
    willKeep x = isLatest x || keepWhereTip policy (getHeight x) tip
        -- We must keep the most recent checkpoint or nothing will be extended

{- | Note [CheckpointPolicy]

To save memory and time, we do not store every checkpoint.
Instead, a 'CheckpointPolicy' determines which checkpoints
to store and which ones to discard.
The 'extendAndPrune' functions consults such a policy and
drops checkpoints as it deems necessary.

A 'CheckpointPolicy' determines whether a checkpoint is worth storing
only based on its block height. The boolean

  keepWhereTip policy tip blockheight

indicates whether the checkpoint should be stored ('True') or
not ('False').
It is important that this function does not oscillate:
If @blockheight <= tip@, the function result may change from 'True'
to 'False' as the @tip@ increases, but not the other way round.
This is because we can only create checkpoints the first time we
read the corresponding block.

TODO:
The 'Checkpoints' collection currently relies on 'Slot' instead
of 'BlockHeight' to store checkpoints. We need to better integrate
this with 'BlockHeight'.

I (Heinrich) actually prefer 'Slot'. However, not every slot contains a block,
and we would lose too many checkpoints if we based the decision of
whether to keep a checkpoint or not based on the slot number alone.
In contrast, block height is "dense".
-}
newtype CheckpointPolicy
    = CheckpointPolicy { keepWhereTip :: BlockHeight -> BlockHeight -> Bool }

{- | Note [sparseArithmeticPolicy]

The 'sparseArithmeticPolicy' checkpoint policy contains essentially two
sets of checkpoints: One fairly dense set near the tip of the chain
in order to handle frequent potential rollbacks, and one sparse
set that spans the entire epoch stability window. These two sets
are arranged as arithmetic sequences.

This policy is motivated by the following observations:

  - We can't rollback for more than `k = epochStability` blocks in the past
  - It is pretty fast to re-sync a few hundred blocks
  - Small rollbacks near the tip may occur more often than long ones

Hence, we should strive to

- Prune any checkpoint that are more than `k` blocks in the past
- Keep only one checkpoint every `largeGap` ~100 blocks
- But still keep ~10 most recent checkpoints to cope with small rollbacks.

Roughly, the 'sparseArithmeticPolicy' 

0 ..... N*largeGap .... (N+1)*largeGap .. .. M*smallGap (M+1)*smallGap tip
        |_______________________________________________________________|
                 epochStability

Note: In the event where chain following "fails completely" (because, for
example, the node has switch to a different chain, different by more than `k`),
we have no choice but rolling back from genesis.
Therefore, we need to keep the very first checkpoint in the database, no
matter what.

-}
sparseArithmeticPolicy :: BlockHeight -> CheckpointPolicy
sparseArithmeticPolicy epochStability = CheckpointPolicy $ \height tip ->
    keep (getQuantity height) (getQuantity tip)
  where
    smallGap = 1
    largeGap = (gapSize epochStability `div` smallGap) * smallGap
        -- integer multiple of smallGap for better retention

    keep height tip
        = notFuture && (
            isOrigin
            || isTip
            || inWindow (5*smallGap) smallGap
            || inWindow (getQuantity epochStability) largeGap
        )
      where
        isTip = height == tip
        isOrigin = height == 0
        notFuture = height <= tip
        inWindow width gap =
            (tip <= height + width + gap-1) && (height `divisibleBy` gap)
        divisibleBy a b = a `mod` b == 0

{- | A reasonable gap size used internally in 'sparseArithmeticPolicy'.

'Reasonable' means that it's not _too frequent_ and it's not too large. A
value that is too small in front of k would require generating much more
checkpoints than necessary.

A value that is larger than `k` may have dramatic consequences in case of
deep rollbacks.

As a middle ground, we current choose `k / 3`, which is justified by:

- The current bandwidth of the network layer (several thousands blocks per seconds)
- The current value of k = 2160

So, `k / 3` = 720, which corresponds to around a second of time needed to catch
up in case of large rollbacks (if our local node has caught up already).
-}
gapSize :: BlockHeight -> Word32
gapSize epochStability = max 1 (getQuantity epochStability  `div` 3)
