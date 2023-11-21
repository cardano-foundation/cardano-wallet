-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Abstract data type that describes a policy for keeping and discarding
-- checkpoints. To be used with the 'Checkpoints' type.
module Cardano.Wallet.Primitive.Types.Checkpoints.Policy
    ( BlockHeight
    , CheckpointPolicy
    , nextCheckpoint
    , keepWhereTip
    , toListAtTip

    -- * Construction
    , atGenesis
    , atTip
    , trailingArithmetic
    , sparseArithmetic
    , defaultPolicy
    , gapSize

    -- * Internal invariants
    -- $invariants
    ) where

import Prelude

import Data.List
    ( unfoldr
    )

{-------------------------------------------------------------------------------
    CheckpointPolicy, abstract data type
-------------------------------------------------------------------------------}
type BlockHeight = Integer

{-| [CheckpointPolicy]

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
newtype CheckpointPolicy = CheckpointPolicy
    { nextCheckpoint :: BlockHeight -> BlockHeight -> Maybe BlockHeight
        -- ^ Assuming that the tip of the chain is at block height @tip@,
        -- @nextCheckpoint policy tip height@ returns the smallest
        -- @height'@ satisfying @height' >= height#
        -- at which the next checkpoint is to be made.
    }

{-$invariants

Internal invariants of the 'CheckpointPolicy' type:

* 'prop_monotonicHeight' — 'nextCheckpoint' returns the same height
  for all heights between a given height and the height returned.
* prop_monotonicTip' — when increasing the @tip@ height, 'nextCheckpoint'
  will never return a blockheight that is smaller.
-}

-- | Assuming that the tip of the chain is at block height @tip@,
-- the value @keepWhereTip policy tip height@
-- indicates whether a checkpoint should ('True') or should not ('False')
-- be stored at @height@.
keepWhereTip
    :: CheckpointPolicy -> BlockHeight -> BlockHeight
    -> Bool
keepWhereTip policy tip height =
    nextCheckpoint policy tip height == Just height

-- | List all checkpoints for a given tip.
toListAtTip :: CheckpointPolicy -> BlockHeight -> [BlockHeight]
toListAtTip policy tip = unfoldr (fmap next . nextCheckpoint policy tip) 0
  where next x = (x,x+1)

{-------------------------------------------------------------------------------
    CheckpointPolicy, construction
-------------------------------------------------------------------------------}
-- | The combination of two 'CheckpointPolicy' makes a checkpoint
-- where at least one of the policies wants to make a checkpoint.
instance Semigroup CheckpointPolicy where
    p1 <> p2 = CheckpointPolicy $ \t h ->
        union min (nextCheckpoint p1 t h) (nextCheckpoint p2 t h)
      where
        union _ Nothing mb = mb
        union _ ma Nothing = ma
        union f (Just a) (Just b) = Just (f a b)

instance Monoid CheckpointPolicy where
    mempty = CheckpointPolicy $ \_ _ -> Nothing

-- | The 'CheckpointPolicy' that keeps only the genesis block.
atGenesis :: CheckpointPolicy
atGenesis = CheckpointPolicy $ \_tip height ->
    if height <= 0 then Just height else Nothing

-- | The 'CheckpointPolicy' that only keeps the tip of the chain.
atTip :: CheckpointPolicy
atTip = CheckpointPolicy $ \tip height ->
    if height <= tip then Just tip else Nothing

-- | @trailingArithmetic n height@ keeps @n@ checkpoints
-- at block heights that are multiples of @height@
-- and which are closest to the tip of the chain.
-- (Fewer than @n@ checkpoints are kept while the chain is too short
-- to accommodate all checkpoints.)
trailingArithmetic :: Integer -> BlockHeight -> CheckpointPolicy
trailingArithmetic n grid = CheckpointPolicy $ \tip height ->
    case [h | h <- window tip, h >= height] of
        [] -> Nothing
        (x:_) -> Just x
  where
    window tip = [a, a + grid .. tip]
      where
        m = n - 1
        a = if tip > m * grid then toGrid (tip - m * grid) else 0
    toGrid x = (x `div` grid) * grid

{- | Note [sparseArithmeticPolicy]

The 'sparseArithmetic' checkpoint policy contains essentially two
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

Roughly, the 'sparseArithmetic'

0 ..... N*largeGap .... (N+1)*largeGap .. .. M*smallGap (M+1)*smallGap tip
        |_______________________________________________________________|
                 epochStability

Note: In the event where chain following "fails completely" (because, for
example, the node has switch to a different chain, different by more than `k`),
we have no choice but rolling back from genesis.
Therefore, we need to keep the very first checkpoint in the database, no
matter what.
-}
sparseArithmetic :: BlockHeight -> CheckpointPolicy
sparseArithmetic epochStability =
    atGenesis
    <> atTip
    <> trailingArithmetic 10 1
    <> trailingArithmetic n largeGap
  where
    largeGap = gapSize epochStability
    n = epochStability `div` largeGap

-- | A sensible default checkpoint policy; currently 'sparseArithmetic'.
defaultPolicy :: BlockHeight -> CheckpointPolicy
defaultPolicy = sparseArithmetic

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
gapSize :: BlockHeight -> Integer
gapSize epochStability = max 1 (epochStability  `div` 3)
