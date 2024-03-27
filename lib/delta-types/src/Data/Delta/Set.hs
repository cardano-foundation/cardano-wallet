{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Copyright: © 2021-2023 IOHK, 2024 Cardano Foundation
License: Apache-2.0

Delta types for 'Set'.
-}
module Data.Delta.Set
    ( -- * Single element
      DeltaSet1 (..)
    -- $DeltaSet1-laws

    -- * Multiple elements
    , DeltaSet
    , diffSet
    , listFromDeltaSet
    , deltaSetFromList
    ) where

import Prelude

import Data.Delta.Core
    ( Delta (..)
    )
import Data.Set
    ( Set
    )

import qualified Data.Set as Set

{-------------------------------------------------------------------------------
    DeltaSet
-------------------------------------------------------------------------------}

-- | Delta type for 'Set' where a single element is deleted or added.
data DeltaSet1 a
    = Insert a
    | Delete a
    deriving (Eq, Ord, Show)

instance Ord a => Delta (DeltaSet1 a) where
    type Base (DeltaSet1 a) = Set a
    apply (Insert a) = Set.insert a
    apply (Delete a) = Set.delete a

-- | Delta type for a 'Set' where
-- collections of elements are inserted or deleted.
data DeltaSet a = DeltaSet
    { inserts :: Set a
    , deletes :: Set a
    -- INVARIANT: The two sets are always disjoint.
    }
    deriving (Eq)

instance Ord a => Delta (DeltaSet a) where
    type Base (DeltaSet a) = Set a
    apply (DeltaSet i d) x = i `Set.union` (x `Set.difference` d)

-- | The smallest delta that changes the second argument to the first argument.
--
-- prop> new = apply (diffSet new old) old
-- prop> diffSet (Set.fromList "ac") (Set.fromList "ab") = deltaSetFromList [Insert 'c', Delete 'b']
diffSet :: Ord a => Set a -> Set a -> DeltaSet a
diffSet new old =
    DeltaSet
        { inserts = new `Set.difference` old
        , deletes = old `Set.difference` new
        }

-- | Flatten a 'DeltaSet' to a list of 'DeltaSet1'.
--
-- In the result list, the set of @a@ appearing as 'Insert'@ a@
-- is /disjoint/ from the set of @a@ appearing as 'Delete'@ a@.
listFromDeltaSet :: DeltaSet a -> [DeltaSet1 a]
listFromDeltaSet DeltaSet{inserts,deletes} =
    map Insert (Set.toList inserts) <> map Delete (Set.toList deletes)

-- | Collect insertions or deletions of elements into a 'DeltaSet'.
--
-- To save space, combinations of 'Insert' and 'Delete'
-- for the same element are simplified when possible.
-- These simplifications always preserve the property
--
-- prop> apply (deltaSetFromList ds) = apply ds
deltaSetFromList :: Ord a => [DeltaSet1 a] -> DeltaSet a
deltaSetFromList = foldr step empty
  where
    empty = DeltaSet Set.empty Set.empty
    step (Insert a) (DeltaSet i d) = DeltaSet (Set.insert a i) (Set.delete a d)
    step (Delete a) (DeltaSet i d) = DeltaSet (Set.delete a i) (Set.insert a d)

-- Note [DeltaSet1 Laws]
{-$DeltaSet1-laws

The following cancellation laws hold:

prop> apply [Insert a, Delete a] = apply (Insert a)
prop> apply [Insert a, Insert a] = apply (Insert a)
prop> apply [Delete a, Insert a] = apply (Delete a)
prop> apply [Delete a, Delete a] = apply (Delete a)

-}

-- | Remember that the semigroup instance is required to satisfy
-- the following properties:
--
-- prop> apply mempty = id
-- prop> apply (d1 <> d2) = apply d1 . apply d2
instance Ord a => Semigroup (DeltaSet a) where
    (DeltaSet i1 d1) <> (DeltaSet i2 d2) = DeltaSet
        (i1 `Set.union` (i2 `Set.difference` d1))
        (d1 `Set.union` (d2 `Set.difference` i1))
        -- This takes into account [DeltaSet1 Cancellations]

instance Ord a => Monoid (DeltaSet a) where
    mempty = DeltaSet Set.empty Set.empty
