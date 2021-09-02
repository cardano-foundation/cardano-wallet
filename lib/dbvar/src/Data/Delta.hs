{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Delta (
    -- * Synopsis
    -- | Delta encodings.
    --
    -- The type constraint 'Delta'@ delta@ means that the type @delta@
    -- is a delta encoding of the corresponding base type 'Base'@ delta@.
    --
    -- Delta encodings can be transformed into each other using an 'Embedding'.
    
    -- * Delta
    Delta (..)
    , NoChange (..), Replace (..)
    , DeltaList (..)
    , DeltaSet1 (..)
    , DeltaSet, mkDeltaSet, deltaSetToList, deltaSetFromList
    
    -- * Embedding
    , Embedding (..)
    , module Data.Semigroupoid
    , replaceFromApply
    ) where

import Prelude

import Control.Monad ((>=>))
import Data.Kind ( Type )
import Data.Semigroupoid ( Semigroupoid (..) )
import Data.Set ( Set )

import qualified Data.Set as Set

{-------------------------------------------------------------------------------
    Delta encodings
-------------------------------------------------------------------------------}
-- | Type class for delta encodings.
class Delta delta where
    -- | Base type for which @delta@ represents a delta encoding.
    -- This is implemented as a type family, so that we can have
    -- multiple delta encodings for the same base type.
    -- 
    -- FIXME: Better name for 'Base'? It's sooo generic.
    -- Pier, dock, ground, anchor, site, …
    type Base delta :: Type
    -- | Apply a delta encoding to the base type.
    apply :: delta -> Base delta -> Base delta

-- | Trivial delta encoding for the type @a@ that admits no change at all.
data NoChange a = NoChange
    deriving (Eq, Ord, Show)

instance Delta (NoChange a) where
    type instance Base (NoChange a) = a
    apply _ a = a

-- | Trivial delta encoding for the type @a@ that replaces the value wholesale.
newtype Replace a = Replace a
    deriving (Eq, Ord, Show)

instance Delta (Replace a) where
    type instance Base (Replace a) = a
    apply (Replace a) _ = a

-- | A list of deltas can be applied like a single delta.
-- This overloading of 'apply' is very convenient.
--
-- Order is important: The 'head' of the list is applied __last__.
-- This way, we have a morphism to the 'Endo' 'Monoid':
--
-- > apply []         = id
-- > apply (d1 <> d2) = apply d1 . apply d2
instance Delta delta => Delta [delta] where
    type instance Base [delta] = Base delta
    apply = foldr (.) id . map apply

-- | A pair of deltas represents a delta for a pair.
instance (Delta d1, Delta d2) => Delta (d1,d2) where
    type instance Base (d1, d2) = (Base d1, Base d2)
    apply (d1,d2) (a1,a2) = (apply d1 a1, apply d2 a2)

-- | Delta encoding for lists where a list of elements is prepended.
data DeltaList a = Append [a]
    deriving (Eq, Ord, Show)

instance Delta (DeltaList a) where
    type instance Base (DeltaList a) = [a]
    apply (Append xs) ys = xs ++ ys

-- | Delta encoding for 'Set' where a single element is deleted or added.
data DeltaSet1 a = Insert a | Delete a
    deriving (Eq, Ord, Show)

instance Ord a => Delta (DeltaSet1 a) where
    type instance Base (DeltaSet1 a) = Set a
    apply (Insert a) = Set.insert a
    apply (Delete a) = Set.delete a

-- | Delta encoding for a 'Set' where
-- collections of elements are inserted or deleted.
data DeltaSet a = DeltaSet
    { inserts :: Set a
    , deletes :: Set a
    } deriving (Eq)
-- INVARIANT: The two sets are always disjoint.

instance Ord a => Delta (DeltaSet a) where
    type instance Base (DeltaSet a) = Set a
    apply (DeltaSet i d) x = i `Set.union` (x `Set.difference` d)

-- | Delta to get from the second argument to the first argument.
mkDeltaSet :: Ord a => Set a -> Set a -> DeltaSet a
mkDeltaSet new old =
    DeltaSet (new `Set.difference` old) (old `Set.difference` new)

-- | Flatten a 'DeltaSet' to a list of 'DeltaSet1'.
--
-- In the result list, the set of @a@ appearing as 'Insert'@ a@
-- is /disjoint/ from the set of @a@ appearing as 'Delete'@ a@.
deltaSetToList :: Ord a => DeltaSet a -> [DeltaSet1 a]
deltaSetToList DeltaSet{inserts,deletes} =
    map Insert (Set.toList inserts) <> map Delete (Set.toList deletes)

-- | Collect insertions or deletions of elements into a 'DeltaSet'.
--
-- To save space, combinations of 'Insert' and 'Delete'
-- for the same element are simplified when possible.
-- These simplifications always preserve the property
--
-- > apply (deltaSetFromList ds) = apply ds
deltaSetFromList :: Ord a => [DeltaSet1 a] -> DeltaSet a
deltaSetFromList = foldr step empty
  where
    empty = DeltaSet Set.empty Set.empty
    step (Insert a) (DeltaSet i d) = DeltaSet (Set.insert a i) (Set.delete a d)
    step (Delete a) (DeltaSet i d) = DeltaSet (Set.delete a i) (Set.insert a d)

{- Note [DeltaSet1 Cancellations]

The following cancellation laws hold:

  apply [Insert a, Delete a] = apply (Insert a)
  apply [Insert a, Insert a] = apply (Insert a)
  apply [Delete a, Insert a] = apply (Delete a)
  apply [Delete a, Delete a] = apply (Delete a)

-}

-- | 'apply' distributes over '(<>)'.
instance Ord a => Semigroup (DeltaSet a) where
    (DeltaSet i1 d1) <> (DeltaSet i2 d2) = DeltaSet
        (i1 `Set.union` (i2 `Set.difference` d1))
        (d1 `Set.union` (d2 `Set.difference` i1))
        -- This takes into account [DeltaSet1 Cancellations]

instance Ord a => Monoid (DeltaSet a) where
    mempty = DeltaSet Set.empty Set.empty

{-------------------------------------------------------------------------------
    Embedding
-------------------------------------------------------------------------------}
-- | An 'Embedding'@ da db@ embeds one type and its delta encoding @da@
-- into another type and its delta encoding @db@.
--
-- An Embedding has three components:
--
-- * 'write' embeds values from the type @a = Base da@
-- into the type @b = Base bd@.
-- * 'load' attempts to retrieve the value of type @a@
-- from the type @b@, but does not necessarily succeed.
-- * 'update' maps a delta encoding @da@ to a delta encoding @db@.
--
-- The embedding of one type into the other is characterized by the following
-- properties:
--
-- * The embedding need __not__ be __surjective__:
--   The type @b@ may contain many values that do not correspond to
--   a value of type @a@. Hence, @load@ has a 'Maybe' result.
--   However, retrieving a written value always succeeds, we have
--
--       > load . write = Just
--
-- * The embedding is __redudant__:
--   The type @b@ may contain multiple values that correspond to
--   one and the same @a@.
--   This is why the 'update' function expects the type @b@ as argument,
--   so that the right delta encoding can be computed.
--   Put differently, we often have
--
--       > write a ≠ b   where Just a = load b
--
-- * The embedding of delta encoding __commutes with 'apply'__.
--   We have
--
--       > Just (apply da a) = load (apply (update a b da) b)
--       >     where Just a = load b
--
--      However, since the embedding is redundant, we often have
--
--      > apply (update a (write a) da) (write a) ≠ write (apply da a)
data Embedding da db where
    Embedding
        :: (Delta da, Delta db) =>
        { load   :: Base db -> Maybe (Base da)
        , write  :: Base da -> Base db
        , update :: Base da -> Base db -> da -> db
        } -> Embedding da db

-- | 'Embedding's can be composed with 'o'.
instance Semigroupoid Embedding where o = compose

compose :: Embedding db dc -> Embedding da db -> Embedding da dc
compose (Embedding load1 write1 update1) (Embedding load2 write2 update2) =
    Embedding
        { load = load1 >=> load2
        , write = write1 . write2
        , update = update_
        }
  where
    update_ a c da = update1 b c db
      where
        b  = maybe (error "Embedding: unlawful load") id $ load1 c
        db = update2 a b da

-- | Having an 'apply' function is equivalent to the existence
-- of a canonical embedding into the trivial 'Replace' delta encoding.
replaceFromApply :: (Delta da, a ~ Base da) => Embedding da (Replace a)
replaceFromApply = Embedding
    { load = Just
    , write = id
    , update = \_ a da -> Replace (apply da a)
    }

{-
-- | Use the 'update' function of an 'Embedding' to convert
-- one delta encoding to another.
-- 
-- This function assumes that the 'Embedding' argument satisfies
-- @load = Just@ and @write = id@.
applyWithEmbedding
    :: (Delta db, a ~ Base db)
    => Embedding da db -> (da -> a -> a)
applyWithEmbedding e delta1 a = apply (update e a delta1) a
-}