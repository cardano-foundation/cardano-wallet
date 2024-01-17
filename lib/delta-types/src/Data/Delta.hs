{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{- HLINT ignore "Use newtype instead of data" -}
module Data.Delta (
    -- * Synopsis
    -- | Delta encodings.
    --
    -- The type constraint 'Delta'@ delta@ means that the type @delta@
    -- is a delta encoding of the corresponding base type 'Base'@ delta@.
    --
    -- Delta encodings can be transformed into each other using an 'Embedding'.

    -- * Delta encodings
    Delta (..)
    , NoChange (..), Replace (..)
    , DeltaList (..)
    , DeltaSet1 (..)
    , DeltaSet, mkDeltaSet, deltaSetToList, deltaSetFromList

    -- * Embedding of types and delta encodings
    -- $Embedding
    , Embedding
    , module Data.Semigroupoid
    , Embedding' (..), mkEmbedding
    , fromEmbedding, pair, liftUpdates
    , replaceFromApply

    -- * Internal
    , inject, project, Machine (..), idle, pairMachine, fromState,
    ) where

import Prelude

import Control.Exception
    ( SomeException
    )
import Data.Either
    ( fromRight
    )
import Data.Kind
    ( Type
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Semigroupoid
    ( Semigroupoid (..)
    )
import Data.Set
    ( Set
    )

import qualified Data.Set as Set

{-------------------------------------------------------------------------------
    Delta encodings
-------------------------------------------------------------------------------}
-- | Type class for delta encodings.
class Delta delta where
    -- | Base type for which @delta@ represents a delta encoding.
    -- This is implemented as a type family, so that we can have
    -- multiple delta encodings for the same base type.
    type Base delta :: Type
    -- | Apply a delta encoding to the base type.
    apply :: delta -> Base delta -> Base delta

-- | Trivial delta encoding for the type @a@ that admits no change at all.
data NoChange (a :: Type) = NoChange
    deriving (Eq, Ord, Show)

instance Delta (NoChange a) where
    type Base (NoChange a) = a
    apply _ a = a

-- | Trivial delta encoding for the type @a@ that replaces the value wholesale.
newtype Replace a = Replace a
    deriving (Eq, Ord, Show)

instance Delta (Replace a) where
    type Base (Replace a) = a
    apply (Replace a) _ = a

-- | Combine replacements. The first argument takes precedence.
-- In this way, 'apply' becomes a 'Semigroup' morphism.
instance Semigroup (Replace a) where
    r <> _ = r

-- | A delta can be optionally applied.
instance Delta delta => Delta (Maybe delta) where
    type Base (Maybe delta) = Base delta
    apply = maybe id apply

-- | A list of deltas can be applied like a single delta.
-- This overloading of 'apply' is very convenient.
--
-- Order is important: The 'head' of the list is applied __last__.
-- This way, we have a morphism to the 'Endo' 'Monoid':
--
-- > apply []         = id
-- > apply (d1 <> d2) = apply d1 . apply d2
instance Delta delta => Delta [delta] where
    type Base [delta] = Base delta
    apply ds a = foldr apply a ds

-- | For convenience, a nonempty list of deltas
-- can be applied like a list of deltas.
instance Delta delta => Delta (NonEmpty delta) where
    type Base (NonEmpty delta) = Base delta
    apply ds a = foldr apply a ds

-- | A pair of deltas represents a delta for a pair.
instance (Delta d1, Delta d2) => Delta (d1,d2) where
    type Base (d1, d2) = (Base d1, Base d2)
    apply (d1,d2) (a1,a2) = (apply d1 a1, apply d2 a2)

-- | A triple of deltas represents a delta for a triple.
instance (Delta d1, Delta d2, Delta d3) => Delta (d1,d2,d3) where
    type Base (d1,d2,d3) = (Base d1,Base d2,Base d3)
    apply (d1,d2,d3) (a1,a2,a3) = (apply d1 a1, apply d2 a2, apply d3 a3)

-- | A 4-tuple of deltas represents a delta for a 4-tuple.
instance (Delta d1, Delta d2, Delta d3, Delta d4) => Delta (d1,d2,d3,d4) where
    type Base (d1,d2,d3,d4) = (Base d1,Base d2,Base d3,Base d4)
    apply (d1,d2,d3,d4) (a1,a2,a3,a4) =
        (apply d1 a1, apply d2 a2, apply d3 a3, apply d4 a4)

-- | Delta encoding for lists where a list of elements is prepended.
data DeltaList a = Append [a]
    deriving (Eq, Ord, Show)

instance Delta (DeltaList a) where
    type Base (DeltaList a) = [a]
    apply (Append xs) ys = xs ++ ys

-- | Delta encoding for 'Set' where a single element is deleted or added.
data DeltaSet1 a = Insert a | Delete a
    deriving (Eq, Ord, Show)

instance Ord a => Delta (DeltaSet1 a) where
    type Base (DeltaSet1 a) = Set a
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
    type Base (DeltaSet a) = Set a
    apply (DeltaSet i d) x = i `Set.union` (x `Set.difference` d)

-- | Delta to get from the second argument to the first argument.
mkDeltaSet :: Ord a => Set a -> Set a -> DeltaSet a
mkDeltaSet new old =
    DeltaSet (new `Set.difference` old) (old `Set.difference` new)

-- | Flatten a 'DeltaSet' to a list of 'DeltaSet1'.
--
-- In the result list, the set of @a@ appearing as 'Insert'@ a@
-- is /disjoint/ from the set of @a@ appearing as 'Delete'@ a@.
deltaSetToList :: DeltaSet a -> [DeltaSet1 a]
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
{- $Embedding

An 'Embedding'@ da db@ embeds one type and its delta encoding @da@
into another type and its delta encoding @db@.

For reasons of efficiency, 'Embedding' is an abstract type.
It is constructed using the 'Embedding'' type, which has
three components.

* 'write' embeds values from the type @a = Base da@
    into the type @b = Base bd@.
* 'load' attempts to retrieve the value of type @a@
    from the type @b@, but does not necessarily succeed.
* 'update' maps a delta encoding @da@ to a delta encoding @db@.
    For this mapping, the value of type @a@ and a corresponding
    value of type @b@ are provided;
    the delta encodings of types @da@ and @db@ are
    relative to these values.

The embedding of one type into the other is characterized by the following
properties:

* The embedding need __not__ be __surjective__:
    The type @b@ may contain many values that do not correspond to
    a value of type @a@. Hence, 'load' has an 'Either' result.
    (See Note [EitherSomeException] for the choice of exception type.)
    However, retrieving a written value always succeeds, we have

        > load . write = Right

* The embedding is __redundant__:
    The type @b@ may contain multiple values that correspond to
    one and the same @a@.
    This is why the 'update' function expects the type @b@ as argument,
    so that the right delta encoding can be computed.
    Put differently, we often have

        > write a ≠ b   where Right a = load b

* The embedding of delta encoding __commutes with 'apply'__.
    We have

        > Just (apply da a) = load (apply (update a b da) b)
        >     where Right a = load b

    However, since the embedding is redundant, we often have

        > apply (update a (write a) da) (write a) ≠ write (apply da a)
-}

-- | Specification of an embedding of a type @a@ with delta encoding @da@
-- into the type @b@ with delta encoding @db@.
-- See [the discussion of @Embedding@](#g:3) for a more detailed description.
data Embedding' da db where
    Embedding'
        :: (Delta da, Delta db, a ~ Base da, b ~ Base db) =>
        { load   :: b -> Either SomeException a
        , write  :: a -> b
        , update :: a -> b -> da -> db
        } -> Embedding' da db

-- | 'Embedding' with efficient composition 'o'.
-- To construct an embedding, use 'mkEmbedding'.
data Embedding da db = Embedding
    { inject  :: Base da -> Machine da db
    , project :: Base db -> Either SomeException (Base da, Machine da db)
    }

-- | Construct 'Embedding' with efficient composition
mkEmbedding :: Embedding' da db -> Embedding da db
mkEmbedding Embedding'{load,write,update} = Embedding
    { inject = start . write
    , project = \b -> (, start b) <$> load b
    }
  where
    start b = fromState step (b,())
    step (a,da) (b,_) = (update a b da, ())

-- | Extract 'load', 'write', and 'update' functions
-- from an efficient 'Embedding'.
fromEmbedding :: (Delta da, Delta db) => Embedding da db -> Embedding' da db
fromEmbedding Embedding{inject,project} = Embedding'
    { load = fmap fst . project
    , write = state_ . inject
    , update = \a b da ->
        let (_ ,mab) = from (project b)
            (db,_  ) = step_ mab (a,da)
        in  db
    }
  where
    from = fromRight (error "Embedding: 'load' violates expected laws")

-- | Efficient composition of 'Embedding'
instance Semigroupoid Embedding where
    (Embedding inject2 project2) `o` (Embedding inject1 project1) =
        Embedding{inject,project}
      where
        inject a =
            let mab = inject1 a
                mbc = inject2 (state_ mab)
            in  mbc `o` mab
        project c = do
            (b, mbc) <- project2 c
            (a, mab) <- project1 b
            pure (a, mbc `o` mab)

-- | A pair of 'Embedding's gives an embedding of pairs.
pair :: Embedding da1 db1 -> Embedding da2 db2 -> Embedding (da1,da2) (db1,db2)
pair (Embedding inject1 project1) (Embedding inject2 project2) =
    Embedding{inject,project}
  where
    inject (a1,a2) = pairMachine (inject1 a1) (inject2 a2)
    project (b1,b2) = do
        (a1, m1) <- project1 b1
        (a2, m2) <- project2 b2
        pure ((a1,a2), pairMachine m1 m2)

-- | Lift a sequence of updates through an 'Embedding'.
liftUpdates
    :: Delta da
    => Embedding da db
    -> [da] -- ^ List of deltas to apply. The 'head' is applied /last/.
    -> Base da -- ^ Base value to apply the deltas to.
    -> (Base db, [db])
    -- ^ (Final base value, updates that were applied ('head' is /last/)).
liftUpdates Embedding{inject} das0 a0 =
    let (b,dbs) = go (inject a0) a0 (reverse das0) in (b, reverse dbs)
  where
    go machine1 _  [] = (state_ machine1, [])
    go machine1 !a (da:das) = (b,db:dbs)
      where
        (b ,dbs) = go machine2 (apply da a) das
        (db,machine2) = step_ machine1 (a,da)

-- | Having an 'apply' function is equivalent to the existence
-- of a canonical embedding into the trivial 'Replace' delta encoding.
replaceFromApply :: (Delta da, a ~ Base da) => Embedding' da (Replace a)
replaceFromApply = Embedding'
    { load = Right
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

{-------------------------------------------------------------------------------
    Machine (state machines) with efficient composition
-------------------------------------------------------------------------------}
-- | Strict pair.
-- If a value of this type is in WHNF, so are the two components.
-- data StrictPair a b = !a :*: !b
-- infixr 1 :*:

-- | A state machine that maps deltas to deltas.
-- This machine always carries a state of type 'Base'@ db@ around.
data Machine da db = Machine
    { state_ :: !(Base db)
    , step_  :: (Base da, da) -> (db, Machine da db)
    }

-- | Composition of 'Machine'
instance Semigroupoid Machine where
    (Machine c fbc) `o` (Machine b fab) = Machine c $ \ada ->
        case fab ada of
            (db, mab) -> case fbc (b,db) of
                (dc, mbc) -> (dc, mbc `o` mab)

-- | Identity machine starting from a base type.
idle :: Delta da => Base da -> Machine da da
idle a0 = Machine a0 $ \(a1,da) -> let a2 = apply da a1 in (da, idle a2)

-- | Pair two 'Machine'.
pairMachine
    :: Machine da1 db1 -> Machine da2 db2 -> Machine (da1,da2) (db1,db2)
pairMachine (Machine s1 step1) (Machine s2 step2) =
    Machine (s1,s2) $ \((a1,a2), (da1,da2)) ->
        let (db1, m1) = step1 (a1,da1)
            (db2, m2) = step2 (a2,da2)
        in  ((db1,db2), pairMachine m1 m2)

-- | Create a 'Machine' from a specific state 's',
-- and the built-in state 'Base'@ db@.
fromState
    :: Delta db
    => ((Base da, da) -> (Base db, s) -> (db, s))
    -> (Base db, s)
    -> Machine da db
fromState step (b,s0) = Machine b $ \ada ->
    case step ada (b,s0) of
        (db,s1) -> (db, fromState step (apply db b,s1))
