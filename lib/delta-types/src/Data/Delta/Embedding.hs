{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-|
Copyright: © 2021-2023 IOHK, 2024 Cardano Foundation
License: Apache-2.0

Embeddings of delta types.
-}
module Data.Delta.Embedding (
    -- $Embedding
    -- * Embedding
      Embedding
    , module Data.Semigroupoid
    , Embedding' (..)
    , mkEmbedding
    , fromEmbedding
    , pair
    , liftUpdates
    , replaceFromApply

    -- * Internal
    , inject
    , project
    , Machine (..)
    , idle
    , pairMachine
    , fromState
    ) where

import Prelude

import Control.Exception
    ( SomeException
    )
import Data.Delta.Core
    ( Delta (..)
    , Replace (..)
    )
import Data.Either
    ( fromRight
    )
import Data.Semigroupoid
    ( Semigroupoid (..)
    )

{-------------------------------------------------------------------------------
    Embedding
-------------------------------------------------------------------------------}
{- $Embedding

An 'Embedding'@ da db@ embeds one type and its delta type @da@
into another type and its delta type @db@.

For reasons of efficiency, 'Embedding' is an abstract type.
It is constructed using the 'Embedding'' type, which has
three components.

* 'write' embeds values from the type @a = 'Base' da@
    into the type @b = 'Base' db@.
* 'load' attempts to retrieve the value of type @a@
    from the type @b@, but does not necessarily succeed.
* 'update' maps a delta type @da@ to a delta type @db@.
    For this mapping, both the value of type @a@ and a corresponding
    value of type @b@ are provided;
    the delta types @da@ and @db@ are
    relative to these values.

The embedding of one type into the other is characterized by the following
properties:

* The embedding is __not necessarily surjective__:
    The type @b@ may contain many values that do not correspond to
    a value of type @a@. Hence, 'load' has an 'Either' result.
    (See Note [EitherSomeException] for the choice of exception type.)
    However, retrieving a written value always succeeds, we have

        prop> load . write = Right

* The embedding is __redundant__:
    The type @b@ may contain multiple values that correspond to
    one and the same @a@.
    This is why the 'update' function expects the type @b@ as argument,
    so that the right deltas can be computed.
    Put differently, we often have

        prop> write a ≠ b   where Right a = load b

* The embedding of a delta __commutes with 'apply'__.
    We have

        > Just (apply da a) = load (apply (update a b da) b)
        >     where Right a = load b

    However, since the embedding is redundant, we often have

        prop> apply (update a (write a) da) (write a) ≠ write (apply da a)
-}

-- | Specification of an embedding of a type @a@ with delta types @da@
-- into the type @b@ with delta type @db@.
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
    -> [da]
    -- ^ List of deltas to apply.
    -- The deltas are applied right-to-left; the 'head' is applied __last__.
    -> Base da
    -- ^ Base value to apply the deltas to.
    -> (Base db, [db])
    -- ^ (Final base value, updates that were applied ('head' is __last__)).
liftUpdates Embedding{inject} das0 a0 =
    let (b,dbs) = go (inject a0) a0 (reverse das0) in (b, reverse dbs)
  where
    go machine1 _  [] = (state_ machine1, [])
    go machine1 !a (da:das) = (b,db:dbs)
      where
        (b ,dbs) = go machine2 (apply da a) das
        (db,machine2) = step_ machine1 (a,da)

-- | Having an 'apply' function is equivalent to the existence
-- of a canonical embedding into the trivial 'Replace' delta type.
replaceFromApply :: (Delta da, a ~ Base da) => Embedding' da (Replace a)
replaceFromApply = Embedding'
    { load = Right
    , write = id
    , update = \_ a da -> Replace (apply da a)
    }

{-
-- | Use the 'update' function of an 'Embedding' to convert
-- one delta type to another.
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
-- Strict pair.
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

-- | Pair two 'Machine's.
pairMachine
    :: Machine da1 db1 -> Machine da2 db2 -> Machine (da1,da2) (db1,db2)
pairMachine (Machine s1 step1) (Machine s2 step2) =
    Machine (s1,s2) $ \((a1,a2), (da1,da2)) ->
        let (db1, m1) = step1 (a1,da1)
            (db2, m2) = step2 (a2,da2)
        in  ((db1,db2), pairMachine m1 m2)

-- | Create a 'Machine' from a specific state @s@,
-- and the built-in state 'Base'@ db@.
fromState
    :: Delta db
    => ((Base da, da) -> (Base db, s) -> (db, s))
    -> (Base db, s)
    -> Machine da db
fromState step (b,s0) = Machine b $ \ada ->
    case step ada (b,s0) of
        (db,s1) -> (db, fromState step (apply db b,s1))
