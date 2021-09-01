{-# LANGUAGE GADTs #-}
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
    , DeltaSet (..)
    
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

-- | Delta encoding for lists where a single element is prepended.
data DeltaList a = Cons a
    deriving (Eq, Ord, Show)

instance Delta (DeltaList a) where
    type instance Base (DeltaList a) = [a]
    apply (Cons x) xs = x : xs

-- | Delta encoding for 'Set' where an element is deleted or added.
data DeltaSet a = Insert a | Delete a
    deriving (Eq, Ord, Show)

instance Ord a => Delta (DeltaSet a) where
    type instance Base (DeltaSet a) = Set a
    apply (Insert a) = Set.insert a
    apply (Delete a) = Set.delete a

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