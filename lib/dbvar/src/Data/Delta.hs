{-# LANGUAGE TypeFamilies #-}
module Data.Delta (
    -- * Synopsis
    -- | Delta encodings.
    -- The type constraint 'Delta'@ delta@ means that the type @delta@
    -- is a delta encoding of the corresponding base type 'Base'@ delta@.

    -- * Delta
    Delta (..)
    , NoChange (..), Replace (..)
    , DeltaList (..)
    , DeltaSet (..)
    -- * Embedding
    , Embedding (..)
    ) where

import Prelude

import Data.Kind ( Type )
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
--
-- This overloading of 'apply' is very useful,
-- and it is a morphism of 'Monoid':
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
-- | An 'Embedding' embeds a type @a1@ into a type @a2@,
-- but also embeds a delta encoding @delta1@ for the first type
-- into a delta encoding @delta2@ for the second type.
-- 
-- This abstraction is useful for transforming 'Store'
-- for different values.
--
-- An 'Embedding' is expected to satisfy several laws:
--
-- Embedding:
--
-- > load . write = Just
-- 
-- Commutes with 'apply':
--
-- > apply (update old delta) (write old) = write (apply delta old)
data Embedding a1 delta1 a2 delta2 = Embedding
    { load   :: a2 -> Maybe a1
    , write  :: a1 -> a2
    , update :: a1 -> delta1 -> delta2
    }
