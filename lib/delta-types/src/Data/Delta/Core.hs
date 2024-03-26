{-# LANGUAGE TypeFamilies #-}

{-|
Copyright: © 2021-2023 IOHK, 2024 Cardano Foundation
License: Apache-2.0
-}
module Data.Delta.Core
    ( Delta (..)
    , NoChange (..)
    , Replace (..)
    ) where

import Prelude

import Data.Kind
    ( Type
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Monoid
    ( Endo (..)
    )

{-------------------------------------------------------------------------------
    Delta types
-------------------------------------------------------------------------------}
-- | Type class for delta types.
class Delta delta where
    -- | Base type for which @delta@ represents a delta.
    -- This is implemented as a type family, so that we can have
    -- multiple delta types for the same base type.
    type Base delta :: Type
    -- | Apply a delta to the base type.
    --
    -- Whenever the type @delta@ is a 'Semigroup', we require that
    --
    -- prop> apply (d1 <> d2) = apply d1 . apply d2
    --
    -- This means that deltas are applied __right-to-left__:
    -- @d1@ is applied __after__ @d2@.
    --
    -- Whenever the type @delta@ is a 'Monoid', we require that
    --
    -- prop> apply mempty = id
    apply :: delta -> Base delta -> Base delta

-- | 'Endo' is the most general delta, which allows any change.
instance Delta (Endo a) where
    type Base (Endo a) = a
    apply (Endo f) = f

-- | The least general delta, where nothing is changed.
data NoChange (a :: Type) = NoChange
    deriving (Eq, Ord, Show)

-- | prop> apply NoChange a = a
instance Delta (NoChange a) where
    type Base (NoChange a) = a
    apply _ a = a

-- | Trivial delta type for the type @a@ that replaces the value wholesale.
newtype Replace a = Replace a
    deriving (Eq, Ord, Show)

-- |
-- prop> apply (Replace a) _ = a
instance Delta (Replace a) where
    type Base (Replace a) = a
    apply (Replace a) _ = a

-- | Combine replacements. The first argument takes precedence.
-- Hence, 'apply' is a morphism:
--
-- prop> apply (Replace a <> Replace b) = apply (Replace a) . apply (Replace b)
--
-- More strongly, we have
--
-- prop> apply (Replace a <> _) = apply (Replace a)
instance Semigroup (Replace a) where
    r <> _ = r

-- | A delta can be optionally applied.
instance Delta delta => Delta (Maybe delta) where
    type Base (Maybe delta) = Base delta
    apply = maybe id apply

-- | A list of deltas can be applied like a single delta.
-- This overloading of 'apply' is very convenient.
--
-- Order is important: The 'head' of the list is applied __last__,
-- so deltas are applied __right-to-left__.
-- Hence, 'apply' is a morphism
--
-- prop> apply []         = id
-- prop> apply (d1 ++ d2) = apply d1 . apply d2
instance Delta delta => Delta [delta] where
    type Base [delta] = Base delta
    apply ds a = foldr apply a ds

-- | For convenience, a nonempty list of deltas
-- can be applied like a list of deltas.
--
-- Remember that deltas are applied right-to-left.
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
