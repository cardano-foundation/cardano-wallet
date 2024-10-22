{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Deposit.Map
    ( Map (..)
    , K
    , W
    , type (^^^)
    , Lookup
    , Key (..)
    , singletonMap
    , openMap
    , singletonPatched
    , unPatch
    , withMap
    , forMap
    , withPatched
    , lookup
    , lookupPatched
    , forPatched
    , openPatched
    , At (..)
    , forgetPatch
    ) where

import Prelude hiding
    ( lookup
    )

import Data.Kind
    ( Type
    )
import Data.Map.Monoidal.Strict
    ( MonoidalMap (..)
    )

import qualified Data.Map.Monoidal.Strict as MonoidalMap

-- | Infix form of MonoidalMap type
type (^^^) = MonoidalMap

infixr 5 ^^^

-- | A phantom type for tuples of mappings from 'k' tupled with a spurious monoid
-- 'w'. This is used to keep track of the patches applied to the map.
data W (w :: Type) (k :: Type)

-- | A phantom type for mappings from 'k'
data K (k :: Type)

-- | A nested monoidal map. Every nesting can also be patched with a monoid 'w'.
data Map :: [Type] -> Type -> Type where
    Value :: v -> Map '[] v
    Map :: k ^^^ Map ks v -> Map (K k ': ks) v
    Patched :: w -> k ^^^ Map ks v -> Map (W w k ': ks) v

deriving instance Show v => Show (Map '[] v)

deriving instance
    ( Show k
    , Show v
    , Show (Map ks v)
    )
    => Show (Map (K k ': ks) v)

deriving instance
    ( Show w
    , Show k
    , Show (Map ks v)
    )
    => Show (Map (W w k ': ks) v)

deriving instance Eq v => Eq (Map '[] v)

deriving instance
    ( Eq k
    , Eq v
    , Eq (k ^^^ Map ks v)
    )
    => Eq (Map (K k ': ks) v)

deriving instance
    ( Eq w
    , Eq k
    , Eq (Map ks v)
    )
    => Eq (Map (W w k ': ks) v)

deriving instance Functor (Map '[])

deriving instance Functor (Map ks) => Functor (Map (k ': ks))

instance Monoid v => Monoid (Map '[] v) where
    mempty = Value mempty

instance
    ( Monoid (Map ks v)
    , Ord k
    )
    => Monoid (Map (K k : ks) v)
    where
    mempty = Map mempty

instance
    ( Monoid (Map ks v)
    , Ord k
    , Monoid w
    )
    => Monoid (Map (W w k : ks) v)
    where
    mempty = Patched mempty mempty

instance Semigroup v => Semigroup (Map '[] v) where
    Value a <> Value b = Value (a <> b)

instance
    ( Monoid (Map ks v)
    , Ord k
    )
    => Semigroup (Map (K k : ks) v)
    where
    Map a <> Map b = Map (a <> b)

instance
    ( Ord x
    , Semigroup (Map xs v)
    , Semigroup w
    )
    => Semigroup (Map (W w x : xs) v)
    where
    Patched w a <> Patched w' b = Patched (w <> w') (a <> b)

instance Foldable (Map '[]) where
    foldMap f (Value v) = f v

instance (Foldable (Map xs), Ord x) => Foldable (Map (K x : xs)) where
    foldMap f (Map m) = foldMap (foldMap f) m

instance (Foldable (Map xs), Ord x) => Foldable (Map (W w x : xs)) where
    foldMap f (Patched _ m) = foldMap (foldMap f) m

-- | Push the patch down to the leaves of the map.
unPatch :: Functor (Map xs) => Map (W w x : xs) v -> Map (K x : xs) (w, v)
unPatch (Patched w m) = Map $ fmap (fmap (w,)) m

-- | Forget the patch applied to the map.
forgetPatch :: Map (W w x : xs) v -> Map (K x : xs) v
forgetPatch (Patched _ m) = Map m

-- | Open the map to access the underlying monoidal map.
openMap :: Map (K x : xs) v -> x ^^^ Map xs v
openMap (Map m) = m

-- | Open the patched map to access the underlying monoidal map and the patch.
openPatched :: Map (W w x : xs) v -> (w, x ^^^ Map xs v)
openPatched (Patched w m) = (w, m)

-- | Create a map with a single key-value pair.
singletonMap :: x -> Map xs v -> Map (K x : xs) v
singletonMap x m = Map $ MonoidalMap.singleton x m

-- | Create a patched map with a single key-value pair.
singletonPatched :: w -> x -> Map xs v -> Map (W w x : xs) v
singletonPatched w x m = Patched w $ MonoidalMap.singleton x m

-- | Destroy the underlying monoidal map.
withMap
    :: Map (K k ': ks) a
    -> (forall x. MonoidalMap k x -> b)
    -> b
withMap (Map m) f = f m

-- | Apply a function to the underlying monoidal map.
forMap
    :: Map (K k ': ks) a
    -> (forall x. MonoidalMap k x -> MonoidalMap k x)
    -> Map (K k ': ks) a
forMap (Map m) f = Map (f m)

-- | Destroy the underlying monoidal map and the patch.
withPatched
    :: Map (W w k ': ks) a
    -> (forall x. w -> MonoidalMap k x -> b)
    -> b
withPatched (Patched w m) f = f w m

-- | Apply a function to the underlying monoidal map and the patch.
forPatched
    :: Map (W w k ': ks) a
    -> (forall x. w -> MonoidalMap k x -> MonoidalMap k x)
    -> Map (W w k ': ks) a
forPatched (Patched w m) f = Patched w (f w m)

-- | Lookup a value in first layer of the map.
lookup :: (Ord k) => k -> Map (K k : ks) a -> Maybe (Map ks a)
lookup k (Map m) = MonoidalMap.lookup k m

-- | Lookup a value in first layer of the map and return the patch as well.
lookupPatched :: (Ord k) => k -> Map (W w k : ks) a -> Maybe (w, Map ks a)
lookupPatched k (Patched w m) = (w,) <$> MonoidalMap.lookup k m

-- | A key to access a value at some depth in the map. It's also a witness of
-- ks being a prefix of rs.
data Key ks rs where
    KeyK :: k -> Key ks rs -> Key (K k ': ks) (K k ': rs)
    KeyW :: k -> Key ks rs -> Key (W w k ': ks) (W w k ': rs)
    LastK :: Key '[] rs

-- | Compute the type of a lookup at some depth in the map. The type is lossless
-- so it carries the patched semigroups as well.
type family Lookup xs ys a :: Type where
    Lookup '[] ys a = Map ys a
    Lookup (K x ': xs) (K x : ys) a = Lookup xs ys a
    Lookup (W w x ': xs) (W w x : ys) a = (w, Lookup xs ys a)

-- | A class to access a value at some depth in the map.
class At ks rs where
    at
        :: Key ks rs
        -- ^ The key list to access the value.
        -> Map rs a
        -- ^ The map to access the value from.
        -> Maybe (Lookup ks rs a)
        -- ^ The value at the given key list.

instance At '[] rs where
    at LastK = Just

instance (Ord k, At ks rs) => At (K k ': ks) (K k ': rs) where
    at (KeyK k ks) m = lookup k m >>= at ks

instance (Ord k, At ks rs) => At (W w k ': ks) (W w k ': rs) where
    at (KeyW k ks) m = lookupPatched k m >>= \(w, m') -> (w,) <$> at ks m'
