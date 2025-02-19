{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Deposit.Map
    ( -- * Type
      Map (..)

      -- * Keys
    , W
    , F

      -- * Patch management
    , unPatch
    , forgetPatch

      -- * Accessors
    , OpenF
    , open
    , PatchF
    , patch
    , ValueF
    , value

      -- * Lookup
    , lookupMap
    , lookupFinger

      -- * Construction
    , singletonMap
    , singletonFinger

      -- * Conversion
    , toFinger

      -- * Modification
    , onMap
    , onFinger
    , Peel
    )
where

import Cardano.Wallet.Deposit.Map.Timed
    ( Timed (..)
    , TimedSeq
    , extractInterval
    , fmapTimedSeq
    , singleton
    )
import Data.Kind
    ( Type
    )
import Data.Map.Monoidal.Strict
    ( MonoidalMap
    )
import Data.Monoid
    ( Last (..)
    )
import Prelude hiding
    ( lookup
    )

import qualified Cardano.Wallet.Deposit.Map.Timed as TimedSeq
import qualified Data.Map.Monoidal.Strict as MonoidalMap

-- | Infix form of MonoidalMap type
type (^^^) = MonoidalMap

infixr 5 ^^^

-- | A phantom type for tuples of mappings from 'k' tupled with a spurious monoid
-- 'w'. This is used to keep track of the patches applied to the map.
data W (w :: Type) (k :: Type)

-- | A phantom type for a finger tree of mappings from 'k' tupled with a spurious
-- monoid 'w'.
data F (w :: Type) (k :: Type)

-- | A nested monoidal map. Every nesting can also be patched with a monoid 'w'.
data Map :: [Type] -> Type -> Type where
    Value
        :: v
        -> Map '[] v
        -- ^ A leaf node with a value.
    Map
        :: w
        -> k ^^^ Map ks v
        -> Map (W w k ': ks) v
        -- ^ A node with a patch 'w' and a nested monoidal map.
    Finger
        :: w
        -> TimedSeq k (Map ks v)
        -> Map (F w k ': ks) v
        -- ^ A node with a patch 'w' and a nested finger tree of maps.

deriving instance Show v => Show (Map '[] v)

deriving instance
    ( Show w
    , Show k
    , Show (Map ks v)
    )
    => Show (Map (W w k ': ks) v)

deriving instance Eq v => Eq (Map '[] v)

deriving instance
    ( Eq w
    , Eq k
    , Eq (Map ks v)
    )
    => Eq (Map (W w k ': ks) v)

deriving instance
    ( Show w
    , Show k
    , Show (Map ks v)
    )
    => Show (Map (F w k ': ks) v)

deriving instance
    ( Eq w
    , Eq k
    , Eq (Map ks v)
    )
    => Eq (Map (F w k ': ks) v)

instance Functor (Map '[]) where
    fmap f (Value v) = Value (f v)

instance Functor (Map xs) => Functor (Map (W w x : xs)) where
    fmap f (Map w m) = Map w $ fmap (fmap f) m

instance
    (Functor (Map xs), forall a. Monoid (Map xs a))
    => Functor (Map (F w x : xs))
    where
    fmap f (Finger w m) = Finger w $ fmapTimedSeq (fmap f) m

instance Monoid v => Monoid (Map '[] v) where
    mempty = Value mempty

instance
    ( Monoid (Map ks v)
    , Ord k
    , Monoid w
    )
    => Monoid (Map (W w k : ks) v)
    where
    mempty = Map mempty mempty

instance (Monoid (Map xs v), Monoid w, Eq x) => Monoid (Map (F w x : xs) v) where
    mempty = Finger mempty mempty

instance Semigroup v => Semigroup (Map '[] v) where
    Value a <> Value b = Value (a <> b)

instance
    ( Ord x
    , Semigroup (Map xs v)
    , Semigroup w
    )
    => Semigroup (Map (W w x : xs) v)
    where
    Map w a <> Map w' b = Map (w <> w') (a <> b)

instance
    (Monoid w, Monoid (Map xs v), Eq x)
    => Semigroup (Map (F w x : xs) v)
    where
    Finger wa a <> Finger wb b = Finger (wa <> wb) (a <> b)

instance Foldable (Map '[]) where
    foldMap f (Value v) = f v

instance (Foldable (Map xs), Ord x) => Foldable (Map (F w x : xs)) where
    foldMap f (Finger _ m) = foldMap (foldMap f) m

instance (Foldable (Map xs), Ord x) => Foldable (Map (W w x : xs)) where
    foldMap f (Map _ m) = foldMap (foldMap f) m

type family UnPatchF xs where
    UnPatchF (Map (W w x ': xs) v) =
        Map (W () x ': xs) (w, v)
    UnPatchF (Map (F w x ': xs) v) =
        Map (F () x ': xs) (w, v)

-- | Push the patch down to the leaves of the map.
unPatch
    :: ( y ~ Map (x : ks) v
       , Functor (Map ks)
       , Monoid (Map ks v)
       , Monoid (Map ks (w, v))
       , w ~ PatchF x
       )
    => y
    -> UnPatchF y
unPatch (Map w m) = Map () $ fmap (fmap (w,)) m
unPatch (Finger w m) = Finger () $ fmapTimedSeq (fmap (w,)) m

type family ForgetPatchF xs where
    ForgetPatchF (Map (W w x ': xs) v) =
        Map (W () x ': xs) v
    ForgetPatchF (Map (F w x ': xs) v) =
        Map (F () x ': xs) v

-- | Forget the patch of any map layer.
forgetPatch
    :: (y ~ Map (x : ks) v)
    => y
    -> ForgetPatchF y
forgetPatch ((Map _ m)) = Map () m
forgetPatch ((Finger _ m)) = Finger () m

type family PatchF x where
    PatchF (W w x) = w
    PatchF (F w x) = w

-- | Extract the patch from any map layer.
patch :: Map (x : xs) v -> PatchF x
patch (Map w _) = w
patch (Finger w _) = w

type family ValueF x where
    ValueF (Map '[] v) = v
    ValueF (Map (W w x ': xs) v) = x ^^^ Map xs v
    ValueF (Map (F w x ': xs) v) = TimedSeq x (Map xs v)

-- | Extract the value from any map layer.
value :: Map xs v -> ValueF (Map xs v)
value (Map _ m) = m
value (Finger _ m) = m
value (Value v) = v

type family OpenF xs where
    OpenF (Map (W w x ': xs) v) = (w, x ^^^ Map xs v)
    OpenF (Map (F w x ': xs) v) = (w, TimedSeq x (Map xs v))

-- | Open any map layer and return the patch as well.
open :: Map (x : xs) v -> OpenF (Map (x : xs) v)
open (Map w m) = (w, m)
open (Finger w m) = (w, m)

-- | Construct a map layer with a single key-value pair.
singletonMap
    :: w -> k -> Map xs v -> Map (W w k ': xs) v
singletonMap w k = Map w . MonoidalMap.singleton k

-- | Construct a finger layer with a single key-value pair.
singletonFinger
    :: Monoid (Map xs v) => w -> k -> Map xs v -> Map (F w k ': xs) v
singletonFinger w k m =
    Finger w $ singleton $ Timed (Last (Just k)) m

toFinger
    :: (Monoid (Map ks a), Eq k) => Map (W w k : ks) a -> Map (F w k : ks) a
toFinger (Map w m) = Finger w $ TimedSeq.fromList $ do
    (k, v) <- MonoidalMap.toList m
    pure $ Timed (Last (Just k)) v

-- | Lookup a value in first layer of the map and return the patch as well.
lookupMap
    :: (Ord k) => k -> Map (W w k : ks) a -> Maybe (w, Map ks a)
lookupMap k (Map w m) = (w,) <$> MonoidalMap.lookup k m

-- | Lookup for an interval of keys in the finger tree and return the patch as well.
lookupFinger
    :: (Ord k, Monoid (Map ks a))
    => k
    -> k
    -> Map (F w k : ks) a
    -> Maybe (w, Map ks a)
lookupFinger k1 k2 (Finger w m) = do
    case extractInterval k1 k2 m of
        Timed (Last Nothing) _ -> Nothing
        Timed _ m' -> Just (w, m')

-- | Apply a function to the nested monoidal map keeping the patch.
onMap
    :: Map (W w k : ks) a
    -> (MonoidalMap k (Map ks a) -> MonoidalMap k (Map ks a))
    -> Map (W w k : ks) a
onMap (Map w m) f = Map w $ f m

-- | Apply a function to the nested finger tree keeping the patch.
onFinger
    :: Map (F w k : ks) a
    -> (TimedSeq k (Map ks a) -> TimedSeq k (Map ks a))
    -> Map (F w k : ks) a
onFinger (Finger w m) f = Finger w $ f m

type family Peel x where
    Peel (Map (W w k : xs) v) =  Map xs v
    Peel (Map (F w k : xs) v) =  Map xs v
    Peel (Map '[] v) = v
