{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides a strict implementation of a non-empty map.
--
-- This implementation is based on the implementation of 'Data.Map.Strict'
-- provided by the 'containers' package, but provides an extra guarantee
-- that the map contains at least one entry at all times.
module Data.Map.Strict.NonEmptyMap.Internal
    ( -- * Map type

    -- Important:
    --
    -- The default data constructor for 'NonEmptyMap' is not exported, by
    -- design, as the internal data structure has an invariant that must be
    -- preserved across all operations.
    --
    -- Exporting the default constructor would make it possible for functions
    -- outside this module to break the invariant, opening the door to subtle
    -- regressions.
    --
    -- See the definition of 'NonEmptyMap' for more details of the invariant.
    --
    -- To construct a 'NonEmptyMap', use one of the provided constructors,
    -- all of which are tested to check that they respect the invariant.
    --
      NonEmptyMap

      -- * Construction
    , fromList
    , fromMap
    , singleton

      -- * Deconstruction
    , toList
    , toMap

      -- * Insertion
    , insert

      -- * Deletion
    , delete

      -- * Lookup
    , lookup

      -- * Combination
    , unionWith

      -- * Internal functions
    , invariantHolds
    ) where

import Prelude hiding
    ( lookup
    )

import Control.DeepSeq
    ( NFData
    )
import Data.Hashable
    ( Hashable (..)
    , hashUsing
    )
import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import Data.Map.Strict
    ( Map
    )
import GHC.Generics
    ( Generic (..)
    )

import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map

-- | A non-empty map from keys of type 'k' to values of type 'v'.
data NonEmptyMap k v = NonEmptyMap
    { least
        :: !(k, v)
    -- ^ Invariant: this key must be less than than all the keys in 'rest'.
    , rest
        :: !(Map.Map k v)
    }
    deriving (Eq, Foldable, Functor, Generic, Read, Show, Traversable)

instance (NFData k, NFData v) => NFData (NonEmptyMap k v)
instance (Hashable k, Hashable v) => Hashable (NonEmptyMap k v) where
    hashWithSalt = hashUsing toList

-- | Builds a non-empty map from a list of key-value pairs.
--
-- If the list contains more than one value for the same key, the last value
-- for the key is retained.
fromList :: Ord k => NonEmpty (k, v) -> NonEmptyMap k v
fromList (x :| xs) =
    F.foldl' (\m (k, v) -> insert k v m) (uncurry singleton x) xs

-- | Builds a non-empty map from the given map.
--
-- If the given map is empty, this function returns 'Nothing'.
fromMap :: Map k v -> Maybe (NonEmptyMap k v)
fromMap = fmap (uncurry NonEmptyMap) . Map.minViewWithKey

-- | Converts a non-empty map to a list of key-value pairs.
toList :: NonEmptyMap k v -> NonEmpty (k, v)
toList m = least m :| Map.toList (rest m)

-- | Converts a non-empty map to an ordinary map.
toMap :: Ord k => NonEmptyMap k v -> Map k v
toMap m = uncurry Map.insert (least m) (rest m)

-- | Inserts a new key and value in the map.
--
-- If the key is already present in the map, the associated value is replaced
-- with the supplied value.
insert :: Ord k => k -> v -> NonEmptyMap k v -> NonEmptyMap k v
insert k v m
    | k < fst (least m) =
        NonEmptyMap (k, v) (uncurry Map.insert (least m) (rest m))
    | k == fst (least m) =
        NonEmptyMap (k, v) (rest m)
    | otherwise =
        m{rest = Map.insert k v (rest m)}

-- | Deletes a key and its value from the map.
--
-- When the key is not a member of the map, the original map is returned.
--
-- This function returns 'Nothing' if the delete operation reduces the number
-- of elements to zero.
delete :: Ord k => k -> NonEmptyMap k a -> Maybe (NonEmptyMap k a)
delete k m
    | k == fst (least m) = fromMap $ rest m
    | otherwise = Just m{rest = Map.delete k $ rest m}

-- | Looks up the value of a key in the map.
--
-- This function will return the corresponding value as '(Just value)',
-- or 'Nothing' if the key isn't in the map.
lookup :: Ord k => k -> NonEmptyMap k v -> Maybe v
lookup k (NonEmptyMap (k1, v1) r)
    | k == k1 = Just v1
    | otherwise = Map.lookup k r

-- | Creates a map with a single element.
singleton :: Ord k => k -> v -> NonEmptyMap k v
singleton k v = NonEmptyMap (k, v) mempty

-- | Finds the union of two maps, with the given combining function.
unionWith
    :: Ord k
    => (v -> v -> v)
    -> NonEmptyMap k v
    -> NonEmptyMap k v
    -> NonEmptyMap k v
unionWith combine (NonEmptyMap (k1, v1) r1) (NonEmptyMap (k2, v2) r2)
    | k1 < k2 =
        NonEmptyMap (k1, v1) (Map.insertWith combine k2 v2 r)
    | k1 > k2 =
        NonEmptyMap (k2, v2) (Map.insertWith combine k1 v1 r)
    | otherwise =
        NonEmptyMap (k1, combine v1 v2) r
  where
    r = Map.unionWith combine r1 r2

--------------------------------------------------------------------------------
-- Internal functions
--------------------------------------------------------------------------------

-- | Returns true if and only if the invariant holds for the given map.
invariantHolds :: Ord k => NonEmptyMap k v -> Bool
invariantHolds NonEmptyMap{least, rest} =
    case Map.lookupMin rest of
        Nothing ->
            True
        Just nextSmallest ->
            fst least < fst nextSmallest
