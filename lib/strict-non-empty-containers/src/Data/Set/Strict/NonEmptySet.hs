{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides a strict implementation of a non-empty set.
module Data.Set.Strict.NonEmptySet
    ( -- * Type
      NonEmptySet

      -- * Construction
    , fromList
    , fromSet
    , singleton

      -- * Deconstruction
    , toList
    , toSet

      -- * Insertion
    , insert

      -- * Deletion
    , delete

      -- * Membership
    , member

      -- * Combination
    , union
    ) where

import Prelude

import Control.DeepSeq
    ( NFData
    )
import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import Data.Map.Strict.NonEmptyMap
    ( NonEmptyMap
    )
import Data.Maybe
    ( isJust
    )
import Data.Set
    ( Set
    )
import GHC.Generics
    ( Generic (..)
    )

import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict.NonEmptyMap as NonEmptyMap

-- | A non-empty set of elements of type 'a'.
newtype NonEmptySet a = NonEmptySet
    { elements :: NonEmptyMap a ()
    }
    deriving (Eq, Generic, Read, Show)

instance Foldable NonEmptySet where
    foldMap f s = foldMap (f . fst) (NonEmptyMap.toList $ elements s)

instance (NFData a) => NFData (NonEmptySet a)

fromList :: (Ord a) => NonEmpty a -> NonEmptySet a
fromList = NonEmptySet . NonEmptyMap.fromList . fmap (,())

fromSet :: Set a -> Maybe (NonEmptySet a)
fromSet = fmap NonEmptySet . NonEmptyMap.fromMap . Map.fromSet (const ())

toList :: NonEmptySet a -> NonEmpty a
toList = fmap fst . NonEmptyMap.toList . elements

toSet :: (Ord a) => NonEmptySet a -> Set a
toSet = Map.keysSet . NonEmptyMap.toMap . elements

insert :: (Ord a) => a -> NonEmptySet a -> NonEmptySet a
insert a (NonEmptySet m) = NonEmptySet $ NonEmptyMap.insert a () m

delete :: (Ord a) => a -> NonEmptySet a -> Maybe (NonEmptySet a)
delete a (NonEmptySet m) = fmap NonEmptySet (NonEmptyMap.delete a m)

member :: (Ord a) => a -> NonEmptySet a -> Bool
member a (NonEmptySet m) = isJust $ NonEmptyMap.lookup a m

singleton :: (Ord a) => a -> NonEmptySet a
singleton a = NonEmptySet $ NonEmptyMap.singleton a ()

union :: (Ord a) => NonEmptySet a -> NonEmptySet a -> NonEmptySet a
union (NonEmptySet x) (NonEmptySet y) =
    NonEmptySet
        $ NonEmptyMap.unionWith const x y
