-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides a strict implementation of a non-empty map.
--
-- This implementation is based on the implementation of 'Data.Map.Strict'
-- provided by the 'containers' package, but provides an extra guarantee
-- that the map contains at least one entry at all times.
module Data.Map.Strict.NonEmptyMap
    ( -- * Map type
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
    ) where

import Data.Map.Strict.NonEmptyMap.Internal
