-- |
-- Copyright: © 2023 Cardano Foundation
--
-- Extra functions for the strict 'Map' type.
--
module Data.Map.Strict.Extra
    ( conflicts
    ) where

import Prelude

import Data.Map.Merge.Strict
    ( dropMissing
    , merge
    , zipWithMaybeMatched
    )
import Data.Map.Strict
    ( Map
    )

-- | Generates the map of all conflicts between a pair of maps.
--
-- A conflict between maps @m1@ and @m2@ is a mapping from @k@ to @(v1, v2)@
-- such that:
--
-- @
-- lookup k m1 == Just v1
-- lookup k m2 == Just v2
-- v1 /= v2
-- @
--
-- === Example
--
-- @
-- >>> m1 = fromList [('a', 1), ('b', 1), ('c', 1), ('d', 1)          ]
-- >>> m2 = fromList [          ('b', 1), ('c', 2), ('d', 1), ('e', 1)]
-- >>> conflicts m1 m2
-- fromList [('c', (1, 2))]
-- @
--
conflicts :: (Ord k, Eq v) => Map k v -> Map k v -> Map k (v, v)
conflicts =
    merge dropMissing dropMissing (zipWithMaybeMatched dropIfEqual)
  where
    dropIfEqual :: Eq v => k -> v -> v -> Maybe (v, v)
    dropIfEqual _k v1 v2
        | v1 == v2  = Nothing
        | otherwise = Just (v1, v2)
