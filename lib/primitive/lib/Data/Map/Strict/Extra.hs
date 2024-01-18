-- |
-- Copyright: © 2023 Cardano Foundation
--
-- Extra functions for the strict 'Map' type.
--
module Data.Map.Strict.Extra
    ( conflicts
    , conflictsWith
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
-- Equivalent to 'conflictsWith' '/='.
--
conflicts :: (Ord k, Eq v) => Map k v -> Map k v -> Map k (v, v)
conflicts = conflictsWith (/=)

-- | Generates the map of all conflicts between a pair of maps, for the given
--   conflict indicator function.
--
-- For a given conflict indicator function 'f', a conflict between maps @m1@
-- and @m2@ is a mapping from @k@ to @(v1, v2)@ such that:
--
-- @
-- lookup k m1 == Just v1
-- lookup k m2 == Just v2
-- f v1 v2
-- @
--
-- === Example
--
-- @
-- >>> m1 = fromList [("A", 1), ("B", 1), ("C", 1), ("D", 1)          ]
-- >>> m2 = fromList [          ("B", 1), ("C", 2), ("D", 1), ("E", 1)]
-- >>> conflictsWith (/=) m1 m2
-- fromList [("C", (1, 2))]
-- @
--
conflictsWith
    :: Ord k
    => (v1 -> v2 -> Bool)
    -> Map k v1
    -> Map k v2
    -> Map k (v1, v2)
conflictsWith inConflictWith =
    merge dropMissing dropMissing (zipWithMaybeMatched (const maybeConflict))
  where
    maybeConflict v1 v2
        | v1 `inConflictWith` v2 = Just (v1, v2)
        | otherwise = Nothing
