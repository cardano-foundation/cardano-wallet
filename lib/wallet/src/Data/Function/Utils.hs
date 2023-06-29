{-# LANGUAGE BangPatterns #-}

module Data.Function.Utils
    ( applyN
    , isInjectiveOver
    ) where

import Prelude

import Data.Set
    ( Set
    )

import qualified Data.Set as Set

-- | Apply a function 'n' times to the specified input.
applyN :: Integral n => n -> (a -> a) -> a -> a
applyN !n !f !a
    | n <= 0 = a
    | otherwise = applyN (n - 1) f (f a)

-- | Returns 'True' if (and only if) the given function is injective over the
--   given domain.
--
-- Examples:
--
-- >>> succ `isInjectiveOver` Set.fromList [1 .. 10]
-- True
--
-- >>> (`div` 2) `isInjectiveOver` Set.fromList [1 .. 10]
-- False
isInjectiveOver :: Ord b => (a -> b) -> Set a -> Bool
isInjectiveOver f domain = Set.size domain == Set.size (Set.map f domain)
