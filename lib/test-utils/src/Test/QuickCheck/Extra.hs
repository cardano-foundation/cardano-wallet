{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Extra helper functions for QuickCheck
--

module Test.QuickCheck.Extra
    ( reasonablySized
    , shrinkInterleaved

    -- * Combinators
    , ManyFolded (..)
    ) where

import Prelude

import Test.QuickCheck
    ( Arbitrary (..), Gen, scale )

import qualified Data.Foldable as F

-- | Resize a generator to grow with the size parameter, but remains reasonably
-- sized. That is handy when testing on data-structures that can be arbitrarily
-- large and, when large entities don't really bring any value to the test
-- itself.
--
-- It uses a square root function which makes the size parameter grows
-- quadratically slower than normal. That is,
--
--     +-------------+------------------+
--     | Normal Size | Reasonable Size  |
--     | ----------- + ---------------- +
--     | 0           | 0                |
--     | 1           | 1                |
--     | 10          | 3                |
--     | 100         | 10               |
--     | 1000        | 31               |
--     +-------------+------------------+
--
reasonablySized :: Gen a -> Gen a
reasonablySized = scale (ceiling . sqrt @Double . fromIntegral)

-- | Shrink the given pair in interleaved fashion.
--
-- Successive shrinks of the left and right hand sides are interleaved in the
-- resulting sequence, to avoid biasing either side.
--
shrinkInterleaved :: (a, a -> [a]) -> (b, b -> [b]) -> [(a, b)]
shrinkInterleaved (a, shrinkA) (b, shrinkB) = interleave
    [ (a', b ) | a' <- shrinkA a ]
    [ (a , b') | b' <- shrinkB b ]
  where
    interleave (x : xs) (y : ys) = x : y : interleave xs ys
    interleave xs [] = xs
    interleave [] ys = ys

--------------------------------------------------------------------------------
-- Combinators
--------------------------------------------------------------------------------

-- | A combinator for generating larger arbitrary values, produced by
--   generating many smaller arbitrary values and folding them together.
--
-- Applying this combinator to a type 'A' with an instance of 'Monoid' will
-- generate larger values of 'A', based on simple monoidal concatenation of
-- multiple smaller values.
--
newtype ManyFolded a = ManyFolded
    { getManyFolded :: a }
    deriving (Eq, Show)

instance forall a. (Arbitrary a, Monoid a) => Arbitrary (ManyFolded a) where
    arbitrary = ManyFolded . F.fold <$> arbitrary @[a]
    shrink (ManyFolded a) = ManyFolded <$> shrink a
