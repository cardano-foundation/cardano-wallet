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
    , genSized2
    , genSized2With
    ) where

import Prelude

import Test.QuickCheck
    ( Gen, scale )

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

-- | Resizes a generator by taking the nth root of the size parameter.
--
-- This combinator can restore size linearity to generators composed of 'n'
-- independent generators in the case that each generator generates values
-- from a range that depends on the size parameter.
--
-- Example:
--
-- Suppose that we have a single generator composed of **three** independent
-- generators, where each generator depends on the size parameter.
--
-- If the current value of the size parameter is 1000, then to generate a range
-- of up to 1000 different composite values, we can resize each individual
-- generator so that it generates up to 10 different values:
--
-- >>> genComposite = Composite
-- >>>     <$> scaleToRoot 3 genA
-- >>>     <*> scaleToRoot 3 genB
-- >>>     <*> scaleToRoot 3 genC
--
scaleToRoot :: Int -> Gen a -> Gen a
scaleToRoot n = scale
    $ floor @Double @Int
    . (** (1.0 / fromIntegral @Int @Double n))
    . fromIntegral @Int @Double

-- | Generates a 2-tuple whose range depends linearly on the size parameter.
--
genSized2 :: Gen a -> Gen b -> Gen (a, b)
genSized2 genA genB = (,)
    <$> scaleToRoot 2 genA
    <*> scaleToRoot 2 genB

-- | Similar to 'genSized2', but with a custom constructor.
--
genSized2With :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
genSized2With f genA genB = uncurry f <$> genSized2 genA genB

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
