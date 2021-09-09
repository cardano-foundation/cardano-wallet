{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Extra helper functions for QuickCheck
--

module Test.QuickCheck.Extra
    (
      -- * Generation
      genMapWith
    , genSized2
    , genSized2With
    , reasonablySized

      -- * Shrinking
    , liftShrink7
    , shrinkInterleaved
    , shrinkMapWith

      -- * Counterexamples
    , report
    , verify

      -- * Utilities
    , interleaveRoundRobin

    ) where

import Prelude

import Data.Map.Strict
    ( Map )
import Fmt
    ( indentF, (+|), (|+) )
import Test.QuickCheck
    ( Gen
    , Property
    , Testable
    , counterexample
    , liftArbitrary2
    , liftShrink2
    , listOf
    , property
    , scale
    , shrinkList
    , shrinkMapBy
    , (.&&.)
    )
import Test.Utils.Pretty
    ( pShowBuilder )

import qualified Data.List as L
import qualified Data.Map.Strict as Map

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

-- | Similar to 'liftShrink2', but applicable to 7-tuples.
--
liftShrink7
    :: (a1 -> [a1])
    -> (a2 -> [a2])
    -> (a3 -> [a3])
    -> (a4 -> [a4])
    -> (a5 -> [a5])
    -> (a6 -> [a6])
    -> (a7 -> [a7])
    -> (a1, a2, a3, a4, a5, a6, a7)
    -> [(a1, a2, a3, a4, a5, a6, a7)]
liftShrink7 s1 s2 s3 s4 s5 s6 s7 (a1, a2, a3, a4, a5, a6, a7) =
    interleaveRoundRobin
    [ [ (a1', a2 , a3 , a4 , a5 , a6 , a7 ) | a1' <- s1 a1 ]
    , [ (a1 , a2', a3 , a4 , a5 , a6 , a7 ) | a2' <- s2 a2 ]
    , [ (a1 , a2 , a3', a4 , a5 , a6 , a7 ) | a3' <- s3 a3 ]
    , [ (a1 , a2 , a3 , a4', a5 , a6 , a7 ) | a4' <- s4 a4 ]
    , [ (a1 , a2 , a3 , a4 , a5', a6 , a7 ) | a5' <- s5 a5 ]
    , [ (a1 , a2 , a3 , a4 , a5 , a6', a7 ) | a6' <- s6 a6 ]
    , [ (a1 , a2 , a3 , a4 , a5 , a6 , a7') | a7' <- s7 a7 ]
    ]

-- Interleaves the given lists together in round-robin order.
--
-- Examples:
--
-- >>> interleaveRoundRobin [["a1", "a2"], ["b1", "b2"]]
-- ["a1", "b1", "a2", "b2"]
--
-- >>> interleaveRoundRobin [["a1", "a2", "a3"], ["b1", "b2"], ["c1"]]
-- ["a1", "b1", "c1", "a2", "b2", "a3"]
--
interleaveRoundRobin :: [[a]] -> [a]
interleaveRoundRobin = concat . L.transpose

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
-- Generating and shrinking key-value maps
--------------------------------------------------------------------------------

-- | Generates a 'Map' with the given key and value generation functions.
--
genMapWith :: Ord k => Gen k -> Gen v -> Gen (Map k v)
genMapWith genKey genValue =
    Map.fromList <$> listOf (liftArbitrary2 genKey genValue)

-- | Shrinks a 'Map' with the given key and value shrinking functions.
--
shrinkMapWith
    :: Ord k
    => (k -> [k])
    -> (v -> [v])
    -> Map k v
    -> [Map k v]
shrinkMapWith shrinkKey shrinkValue
    = shrinkMapBy Map.fromList Map.toList
    $ shrinkList
    $ liftShrink2 shrinkKey shrinkValue

--------------------------------------------------------------------------------
-- Counterexamples
--------------------------------------------------------------------------------

-- | Adds a named variable to the counterexample output of a property.
--
-- On failure, uses pretty-printing to show the contents of the variable.
--
report :: (Show a, Testable prop) => a -> String -> prop -> Property
report a name = counterexample $
    "" +|name|+ ":\n" +|indentF 4 (pShowBuilder a) |+ ""

-- | Adds a named condition to a property.
--
-- On failure, reports the name of the condition that failed.
--
verify :: Bool -> String -> Property -> Property
verify condition conditionTitle =
    (.&&.) (counterexample counterexampleText $ property condition)
  where
    counterexampleText = "Condition violated: " <> conditionTitle
