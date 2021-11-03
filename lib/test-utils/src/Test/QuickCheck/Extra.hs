{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Extra helper functions for QuickCheck
--

module Test.QuickCheck.Extra
    (
      -- * Generation
      genFunction
    , genMapWith
    , genSized2
    , genSized2With
    , reasonablySized

      -- * Shrinking
    , liftShrink3
    , liftShrink4
    , liftShrink5
    , liftShrink6
    , liftShrink7
    , liftShrink8
    , liftShrink9
    , liftShrinker
    , shrinkInterleaved
    , shrinkMapWith
    , groundRobinShrink
    , groundRobinShrink'
    , genericRoundRobinShrink

      -- * Generating and shrinking natural numbers
    , chooseNatural
    , shrinkNatural

      -- * Counterexamples
    , report
    , verify

      -- * Pretty-printing
    , Pretty (..)

      -- * Combinators
    , NotNull (..)

      -- * Utilities
    , interleaveRoundRobin

    ) where

import Prelude

import Data.IntCast
    ( intCast, intCastMaybe )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( mapMaybe )
import Fmt
    ( indentF, (+|), (|+) )
import Generics.SOP
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , Testable
    , chooseInteger
    , counterexample
    , liftArbitrary2
    , liftShrink2
    , listOf
    , property
    , scale
    , shrinkIntegral
    , shrinkList
    , shrinkMapBy
    , suchThat
    , suchThatMap
    , (.&&.)
    )
import Test.QuickCheck.Gen.Unsafe
    ( promote )
import Test.Utils.Pretty
    ( pShowBuilder )
import Text.Pretty.Simple
    ( pShow )

import qualified Data.Generic.Fields as Fields
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as TL
import qualified Generics.SOP.GGP as GGP
import qualified GHC.Generics as GHC

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

-- | Similar to 'liftShrink2', but applicable to values with 3 fields.
--
liftShrink3
    :: Fields.HasFields3 r a1 a2 a3
    => (a1 -> a2 -> a3 -> r)
    -> (a1 -> [a1])
    -> (a2 -> [a2])
    -> (a3 -> [a3])
    -> r
    -> [r]
liftShrink3 f s1 s2 s3 r =
    interleaveRoundRobin
    [ [ f a1' a2  a3  | a1' <- s1 a1 ]
    , [ f a1  a2' a3  | a2' <- s2 a2 ]
    , [ f a1  a2  a3' | a3' <- s3 a3 ]
    ]
  where
    (a1, a2, a3) = Fields.toTuple3 r

-- | Similar to 'liftShrink2', but applicable to values with 4 fields.
--
liftShrink4
    :: Fields.HasFields4 r a1 a2 a3 a4
    => (a1 -> a2 -> a3 -> a4 -> r)
    -> (a1 -> [a1])
    -> (a2 -> [a2])
    -> (a3 -> [a3])
    -> (a4 -> [a4])
    -> r
    -> [r]
liftShrink4 f s1 s2 s3 s4 r =
    interleaveRoundRobin
    [ [ f a1' a2  a3  a4  | a1' <- s1 a1 ]
    , [ f a1  a2' a3  a4  | a2' <- s2 a2 ]
    , [ f a1  a2  a3' a4  | a3' <- s3 a3 ]
    , [ f a1  a2  a3  a4' | a4' <- s4 a4 ]
    ]
  where
    (a1, a2, a3, a4) = Fields.toTuple4 r

-- | Similar to 'liftShrink2', but applicable to values with 5 fields.
--
liftShrink5
    :: Fields.HasFields5 r a1 a2 a3 a4 a5
    => (a1 -> a2 -> a3 -> a4 -> a5 -> r)
    -> (a1 -> [a1])
    -> (a2 -> [a2])
    -> (a3 -> [a3])
    -> (a4 -> [a4])
    -> (a5 -> [a5])
    -> r
    -> [r]
liftShrink5 f s1 s2 s3 s4 s5 r =
    interleaveRoundRobin
    [ [ f a1' a2  a3  a4  a5  | a1' <- s1 a1 ]
    , [ f a1  a2' a3  a4  a5  | a2' <- s2 a2 ]
    , [ f a1  a2  a3' a4  a5  | a3' <- s3 a3 ]
    , [ f a1  a2  a3  a4' a5  | a4' <- s4 a4 ]
    , [ f a1  a2  a3  a4  a5' | a5' <- s5 a5 ]
    ]
  where
    (a1, a2, a3, a4, a5) = Fields.toTuple5 r

-- | Similar to 'liftShrink2', but applicable to values with 6 fields.
--
liftShrink6
    :: Fields.HasFields6 r a1 a2 a3 a4 a5 a6
    => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r)
    -> (a1 -> [a1])
    -> (a2 -> [a2])
    -> (a3 -> [a3])
    -> (a4 -> [a4])
    -> (a5 -> [a5])
    -> (a6 -> [a6])
    -> r
    -> [r]
liftShrink6 f s1 s2 s3 s4 s5 s6 r =
    interleaveRoundRobin
    [ [ f a1' a2  a3  a4  a5  a6  | a1' <- s1 a1 ]
    , [ f a1  a2' a3  a4  a5  a6  | a2' <- s2 a2 ]
    , [ f a1  a2  a3' a4  a5  a6  | a3' <- s3 a3 ]
    , [ f a1  a2  a3  a4' a5  a6  | a4' <- s4 a4 ]
    , [ f a1  a2  a3  a4  a5' a6  | a5' <- s5 a5 ]
    , [ f a1  a2  a3  a4  a5  a6' | a6' <- s6 a6 ]
    ]
  where
    (a1, a2, a3, a4, a5, a6) = Fields.toTuple6 r

-- | Similar to 'liftShrink2', but applicable to values with 7 fields.
--
liftShrink7
    :: Fields.HasFields7 r a1 a2 a3 a4 a5 a6 a7
    => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> r)
    -> (a1 -> [a1])
    -> (a2 -> [a2])
    -> (a3 -> [a3])
    -> (a4 -> [a4])
    -> (a5 -> [a5])
    -> (a6 -> [a6])
    -> (a7 -> [a7])
    -> r
    -> [r]
liftShrink7 f s1 s2 s3 s4 s5 s6 s7 r =
    interleaveRoundRobin
    [ [ f a1' a2  a3  a4  a5  a6  a7  | a1' <- s1 a1 ]
    , [ f a1  a2' a3  a4  a5  a6  a7  | a2' <- s2 a2 ]
    , [ f a1  a2  a3' a4  a5  a6  a7  | a3' <- s3 a3 ]
    , [ f a1  a2  a3  a4' a5  a6  a7  | a4' <- s4 a4 ]
    , [ f a1  a2  a3  a4  a5' a6  a7  | a5' <- s5 a5 ]
    , [ f a1  a2  a3  a4  a5  a6' a7  | a6' <- s6 a6 ]
    , [ f a1  a2  a3  a4  a5  a6  a7' | a7' <- s7 a7 ]
    ]
  where
    (a1, a2, a3, a4, a5, a6, a7) = Fields.toTuple7 r

-- | Similar to 'liftShrink2', but applicable to values with 8 fields.
--
liftShrink8
    :: Fields.HasFields8 r a1 a2 a3 a4 a5 a6 a7 a8
    => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> r)
    -> (a1 -> [a1])
    -> (a2 -> [a2])
    -> (a3 -> [a3])
    -> (a4 -> [a4])
    -> (a5 -> [a5])
    -> (a6 -> [a6])
    -> (a7 -> [a7])
    -> (a8 -> [a8])
    -> r
    -> [r]
liftShrink8 f s1 s2 s3 s4 s5 s6 s7 s8 r =
    interleaveRoundRobin
    [ [ f a1' a2  a3  a4  a5  a6  a7  a8  | a1' <- s1 a1 ]
    , [ f a1  a2' a3  a4  a5  a6  a7  a8  | a2' <- s2 a2 ]
    , [ f a1  a2  a3' a4  a5  a6  a7  a8  | a3' <- s3 a3 ]
    , [ f a1  a2  a3  a4' a5  a6  a7  a8  | a4' <- s4 a4 ]
    , [ f a1  a2  a3  a4  a5' a6  a7  a8  | a5' <- s5 a5 ]
    , [ f a1  a2  a3  a4  a5  a6' a7  a8  | a6' <- s6 a6 ]
    , [ f a1  a2  a3  a4  a5  a6  a7' a8  | a7' <- s7 a7 ]
    , [ f a1  a2  a3  a4  a5  a6  a7  a8' | a8' <- s8 a8 ]
    ]
  where
    (a1, a2, a3, a4, a5, a6, a7, a8) = Fields.toTuple8 r

-- | Similar to 'liftShrink2', but applicable to values with 9 fields.
--
liftShrink9
    :: Fields.HasFields9 r a1 a2 a3 a4 a5 a6 a7 a8 a9
    => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> r)
    -> (a1 -> [a1])
    -> (a2 -> [a2])
    -> (a3 -> [a3])
    -> (a4 -> [a4])
    -> (a5 -> [a5])
    -> (a6 -> [a6])
    -> (a7 -> [a7])
    -> (a8 -> [a8])
    -> (a9 -> [a9])
    -> r
    -> [r]
liftShrink9 f s1 s2 s3 s4 s5 s6 s7 s8 s9 r =
    interleaveRoundRobin
    [ [ f a1' a2  a3  a4  a5  a6  a7  a8  a9  | a1' <- s1 a1 ]
    , [ f a1  a2' a3  a4  a5  a6  a7  a8  a9  | a2' <- s2 a2 ]
    , [ f a1  a2  a3' a4  a5  a6  a7  a8  a9  | a3' <- s3 a3 ]
    , [ f a1  a2  a3  a4' a5  a6  a7  a8  a9  | a4' <- s4 a4 ]
    , [ f a1  a2  a3  a4  a5' a6  a7  a8  a9  | a5' <- s5 a5 ]
    , [ f a1  a2  a3  a4  a5  a6' a7  a8  a9  | a6' <- s6 a6 ]
    , [ f a1  a2  a3  a4  a5  a6  a7' a8  a9  | a7' <- s7 a7 ]
    , [ f a1  a2  a3  a4  a5  a6  a7  a8' a9  | a8' <- s8 a8 ]
    , [ f a1  a2  a3  a4  a5  a6  a7  a8  a9' | a9' <- s9 a9 ]
    ]
  where
    (a1, a2, a3, a4, a5, a6, a7, a8, a9) = Fields.toTuple9 r

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
-- Generating and shrinking natural numbers
--------------------------------------------------------------------------------

chooseNatural :: (Natural, Natural) -> Gen Natural
chooseNatural (lo, hi) =
    chooseInteger (intCast lo, intCast hi)
    `suchThatMap`
    intCastMaybe @Integer @Natural

shrinkNatural :: Natural -> [Natural]
shrinkNatural n
    = mapMaybe (intCastMaybe @Integer @Natural)
    $ shrinkIntegral
    $ intCast n

--------------------------------------------------------------------------------
-- Generating functions
--------------------------------------------------------------------------------

-- | Generates a function.
--
-- This is based on the implementation of 'Arbitrary' for 'a -> b'.
--
genFunction :: (a -> Gen b -> Gen b) -> Gen b -> Gen (a -> b)
genFunction coarbitraryFn gen = promote (`coarbitraryFn` gen)

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

--------------------------------------------------------------------------------
-- Pretty-printing
--------------------------------------------------------------------------------

-- | A combinator that causes the output of `show` to be pretty-printed.
--
newtype Pretty a = Pretty { unPretty :: a }
    deriving Eq

instance Show a => Show (Pretty a) where
    show (Pretty a) = TL.unpack ("\n" <> pShow a <> "\n")

instance Arbitrary a => Arbitrary (Pretty a) where
    arbitrary = Pretty <$> arbitrary
    shrink (Pretty a) = Pretty <$> shrink a

--------------------------------------------------------------------------------
-- Non-null values
--------------------------------------------------------------------------------

newtype NotNull a = NotNull { unNotNull :: a }
    deriving (Eq, Show)

instance (Arbitrary a, Eq a, Monoid a) => Arbitrary (NotNull a) where
    arbitrary = NotNull <$> arbitrary `suchThat` (/= mempty)
    shrink (NotNull u) = NotNull <$> filter (/= mempty) (shrink u)

--------------------------------------------------------------------------------
-- Generic shrinking
--------------------------------------------------------------------------------

-- | Lift a shrinking function into a type more convenient for use with
-- Generics.SOP.
--
-- __Examples:__
--
-- @
-- shrinkBool :: Bool -> [Bool]
-- shrinkChar :: Char -> [Char]
--
-- data BoolChar = BoolChar Bool Char
--
-- instance Generic BoolChar
--
-- shrinkBoolChar :: BoolChar -> [BoolChar]
-- shrinkBoolChar =
--     groundRobinShrink (  liftShrinker shrinkBool
--                       :* liftShrinker shrinkChar
--                       :* Nil
--                       )
-- @
liftShrinker :: (a -> [a]) -> (I -.-> []) a
liftShrinker shrinker = fn (shrinker . unI)

-- | Using a round-robin algorithm, apply a list of shrinkers to their
-- corresponding types in a n-ary product.
--
-- __Examples:__
--
-- @
-- shrinkBoolChar :: BoolChar -> [NP I '[Bool, Char]]
-- shrinkBoolChar (BoolChar b c) =
--     groundRobinShrinkP (  liftShrinker shrinkBool
--                        :* liftShrinker shrinkChar
--                        :* Nil
--                        )
--                        (I b :* I c :* Nil)
-- @
groundRobinShrinkP :: NP (I -.-> []) xs -> NP I xs -> [NP I xs]
groundRobinShrinkP fns = interleaveRoundRobin . groundRobinShrinkP' fns
    where
        groundRobinShrinkP' :: NP (I -.-> []) xs -> NP I xs -> [[NP I xs]]
        groundRobinShrinkP' Nil Nil =
            -- In the case of no argument constructors, there is no need to
            -- shrink
            []
        groundRobinShrinkP' (s :* ss) (x1 :* xs) =
            -- Best explained with example:
            --   BoolChar b c
            --     1. shrink b = [b1, b2, b3]
            --     -- shrink the first argument
            --     2. [ BoolChar b1 c, BoolChar b2 c, BoolChar b3 c ]
            --     -- create a list of values with only first value shrunk
            --     3. shrink c = [c1, c2, c3]
            --     -- shrink the second argument
            --     4. [ BoolChar b c1, BoolChar b c2, BoolChar b c3 ]
            --     -- create a list of values with only second value shrunk
            --     -- append and return the lists in 2. and 4.
            [ [ ( I x1' :* xs ) | x1' <- apFn s x1 ] ]
            <> (fmap (x1 :*) <$> groundRobinShrinkP' ss xs)

-- | Using a round-robin algorithm, apply a list of shrinkers to their
-- corresponding types in a Generics.SOP type. Only defined for types with a
-- single constructor (product types).
--
-- __Examples:__
--
-- @
-- shrinkBoolChar :: BoolChar -> [SOP I '[Bool, Char]]
-- shrinkBoolChar bc =
--     groundRobinShrinkS (  liftShrinker shrinkBool
--                        :* liftShrinker shrinkChar
--                        :* Nil
--                        )
--                        (from bc)
-- @
groundRobinShrinkS
    :: NP (I -.-> []) xs
    -- ^ Given a list of shrinkers for each element in a product type
    -> SOP I (xs ': '[])
    -- ^ And a type with only one constructor
    -> [SOP I (xs ': '[])]
    -- ^ Return a shrunk list of that product type, using the round-robin
    -- algorithm.
groundRobinShrinkS fs (SOP (Z xs)) = (SOP . Z) <$> groundRobinShrinkP fs xs
groundRobinShrinkS _ (SOP (S _))   = error "only defined for product types."

-- | Given a list of shrinkers for each element of a product type (NOTE: this
-- function is not defined for sum types), and a value of that product type,
-- shrink the value using a round-robin algorithm.
--
-- __Examples:__
--
-- @
-- shrinkBoolChar :: BoolChar -> [BoolChar]
-- shrinkBoolChar =
--     groundRobinShrink (  liftShrinker shrinkBool
--                       :* liftShrinker shrinkChar
--                       :* Nil
--                       )
-- @
groundRobinShrink
    :: ( Generic a
       -- Given a generic type
       , Code a ~ '[xs]
       -- whose generic representation matches the structure of the list of
       -- functions
       )
    => NP (I -.-> []) xs
    -- ^ and a list of shrinking functions, one for each argument of the product
    -> a
    -- ^ and a value of that type
    -> [a]
    -- ^ provide a list of shrunk values.
groundRobinShrink f x = to <$> groundRobinShrinkS f (from x)

-- | Same as groundRobinShrink, but use the available shrinkers via Arbitrary
-- instance.
--
-- __Examples:__
--
-- @
-- shrinkBoolChar :: BoolChar -> [BoolChar]
-- shrinkBoolChar = groundRobinShrink'
-- @
groundRobinShrink'
    :: ( Generic a
       -- The type is an instance of SOP.Generic
       , Code a ~ '[xs]
       , All Arbitrary xs
       -- and each element of the constructor has an instance of arbitrary
       )
    => a
    -- ^ Given such a type
    -> [a]
    -- ^ return a shrunk list of that product type, using the round-robin
    -- algorithm.
groundRobinShrink' x =
    fmap to
    $ groundRobinShrinkS (hcpure (Proxy @Arbitrary) (liftShrinker shrink))
    $ from x

-- | This function exists to provide a GHC.Generics version of
-- @groundRobinShrink@, so that users of this code don't have to derive an
-- instance of Generics.SOP.Generic; an instance of GHC.Generics.Generic will
-- do.
genericRoundRobinShrink
    :: ( GHC.Generic a
       , GGP.GFrom a
       , GGP.GTo a
       , GGP.GCode a ~ '[xs]
       )
    => NP (I -.-> []) xs
    -> a
    -> [a]
genericRoundRobinShrink f x =
  GGP.gto <$> groundRobinShrinkS f (GGP.gfrom x)
