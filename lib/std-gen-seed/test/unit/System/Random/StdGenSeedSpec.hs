{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.Random.StdGenSeedSpec
    where

import Prelude

import System.Random
    ( mkStdGen
    )
import System.Random.StdGenSeed
    ( StdGenSeed (..)
    , stdGenFromSeed
    , stdGenToSeed
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , arbitraryBoundedIntegral
    , checkCoverage
    , cover
    , frequency
    , property
    , shrinkIntegral
    , (===)
    )

spec :: Spec
spec = do
    describe "Roundtrip conversion between StdGen and StdGenSeed" $ do
        it "prop_stdGenToSeed_stdGenFromSeed" $
            property prop_stdGenToSeed_stdGenFromSeed
        it "prop_stdGenFromSeed_stdGenToSeed" $
            property prop_stdGenFromSeed_stdGenToSeed

--------------------------------------------------------------------------------
-- Random number generator seeds
--------------------------------------------------------------------------------

prop_stdGenToSeed_stdGenFromSeed :: StdGenSeed -> Property
prop_stdGenToSeed_stdGenFromSeed s =
    checkCoverage $
    cover 1 (s == minBound) "s == minBound" $
    cover 1 (s == maxBound) "s == maxBound" $
    stdGenToSeed (stdGenFromSeed s) === s

prop_stdGenFromSeed_stdGenToSeed :: MkStdGenInt -> Property
prop_stdGenFromSeed_stdGenToSeed (MkStdGenInt i) =
    checkCoverage $
    cover 1 (i == minBound) "i == minBound" $
    cover 1 (i == maxBound) "i == maxBound" $
    stdGenFromSeed (stdGenToSeed (mkStdGen i)) === mkStdGen i

instance Arbitrary StdGenSeed where
    arbitrary = genStdGenSeed
    shrink = shrinkStdGenSeed

genStdGenSeed :: Gen StdGenSeed
genStdGenSeed = StdGenSeed <$> genBoundedIntegralWithUniformPriority 16

shrinkStdGenSeed :: StdGenSeed -> [StdGenSeed]
shrinkStdGenSeed (StdGenSeed s) = StdGenSeed <$> shrinkIntegral s

newtype MkStdGenInt = MkStdGenInt Int
    deriving (Eq, Show)

instance Arbitrary MkStdGenInt where
    arbitrary = genMkStdGenInt
    shrink = shrinkMkStdGenInt

genMkStdGenInt :: Gen MkStdGenInt
genMkStdGenInt = MkStdGenInt <$> genBoundedIntegralWithUniformPriority 16

shrinkMkStdGenInt :: MkStdGenInt -> [MkStdGenInt]
shrinkMkStdGenInt (MkStdGenInt i) = MkStdGenInt <$> shrinkIntegral i

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Generates a distribution that explicitly includes boundary values.
--
-- The given uniform priority parameter determines how uniform the distribution
-- should be:
--
--  - Higher values of this parameter produce distributions that are more
--    uniform, where boundary values are deprioritized.
--  - Lower values of this parameter produce distributions that are less
--    uniform, where boundary values are prioritized.
--
genBoundedIntegralWithUniformPriority
    :: Bounded a => Integral a => Int -> Gen a
genBoundedIntegralWithUniformPriority uniformPriority = frequency
    [ (1, pure minBound)
    , (1, pure maxBound)
    , (1, pure (minBound + 1))
    , (1, pure (maxBound - 1))
    , (uniformPriority, arbitraryBoundedIntegral)
    ]
