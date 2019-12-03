module Data.Vector.ShuffleSpec
    ( spec
    ) where

import Prelude

import Data.Vector.Shuffle
    ( shuffle )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Confidence (..), NonEmptyList (..), Property, checkCoverageWith, cover )
import Test.QuickCheck.Monadic
    ( monadicIO, run )

import qualified Data.List as L


spec :: Spec
spec = do
    describe "shuffle" $ do
        it "every non-empty list can be shuffled, ultimately"
            (checkCoverageWith lowerConfidence prop_shuffleCanShuffle)
        it "shuffle is non-deterministic"
            (checkCoverageWith lowerConfidence prop_shuffleNotDeterministic)
        it "sort (shuffled xs) == sort xs"
            (checkCoverageWith lowerConfidence prop_shufflePreserveElements)
  where
    lowerConfidence :: Confidence
    lowerConfidence = Confidence (10^(6 :: Integer)) 0.75

{-------------------------------------------------------------------------------
                                 Properties
-------------------------------------------------------------------------------}

prop_shuffleCanShuffle
    :: NonEmptyList Int
    -> Property
prop_shuffleCanShuffle (NonEmpty xs) = monadicIO $ run $ do
    xs' <- shuffle xs
    return $ cover 90 (xs /= xs') "shuffled" ()

prop_shuffleNotDeterministic
    :: NonEmptyList Int
    -> Property
prop_shuffleNotDeterministic (NonEmpty xs) = monadicIO $ run $ do
    xs1 <- shuffle xs
    xs2 <- shuffle xs
    return $ cover 90 (xs1 /= xs2) "not deterministic" ()

prop_shufflePreserveElements
    :: [Int]
    -> Property
prop_shufflePreserveElements xs = monadicIO $ run $ do
    xs' <- shuffle xs
    return $ cover 90 (not $ null xs) "non-empty" (L.sort xs == L.sort xs')
