{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cardano.Wallet.CheckpointsSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Checkpoints
    ( Checkpoints
    , DeltaCheckpoints (..)
    , checkpoints
    , extendAndPrune
    , fromGenesis
    , getLatest
    , loadCheckpoints
    )
import Cardano.Wallet.Checkpoints.Policy
    ( sparseArithmetic )
import Cardano.Wallet.Gen
    ( genSlotNo )
import Cardano.Wallet.Primitive.Types
    ( Slot, SlotNo (..), WithOrigin (..) )
import Data.Delta
    ( Delta (..) )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , choose
    , forAll
    , frequency
    , getPositive
    , listOf
    , property
    , (===)
    )

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "extendAndPrune" $ do
        it "actually prunes checkpoints" $
            property prop_doesPrune
        it "keeps the tip of the chain" $
            property prop_keepTip

{-------------------------------------------------------------------------------
    Properties of extendAndPrune
-------------------------------------------------------------------------------}
prop_doesPrune :: Property
prop_doesPrune =
    forAll (choose (10,100)) $ \n ->
    forAll (choose (10,1000)) $ \tip ->
        let cps0 = denseCheckpoints n
            m = size cps0
        in  m > size (testExtendAndPrune tip 1 cps0)

prop_keepTip :: Checkpoints MockCheckpoint -> Property
prop_keepTip cps0 =
    (tipHeight + m + 1)
    === snd (snd $ getLatest $ testExtendAndPrune tipHeight m cps0)
  where
    m = 2
    tipHeight = snd . snd $ getLatest cps0

{-------------------------------------------------------------------------------
    Helper functions and generators
-------------------------------------------------------------------------------}
type MockCheckpoint = (Slot, Integer)

instance Arbitrary Slot where
    arbitrary = frequency
        [ (1, pure Origin)
        , (20, At <$> genSlotNo)
        ]

instance Arbitrary (Checkpoints MockCheckpoint) where
    arbitrary = do
        xs <- listOf (getPositive <$> arbitrary)
        pure $ loadCheckpoints $ map expand
            $ (Origin,0): map mkMockCheckpoint xs
      where
        expand (slot,j) = (slot, (slot,j))

size :: Checkpoints a -> Integer
size = fromIntegral . Map.size . checkpoints

-- | Specialized version of 'extendAndPrune' for testing.
testExtendAndPrune
    :: Integer
    -> Integer
    -> Checkpoints MockCheckpoint
    -> Checkpoints MockCheckpoint
testExtendAndPrune tip n cps =
    apply (extendAndPrune fst snd policy tip nexts cps) cps
  where
    next = snd (snd (getLatest cps)) + 1
    nexts = NE.fromList $ mkSlotRange next (next + n)
    policy = sparseArithmetic 20

-- | Generate a range of slots.
mkSlotRange :: Integer -> Integer -> [MockCheckpoint]
mkSlotRange a b = map mkMockCheckpoint [a..b]

mkMockCheckpoint :: Integer -> MockCheckpoint
mkMockCheckpoint j = (At (slotNo j), fromIntegral j)

-- | Dense collection of checkpoints.
denseCheckpoints :: Integer -> Checkpoints MockCheckpoint
denseCheckpoints n = apply deltas $ fromGenesis (Origin,0)
  where
    deltas = [ PutCheckpoint slot (slot,j) | (slot,j) <- mkSlotRange 1 (n-1) ]

slotNo :: Integer -> SlotNo
slotNo = SlotNo . fromIntegral
