{-# LANGUAGE DataKinds #-}
module Cardano.Wallet.Checkpoints.PolicySpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Checkpoints.Policy
    ( BlockHeight
    , CheckpointPolicy
    , keepWhereTip
    , nextCheckpoint
    , toListAtTip
    )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , NonNegative (..)
    , Property
    , choose
    , elements
    , forAll
    , frequency
    , oneof
    , property
    , (===)
    )

import qualified Cardano.Wallet.Checkpoints.Policy as CP
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "general laws, tested on trailingArithmetic" $
        it "(<>) is union of checkpoints" $
            property prop_mappendOnTrailing

    describe "general laws, tested on sparseArithmetic" $ do
        it "prop_monotonicHeight — next heights do not fluctuate" $
            property $ \ctx@(GenHeightContext epochStability tip) ->
                forAll (genBlockHeight ctx) $ \height ->
                    prop_monotonicHeight
                        (CP.sparseArithmetic epochStability)
                        tip
                        height

        it "prop_monotonicTip — heights become sparser as tip increases" $
            property $ \ctx@(GenHeightContext epochStability tip) ->
                forAll (genBlockHeight ctx) $ \height ->
                    prop_monotonicTip
                        (CP.sparseArithmetic epochStability)
                        tip
                        height

    describe "specific policies" $ do
        it "atGenesis <> atTip has exactly two checkpoints" $
            property $ \(GenHeightContext _ tip) ->
                CP.toListAtTip (CP.atGenesis <> CP.atTip) tip == [0,tip]

        it "trailingArithmetic n _  has at most n checkpoints" $
            property prop_trailingLength

        it "trailingArithmetic checkpoints are located at grid points" $
            property prop_trailingGrid

        it "sparseArithmetic has genesis" $
            property $ \(GenHeightContext epochStability tip) ->
                keepWhereTip (CP.sparseArithmetic epochStability) tip 0

        it "sparseArithmetic checkpoints after genesis are close to tip" $
            property $ \(GenHeightContext epochStability tip) ->
                maybe False (>= tip - 2*epochStability - 20) $
                    nextCheckpoint (CP.sparseArithmetic epochStability) tip 1

{-------------------------------------------------------------------------------
    Properties, general
-------------------------------------------------------------------------------}
-- | Internal invariant.
prop_monotonicHeight
    :: CheckpointPolicy
    -> BlockHeight -> BlockHeight
    -> Property
prop_monotonicHeight policy tip h1 = case nextCheckpoint policy tip h1 of
    Nothing -> forAll ((h1 +) <$> genNonNegative) $ \h ->
        nextCheckpoint policy tip h === Nothing
    Just h2 -> forAll (genInterval (h1,h2)) $ \h ->
        nextCheckpoint policy tip h === Just h2

-- | Internal invariant.
prop_monotonicTip
    :: CheckpointPolicy
    -> BlockHeight -> BlockHeight
    -> Property
prop_monotonicTip policy tip height = case nextCheckpoint policy tip height of
    Nothing -> property True
    Just height1 -> forAll ((tip +) <$> genNonNegative) $ \tip2 ->
        case nextCheckpoint policy tip2 height of
            Nothing -> property False
            Just height2 -> property $ height1 <= height2

prop_mappendOnTrailing :: Property
prop_mappendOnTrailing = let g17 = choose (1,7) in
    forAll g17 $ \n1 -> forAll g17 $ \gap1 ->
    forAll g17 $ \n2 -> forAll g17 $ \gap2 ->
        let policy1 = CP.trailingArithmetic n1 gap1
            policy2 = CP.trailingArithmetic n2 gap2
            tip     = 100
        in      Set.fromList (toListAtTip policy1 tip)
                <> Set.fromList (toListAtTip policy2 tip)
            ===
                Set.fromList (toListAtTip (policy1 <> policy2) tip)

{-------------------------------------------------------------------------------
    Properties, specific
-------------------------------------------------------------------------------}
prop_trailingLength :: GenHeightContext -> Property
prop_trailingLength (GenHeightContext gap tip) =
    forAll (choose (0,5)) $ \n ->
        fromIntegral n >= length (toListAtTip (CP.trailingArithmetic n gap) tip)

prop_trailingGrid :: GenHeightContext -> Property
prop_trailingGrid (GenHeightContext gap tip) =
    forAll (choose (1,7)) $ \n ->
        all (`divisibleBy` gap) $ toListAtTip (CP.trailingArithmetic n gap) tip
  where
    a `divisibleBy` b = a `mod` b == 0

{-------------------------------------------------------------------------------
    Generators
-------------------------------------------------------------------------------}
-- | Data type for generating sensible blockchain heights
data GenHeightContext = GenHeightContext
    { _epochStability :: BlockHeight
    , _tip :: BlockHeight
    } deriving Show

instance Arbitrary GenHeightContext where
    arbitrary = do
        es <- max 1 <$> oneof
            [choose (0,1000000), elements [1,3,10,30,100,300,1000] ]
        tip <- max 1 <$> oneof
            [choose (0,1000000), choose (0,10), choose (es-200,es+200)]
        pure $ GenHeightContext es tip

-- | Generate 'Integers' from an interval, skewed towards the ends.
genInterval :: (Integer, Integer) -> Gen Integer
genInterval (a,b) = clip <$> frequency
    [ (1, choose (a,a+3))
    , (1, choose (b,b+3))
    , (5, choose (a,b))
    ]
  where
    clip = max a . min b

genNonNegative :: Gen Integer
genNonNegative = getNonNegative <$> arbitrary

genBlockHeight :: GenHeightContext -> Gen BlockHeight
genBlockHeight (GenHeightContext epochStability tip) = oneof
    [ genNonNegative
    , choose (0,10)
    , choose (tip-2*epochStability,tip+2*epochStability)
    ]
