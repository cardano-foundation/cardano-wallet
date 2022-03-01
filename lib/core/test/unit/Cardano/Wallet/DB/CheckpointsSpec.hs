{-# LANGUAGE DataKinds #-}
module Cardano.Wallet.DB.CheckpointsSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.DB.Checkpoints
    ( CheckpointPolicy (..), gapSize, sparseArithmeticPolicy )
import Data.Function
    ( (&) )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set )
import Data.Word
    ( Word32 )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , choose
    , counterexample
    , elements
    , forAll
    , oneof
    , property
    , (===)
    , (==>)
    )

import qualified Data.Set as Set


spec :: Spec
spec = do
    describe "sparseArithmeticPolicy" $ do
        it "predicate ⊇ constructive" $
            property $ \(GenHeightContext epochStability tip) ->
                let policy = sparseArithmeticPolicy epochStability
                    predicate = \height -> keepWhereTip policy height tip
                    constructive = sparseArithmeticSet epochStability tip
                in  all predicate constructive
                    & counterexample ("Heights: " <> show constructive)
                    & counterexample ("Predicate: " <> show
                        (map predicate $ Set.toList constructive))

        it "predicate ⊆ constructive" $
            property $ \ctx@(GenHeightContext epochStability tip) ->
                let policy = sparseArithmeticPolicy epochStability
                    predicate = \height -> keepWhereTip policy height tip
                    constructive = sparseArithmeticSet epochStability tip
                in  forAll (genBlockHeight ctx) $ \height ->
                        predicate height === height `Set.member` constructive

        it "monotonicity - old checkpoints are only discarded" $
            property prop_monotonicity

{-------------------------------------------------------------------------------
    Checkpoint hygiene
-------------------------------------------------------------------------------}
type BlockHeight = Quantity "block" Word32

-- | Constructive counterpart of the 'sparseArithmeticPolicy'
sparseArithmeticSet
    :: BlockHeight
        -- ^ Epoch stability
    -> BlockHeight
        -- ^ Tip of the blockchain
    -> Set BlockHeight
sparseArithmeticSet (Quantity epochStability) (Quantity tip) =
    Set.map Quantity $ Set.fromList [0,tip] <> smallWindow <> largeWindow
  where
    toGrid x g = (x `div` g) * g

    smallWindow = Set.fromList [smallMin .. tip]
    smallSize = 5
    smallMin = if tip < smallSize then 0 else tip - smallSize
    -- I would prefer  max 0 (..)  but this doesn't work as Word32 is unsigned

    largeWindow = Set.fromList [largeMin, largeMin + largeGap .. tip]
    largeGap = gapSize $ Quantity epochStability
    largeMin = if tip < epochStability then 0
        else toGrid (tip - epochStability) largeGap

prop_monotonicity :: GenHeightContext -> Property
prop_monotonicity (GenHeightContext epochStability tip2) =
    forAll (Quantity <$> choose (0,getQuantity tip2)) $ \tip1 ->
    forAll (Quantity <$> choose (0,getQuantity tip1)) $ \height ->
        not (keep height tip1) ==> not (keep height tip2)
  where
    keep = keepWhereTip (sparseArithmeticPolicy epochStability)

-- | Data type for generating sensible blockchein heights
data GenHeightContext = GenHeightContext
    { _epochStability :: BlockHeight
    , _tip :: BlockHeight
    } deriving Show

instance Arbitrary GenHeightContext where
    arbitrary = do
        es <- max 1 <$> oneof [choose (0,1000000), elements [1,3,10,30,100,300,1000] ]
        tip <- oneof [choose (0,1000000), choose (0,10), choose (es-200,es+200)]
            -- ^ do not generate values near upper limit of Word32
        pure $ GenHeightContext (Quantity es) (Quantity tip)

genBlockHeight :: GenHeightContext -> Gen BlockHeight
genBlockHeight (GenHeightContext (Quantity epochStability) (Quantity tip)) =
    Quantity <$> oneof
        [ arbitrary
        , choose (0,10)
        , choose (tip-2*epochStability,tip+2*epochStability)
        ]
