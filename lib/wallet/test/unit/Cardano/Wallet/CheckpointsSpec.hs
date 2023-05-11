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
    , SparseCheckpointsConfig (..)
    , checkpoints
    , extendAndPrune
    , fromGenesis
    , gapSize
    , getLatest
    , loadCheckpoints
    , sparseCheckpoints
    )
import Cardano.Wallet.Checkpoints.Policy
    ( sparseArithmetic )
import Cardano.Wallet.Gen
    ( genSlotNo )
import Cardano.Wallet.Primitive.Types
    ( Slot, SlotNo (..), WithOrigin (..) )
import Data.Delta
    ( Delta (..) )
import Data.Function
    ( (&) )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word32 )
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , choose
    , conjoin
    , counterexample
    , forAll
    , frequency
    , getPositive
    , listOf
    , property
    , (.&&.)
    , (===)
    , (==>)
    )

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "sparseCheckpoints" $ do
        it "k=2160, h=42" $ \_ -> do
            let cfg = SparseCheckpointsConfig
                    { edgeSize = 10
                    , epochStability = 2160
                    }
            let h = Quantity 42

            -- First unstable block: 0
            sparseCheckpoints cfg h `shouldBe`
                [ 0
                , 32,33,34,35,36,37,38,39,40,41 -- Short-term checkpoints
                , 42 -- Tip
                ]

        it "k=2160, h=2414" $ \_ -> do
            let cfg = SparseCheckpointsConfig
                    { edgeSize = 10
                    , epochStability = 2160
                    }
            let h = Quantity 2714
            -- First unstable block: 554
            sparseCheckpoints cfg h `shouldBe`
                [ 0
                , 720, 1440, 2160               -- Long-term checkpoints

                , 2704, 2705, 2706, 2707, 2708  -- Short-term checkpoints
                , 2709, 2710, 2711, 2712, 2713  -- edgeSize = 10

                , 2714  -- Tip
                ]

        it "k=2160, h=2414" $ \_ -> do
            let cfg = SparseCheckpointsConfig
                    { edgeSize = 0
                    , epochStability = 2160
                    }
            let h = Quantity 2714
            -- First unstable block: 554
            sparseCheckpoints cfg h `shouldBe`
                [ 0
                , 720, 1440, 2160               -- Long-term checkpoints
                , 2714  -- Tip
                ]

        it "The tip is always a checkpoint" $ \_ ->
            property prop_sparseCheckpointTipAlwaysThere

        it "There's at least (min h edgeSize) checkpoints" $ \_ ->
            property prop_sparseCheckpointMinimum

        it "∀ cfg. sparseCheckpoints (cfg { edgeSize = 0 }) ⊆ sparseCheckpoints cfg" $ \_ ->
            property prop_sparseCheckpointEdgeSize0

        it "Checkpoints are eventually stored in a sparse manner" $ \_ ->
            property prop_checkpointsEventuallyEqual

    describe "extendAndPrune" $ do
        it "actually prunes checkpoints" $
            property prop_doesPrune
        it "keeps the tip of the chain" $
            property prop_keepTip

{-------------------------------------------------------------------------------
    Checkpoint hygiene
-------------------------------------------------------------------------------}
-- | No matter what, the current tip is always a checkpoint.
prop_sparseCheckpointTipAlwaysThere
    :: GenSparseCheckpointsArgs
    -> Property
prop_sparseCheckpointTipAlwaysThere (GenSparseCheckpointsArgs cfg h) = prop
    & counterexample ("Checkpoints: " <> show cps)
    & counterexample ("h=" <> show h)
  where
    cps = sparseCheckpoints cfg (Quantity h)

    prop :: Property
    prop = property $ fromIntegral h `elem` cps

-- | Check that sparseCheckpoints always return at least edgeSize checkpoints (or
-- exactly the current height if h < edgeSize).
prop_sparseCheckpointMinimum
    :: GenSparseCheckpointsArgs
    -> Property
prop_sparseCheckpointMinimum (GenSparseCheckpointsArgs cfg h) = prop
    & counterexample ("Checkpoints: " <> show cps)
    & counterexample ("h=" <> show h)
  where
    cps = sparseCheckpoints cfg (Quantity h)

    prop :: Property
    prop = property $ fromIntegral (length cps) >= min e h
      where
        e = fromIntegral $ edgeSize cfg

-- | This property checks that, the checkpoints kept for an edge size of 0 are
-- included in the list with a non-null edge size, all else equals.
prop_sparseCheckpointEdgeSize0
    :: GenSparseCheckpointsArgs
    -> Property
prop_sparseCheckpointEdgeSize0 (GenSparseCheckpointsArgs cfg h) = prop
    & counterexample ("Checkpoints: " <> show cps)
    & counterexample ("h=" <> show h)
  where
    cps  = sparseCheckpoints cfg (Quantity h)
    cps' = sparseCheckpoints (cfg { edgeSize = 0 }) (Quantity h)

    prop :: Property
    prop = property (cps' `L.isSubsequenceOf` cps)

-- | This property shows that, for all possible cuts (i.e. non-null batches) of
-- a sequence of blocks, the following steps:
--
--  For all batch B in sequence:
--
--  - Keep all checkpoints in B yielded by sparseCheckpoint with and
--    edge size of 0.
--
--  - Keep the last checkpoint of the batch regardless
--
--  - Prune all checkpoints not yielded by sparseCheckpoint with a non-null edge
--    size
--
-- are equivalent to calling `sparseCheckpoints` on the flattened batch
-- sequence.
--
-- Note that the batch creation mimics the way blocks are served by the network
-- layer to wallets: first by batches of arbitrary size, and then one by one.
--
-- The property shows that regardless of how batches are served, after
-- 'edgeSize' one-by-one step, the end result is the same as if the entire
-- stream of blocks had been processed in one go.
prop_checkpointsEventuallyEqual
    :: GenSparseCheckpointsArgs
    -> Property
prop_checkpointsEventuallyEqual args@(GenSparseCheckpointsArgs cfg h) =
    h > epochStability cfg ==> forAll (genBatches args) $ \(Batches batches) ->
        let
            tip =
                Quantity $ last $ mconcat batches
            emptyDB =
                SparseCheckpointsDB []
            dbs =
                L.scanl (\db batch -> prune $ step batch db) emptyDB batches
        in
            ( prop_eventuallyReachesExpectedTip tip dbs
              .&&.
              prop_canNeverRollbackMoreThanKPlusGap tip dbs
            )
  where
    prop_eventuallyReachesExpectedTip
        :: Quantity "block" Word32
        -> [SparseCheckpointsDB]
        -> Property
    prop_eventuallyReachesExpectedTip tip dbs =
        last dbs === SparseCheckpointsDB (sparseCheckpoints cfg tip)

    prop_canNeverRollbackMoreThanKPlusGap
        :: Quantity "block" Word32
        -> [SparseCheckpointsDB]
        -> Property
    prop_canNeverRollbackMoreThanKPlusGap (Quantity tip) dbs =
        conjoin (forEachStep <$> L.tail dbs)
      where
        forEachStep (SparseCheckpointsDB db) =
            let
                -- db' contains all the _stable checkpoints_ in the database,
                -- i.e. those that are in the interval [0; network tip - k)
                --
                -- So, if we are asked to rollback for a full k, we'll end up
                -- rolling back to the closest checkpoint from that interval.
                db' = filter (< (tip - epochStability cfg)) db
                farthestRollback = last db - last db'
            in
                property
                    (farthestRollback <= epochStability cfg + gapSize cfg)
                & counterexample
                    ("database: " <> show db)
                & counterexample
                    ("stable checkpoints: " <> show db')

    step :: [Word32] -> SparseCheckpointsDB -> SparseCheckpointsDB
    step cps (SparseCheckpointsDB db) =
        let
            toKeep =
                sparseCheckpoints (cfg { edgeSize = 0 }) (Quantity h)
            cps' =
                last cps : (toKeep `L.intersect` cps)
        in
            SparseCheckpointsDB $ L.sort $ cps' ++ db

    prune :: SparseCheckpointsDB -> SparseCheckpointsDB
    prune (SparseCheckpointsDB db) =
        let
            tip =
                Quantity $ last db
            db' =
                sparseCheckpoints cfg tip `L.intersect` db
        in
            SparseCheckpointsDB db'

newtype Batches = Batches [[Word32]] deriving Show

newtype SparseCheckpointsDB = SparseCheckpointsDB [Word32] deriving (Show, Eq)

data GenSparseCheckpointsArgs
    = GenSparseCheckpointsArgs SparseCheckpointsConfig Word32
    deriving Show

instance Arbitrary GenSparseCheckpointsArgs where
    arbitrary = do
        k <- (\x -> 10 + (x `mod` 1000)) <$> arbitrary
        h <- (`mod` 10000) <$> arbitrary
        cfg <- SparseCheckpointsConfig <$> arbitrary <*> pure k
        pure $ GenSparseCheckpointsArgs cfg h

-- This functions generate `h` "block header" (modeled as a Word32), grouped in
-- batches of arbitrary (albeit meaningful) sizes.
--
-- Batches always end with `edgeSize` "block header" in singleton batches, to
-- simulate a fast and slow mode.
genBatches
    :: GenSparseCheckpointsArgs
    -> Gen Batches
genBatches (GenSparseCheckpointsArgs cfg h) = do
    bs <- go [0..h] []
    let e = fromIntegral $ edgeSize cfg
    let oneByOne = pure <$> [h+1..h+e]
    pure (Batches (bs ++ oneByOne))
  where
    go :: [Word32] -> [[Word32]] -> Gen [[Word32]]
    go []     batches = pure $ reverse batches
    go source batches = do
        -- NOTE:
        -- Generate batches that can be larger than the chosen gap size, to make
        -- sure we generate realistic cases.
        n <- fromIntegral <$> choose (1, 3 * gapSize cfg)
        go (drop n source) (take n source : batches)

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
