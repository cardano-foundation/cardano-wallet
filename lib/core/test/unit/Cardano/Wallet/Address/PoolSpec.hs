module Cardano.Wallet.Address.PoolSpec
    ( spec
    , genPool
    , shrinkPool
    ) where

import Prelude

import Cardano.Wallet.Address.Pool
    ( Pool, addresses, generator, prop_consistent, prop_fresh, prop_gap )
import Cardano.Wallet.Primitive.Types.Address
    ( AddressState (..) )
import Data.List
    ( sortOn )
import Test.Hspec
    ( Spec, describe, it, parallel )
import Test.QuickCheck
    ( Gen, Property, choose, forAll, listOf, oneof, sized, (===) )

import qualified Cardano.Wallet.Address.Pool as AddressPool
import qualified Data.Map.Strict as Map

{-------------------------------------------------------------------------------
    Properties
-------------------------------------------------------------------------------}
spec :: Spec
spec = do
    parallel $ describe "Cardano.Wallet.Address.Pool" $ do
        it "prop_consistent . new" $
            prop_consistent testPool

        it "generator satisfies prop_gap and prop_fresh" $
            forAll (genUsageForGap $ AddressPool.gap testPool) $ \usage ->
                let pool = fromUsage testPool usage
                in  all ($ pool) [prop_gap, prop_fresh]

        it "sequence of updates preserves invariants" $
            prop_updates testPool

        it "order of updates (within range) is irrelevant for pool content" $
            prop_updates_order $ AddressPool.new (id :: Int -> Int) 3

        it "update does nothing on addresses outside the gap" $
            let p1 = testPool
                p2 = AddressPool.update (AddressPool.gap p1 + 1) p1
            in  addresses p1 === addresses p2

prop_updates :: (Ord addr, Ord ix, Enum ix) => Pool addr ix -> Property
prop_updates pool = forAll (genUsageForGap $ AddressPool.gap pool) $ \usage ->
    let addr ix = AddressPool.generator pool ix
        g       = AddressPool.gap pool
        addrs1  = [ addr ix | (ix,Used) <- zip [toEnum 0..] usage ]
        addrs2  = map (addr . toEnum) [0..2*g]
        pool1   = foldl (flip AddressPool.update) pool (addrs1 <> addrs2)
    in  prop_consistent pool1

prop_updates_order
    :: (Ord addr, Ord ix, Enum ix, Show addr, Show ix)
    => Pool addr ix -> Property
prop_updates_order pool0 = forAll genUpdates $ \pool ->
      AddressPool.addresses pool
    ===
      AddressPool.addresses (applyUsageInOrder pool0 $ toUsage pool)
  where
    addr ix = AddressPool.generator pool0 ix
    g       = AddressPool.gap pool0

    -- generate and apply a random sequence of updates
    genUpdates = sized $ randomUpdates pool0
    randomUpdates pool 0 = pure pool
    randomUpdates pool n = do
        let end = AddressPool.size pool - 1
        ix <- toEnum <$>
            oneof [choose (0,end), choose (end-(g-1),end)]
        randomUpdates (AddressPool.update (addr ix) pool) (n-1)

    applyUsageInOrder pool usage =
        foldl (flip AddressPool.update) pool
            [ addr ix | (ix,Used) <- zip [toEnum 0..] usage ]
    toUsage = map snd . sortOn fst . Map.elems . AddressPool.addresses

{-------------------------------------------------------------------------------
    Generators
-------------------------------------------------------------------------------}
type TestPool = Pool Int Int

-- | Test pool with small parameters suitable for testing.
testPool :: TestPool
testPool = AddressPool.new id 5

-- | Fill a given pool from a list of address statuses
fromUsage :: (Ord addr, Enum ix) => Pool addr ix -> [AddressState] -> Pool addr ix
fromUsage pool = AddressPool.loadUnsafe pool
    . Map.fromList . zipWith decorate [(toEnum 0)..]
  where
    decorate ix status = (generator pool ix, (ix, status))

-- | Generate address statuses that respect the address gap.
genUsageForGap :: Int -> Gen [AddressState]
genUsageForGap gap = do
    xs <- listOf uu
    y  <- used
    pure $ concat xs <> y <> replicate gap Unused
  where
    used   = flip replicate Used <$> oneof [choose (1,3), choose (gap+1,2*gap)]
    unused = flip replicate Unused <$> choose (0,gap-1)
    uu     = (<>) <$> used <*> unused

-- | Generate a random address Pool that satisfies 'prop_consistent'
-- from existing pool (parameters).
genPool :: (Ord addr, Enum ix) => Pool addr ix -> Gen (Pool addr ix)
genPool pool = fromUsage pool <$> genUsageForGap (AddressPool.gap pool) 

{-------------------------------------------------------------------------------
    Shrinkers
-------------------------------------------------------------------------------}
-- | Shrink an address pool. The gap will be shrunk as well.
shrinkPool
    :: (Ord addr, Enum ix)
    => Int -- ^ minimum gap to shrink to
    -> Pool addr ix -> [Pool addr ix]
shrinkPool minGap pool
    | k == gap && gap == minGap = []
    | k == gap && gap >  minGap = [ minimalPool ]
    | otherwise =
        [ minimalPool
        , AddressPool.clear pool
        , AddressPool.loadUnsafe pool $ removeN n gap $ AddressPool.addresses pool
        ]
  where
    k = AddressPool.size pool
    n = gap `div` 5
    gap = AddressPool.gap pool
    minimalPool = AddressPool.new (AddressPool.generator pool) minGap

-- | Remove the top @n@ indices and restore the upper @gap@ indices to 'Unused'.
--
-- Note: This function is only used for shrinking and not unit tested.
-- This is ok, because a bug in the shrinker only affects
-- our ability to find the cause of a bug,
-- but does not affect the visibility of said bug.
removeN
    :: Enum ix
    => Int -> Int
    -> Map.Map addr (ix, AddressState)
    -> Map.Map addr (ix, AddressState)
removeN n gap addrs = Map.map unuse $ Map.filter (not . isTop) addrs
  where
    top = Map.size addrs - 1 -- topmost index
    isTop (ix,_) = top <= fromEnum ix + (n-1)
    unuse a@(ix,_)
        | top <= fromEnum ix + (n-1) + gap = (ix, Unused)
        | otherwise = a
