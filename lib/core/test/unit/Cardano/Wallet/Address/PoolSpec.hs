module Cardano.Wallet.Address.PoolSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Address.Pool
    ( Pool, addresses, generator, prop_consistent, prop_fresh, prop_gap )
import Cardano.Wallet.Primitive.Types.Address
    ( AddressState (..) )
import Test.Hspec
    ( Spec, describe, it, parallel )
import Test.QuickCheck
    ( Gen, Property, choose, forAll, listOf, oneof, (===) )

import qualified Cardano.Wallet.Address.Pool as AddressPool
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    parallel $ describe "Cardano.Wallet.Address.Pool" $ do
        it "prop_consistent . new" $
            prop_consistent testPool

        it "generator satisfies prop_gap and prop_fresh" $
            forAll genUsageWithGap $ \usage ->
                let pool = fromUsage usage
                in  all ($ pool) [prop_gap, prop_fresh]

        it "sequence of updates preserves invariants"
            prop_updates

        it "update does nothing on addresses outside the gap" $
            let p1 = testPool
                p2 = AddressPool.update (AddressPool.gap p1 + 1) p1
            in  addresses p1 === addresses p2

prop_updates :: Property
prop_updates = forAll genUsageWithGap $ \usage ->
    let addr ix = AddressPool.generator testPool ix
        g       = AddressPool.gap testPool
        addrs1  = [ addr ix | (ix,Used) <- zip [0..] usage ]
        addrs2  = map addr [0..2*g]
        pool    = foldl (flip AddressPool.update) testPool (addrs1 <> addrs2)
    in  prop_consistent pool

{-------------------------------------------------------------------------------
    Generators
-------------------------------------------------------------------------------}
type TestPool = Pool Int Int

-- | Test pool with small parameters suitable for testing.
testPool :: TestPool
testPool = AddressPool.new id 5

{- HLINT ignore "Use zipWith" -}
-- | Make a testing pool from a given list of address statuses
fromUsage :: [AddressState] -> TestPool
fromUsage = AddressPool.loadUnsafe testPool
    . Map.fromList . map decorate . zip [0..]
  where
    decorate (ix,status) = (generator testPool ix, (ix, status))

-- | Generate address statuses that respect the address gap.
genUsageWithGap :: Gen [AddressState]
genUsageWithGap = do
    xs <- listOf uu
    y  <- used
    pure $ concat xs <> y <> replicate gap Unused
  where
    gap    = AddressPool.gap testPool
    used   = flip replicate Used <$> oneof [choose (1,3), choose (gap+1,2*gap)]
    unused = flip replicate Unused <$> choose (0,gap-1)
    uu     = (<>) <$> used <*> unused
