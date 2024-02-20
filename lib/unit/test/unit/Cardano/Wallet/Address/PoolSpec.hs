module Cardano.Wallet.Address.PoolSpec
    ( spec
    , genPool
    , shrinkPool
    ) where

import Prelude

import Cardano.Wallet.Address.Pool
    ( Pool
    , addressFromIx
    , addresses
    , prop_consistent
    , prop_fresh
    , prop_gap
    )
import Cardano.Wallet.Primitive.Types.Address
    ( AddressState (..)
    )
import Data.Bifunctor
    ( second
    )
import Data.Foldable
    ( fold
    )
import Data.Functor.Identity
    ( Identity (..)
    )
import Data.List
    ( foldl'
    , sortOn
    )
import Data.Map
    ( Map
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Set
    ( Set
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Gen
    , Property
    , choose
    , counterexample
    , forAll
    , frequency
    , listOf
    , oneof
    , sized
    , (.&&.)
    , (===)
    )

import qualified Cardano.Wallet.Address.Pool as AddressPool
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "Pool invariants" $ do
        it "prop_consistent . new" $
            prop_consistent testPool

        it "generated pool always satisfies prop_gap and prop_fresh" $
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

        it "nextIndex always points to an unused address"
            prop_next_is_unused

    describe "Address discovery" $ do
        it "discover by query = discover in sequence" $
            prop_discover (AddressPool.new (*2) (6::Int))

{-------------------------------------------------------------------------------
    Properties
-------------------------------------------------------------------------------}

prop_updates :: (Ord addr, Ord ix, Enum ix) => Pool addr ix -> Property
prop_updates pool = forAll (genUsageForGap $ AddressPool.gap pool) $ \usage ->
    let addr    = AddressPool.addressFromIx pool
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
    addr = AddressPool.addressFromIx pool0
    g    = AddressPool.gap pool0

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

prop_discover
    :: Pool MockAddress Int -> Property
prop_discover pool0 = forAll (genAddresses pool0) prop
  where
    prop addrs =
        counterexample ("positions = " <> show positions) $
        counterexample ("pool0 = " <> show (AddressPool.addresses pool0)) $
            snd discoverQuery === discoverSequential
            .&&. fst discoverQuery === fold positions
      where
        discoverSequential =
            AddressPool.addresses
            $ foldl' (flip AddressPool.update) pool0 addrs

        positions :: Map MockAddress (Set Int)
        positions =
            Map.fromListWith (<>) $ zip addrs $ map Set.singleton [0..]

        query a = pure . fromMaybe mempty $ Map.lookup a positions

        discoverQuery =
            second AddressPool.addresses
            . runIdentity $ AddressPool.discover query pool0

prop_next_is_unused :: Property
prop_next_is_unused = forAll (genPool testPool) $ \p ->
    let i = AddressPool.nextIndex p
        indexState = filter ((== i) . fst) (Map.elems (addresses p))
    in  indexState == [(i, Unused)]

{-------------------------------------------------------------------------------
    Generators
-------------------------------------------------------------------------------}
type MockAddress = Int
type TestPool = Pool MockAddress Int

-- | Test pool with small parameters suitable for testing.
testPool :: TestPool
testPool = AddressPool.new id 5

-- | Fill a given pool from a list of address statuses
fromUsage :: (Ord addr, Enum ix) => Pool addr ix -> [AddressState] -> Pool addr ix
fromUsage pool = AddressPool.loadUnsafe pool
    . Map.fromList . zipWith decorate [(toEnum 0)..]
  where
    decorate ix status = (addressFromIx pool ix, (ix, status))

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

-- | Generate a random address 'Pool' that satisfies 'prop_consistent'
-- from existing pool (parameters).
genPool :: (Ord addr, Enum ix) => Pool addr ix -> Gen (Pool addr ix)
genPool pool = fromUsage pool <$> genUsageForGap (AddressPool.gap pool)

-- | Generate a sequence of addresses as they may appear on the blockchain.
-- This sequence may contain duplicates,
-- but respects the address gap of the given pool.
genAddresses :: Enum ix => Pool addr ix -> Gen [addr]
genAddresses pool = sized $ go 0
  where
    gap = AddressPool.gap pool
    go _    0 = pure []
    go maxi n = do
        i <- frequency
            [ (1, choose (0,min maxi 3)) -- generate duplicates
            , (3, choose (0,maxi))
            , (3, choose (maxi,maxi+gap-1))
            , (2, pure (maxi+gap-1))     -- generate at edge of the pool
            ]
        let addr = AddressPool.addressFromIx pool $ toEnum i
        (addr:) <$> go (max i maxi) (n-1)

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
    minimalPool = AddressPool.new (AddressPool.addressFromIx pool) minGap

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
