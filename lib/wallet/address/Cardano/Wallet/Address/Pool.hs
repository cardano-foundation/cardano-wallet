{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
-- An address pool caches a collection of addresses.
-- The purpose of this data structure is to aid in BIP-44 style
-- address discovery with an address gap.
module Cardano.Wallet.Address.Pool
    ( Pool
    , addressFromIx
    , addresses
    , usedAddresses
    , gap
    , lookup
    , size
    , successor
    , new
    , load
    , update
    , clear
    , nextIndex

    -- * Address Discovery
    , discover

    -- * Internal
    , loadUnsafe
    , prop_sequence
    , prop_gap
    , prop_fresh
    , prop_fromIx
    , prop_consistent
    )
  where

import Prelude hiding
    ( last, lookup )

import Cardano.Wallet.Primitive.Types.Address
    ( AddressState (..) )
import Control.DeepSeq
    ( NFData )
import Data.Map.Strict
    ( Map )
import Data.Ord
    ( Down (..) )
import Fmt
    ( Buildable (..) )
import GHC.Generics
    ( Generic )

import qualified Data.List as L
import qualified Data.Map.Strict as Map

{-------------------------------------------------------------------------------
    Address Pool, abstract data type
-------------------------------------------------------------------------------}
-- | An address pool caches a collection of addresses (type @addr@)
-- which are derived from a numeric index (type @ix@).
data Pool addr ix = Pool
    { addressFromIx :: ix -> addr
    -- ^ Mapping from a numeric index to its corresponding address.
    --
    -- This mapping is supposed to be (practically) a one-way function:
    -- Given an 'addr', it is impossible to compute the preimage
    -- 'ix' in practice.
    -- The purpose of the 'Pool' data structure is to help inverting
    -- this function regardless. The idea is that addresses
    -- with small indices @0,1,…@ are 'Used' before addresses with larger
    -- indices; specifically, only less than 'gap' many addresses in sequence
    -- may be 'Unused' before the next 'Used' address.
    -- This usage scheme restricts the search space considerably
    -- and allows us to practically invert the 'addressFromIx' function.
    , gap :: Int
    -- ^ The pool gap determines how 'Used' and 'Unused'
    -- have to be distributed.
    -- See 'prop_gap' and 'prop_fresh'.
    , addresses :: Map addr (ix, AddressState)
    -- ^ Partial, cached inverse of the 'addressFromIx'.
    -- This map contains all cached addresses @addr@,
    -- their corresponding indices @ix@,
    -- and whether they are 'Used' or 'Unused'.
    -- See 'prop_sequence'.
    } deriving (Generic)

instance (NFData addr, NFData ix) => NFData (Pool addr ix)

-- | Internal invariant:
-- The indices of the addresses in a pool form a finite
-- sequence beginning with 'fromEnum'@ 0@.
prop_sequence :: (Ord ix, Enum ix) => Pool addr ix -> Bool
prop_sequence Pool{addresses} =
    indices `L.isPrefixOf` [toEnum 0..]
  where
    indices = L.sort $ map fst $ Map.elems addresses

-- | Internal invariant:
-- If we order the 'addresses' by their indices,
-- then there are always /less than/ 'gap' many 'Unused'
-- addresses between two consecutive 'Used' addresses,
-- or before the first 'Used' address.
prop_gap :: Ord ix => Pool addr ix -> Bool
prop_gap Pool{gap,addresses}
    = all (< gap) . consecutiveUnused . L.group $ statuses
  where
    consecutiveUnused ((Used:_):xs) = consecutiveUnused xs
    consecutiveUnused (x@(Unused:_):(Used:_):xs) =
        length x : consecutiveUnused xs
    consecutiveUnused _ = []

    statuses = map snd $ L.sortOn fst $ Map.elems addresses

-- | Internal invariant:
-- If we order the 'addresses' by their indices,
-- there are exactly 'gap' many 'Unused' addresses after the last
-- 'Used' address.
prop_fresh :: Ord ix => Pool addr ix -> Bool
prop_fresh Pool{gap,addresses} =
    takeWhile (== Unused) end == replicate gap Unused
  where
    end = map snd $ L.sortOn (Down . fst) $ Map.elems addresses

-- | Internal invariant:
-- All 'addresses' in the pool have been generated from their index
-- via the pool 'addressFromIx'.
prop_fromIx :: Eq addr => Pool addr ix -> Bool
prop_fromIx Pool{addressFromIx,addresses} =
    and $ Map.mapWithKey isGenerated addresses
  where
    isGenerated addr (ix,_) = addressFromIx ix == addr

-- | Internal invariant: The pool satisfies all invariants above.
prop_consistent :: (Ord ix, Enum ix, Eq addr) => Pool addr ix -> Bool
prop_consistent p = all ($ p)
    [ prop_sequence
    , prop_gap
    , prop_fresh
    , prop_fromIx
    ]

{-------------------------------------------------------------------------------
    Pretty printing
-------------------------------------------------------------------------------}
instance Buildable (Pool addr ix) where
    build pool = "AddressPool "
        <> "{ " <> build (size pool) <> " addresses"
        <> ", gap = " <> build (gap pool)
        <> "}"

instance (Show addr, Show ix) => Show (Pool addr ix) where
    show pool = "AddressPool"
        <> "{ addressFromIx = <<function>>"
        <> ", gap = " <> show (gap pool)
        <> ", addresses = " <> show (addresses pool)
        <> "}"

{-------------------------------------------------------------------------------
    Address Pool, operations
-------------------------------------------------------------------------------}
-- | Create a new address pool.
new :: (Ord addr, Enum ix) => (ix -> addr) -> Int -> Pool addr ix
new addressFromIx gap
    = ensureFresh (toEnum 0) $ Pool{ addressFromIx, gap, addresses = Map.empty }

-- | Replace the collection of addresses in a pool,
-- but only if this collection satisfies the necessary invariants
-- such as 'prop_sequence' etc.
load
    :: (Ord addr,  Ord ix, Enum ix)
    => Pool addr ix -> Map addr (ix,AddressState) -> Maybe (Pool addr ix)
load pool0 addrs = if prop_consistent pool then Just pool else Nothing
  where pool = loadUnsafe pool0 addrs

-- | Replace the collection of addresses in a pool,
-- but skips checking the invariants.
loadUnsafe :: Pool addr ix -> Map addr (ix,AddressState) -> Pool addr ix
loadUnsafe pool addrs = pool{ addresses = addrs }

-- | Remove all previously discovered addresses,
-- i.e. create a new pool with the same 'addressFromIx' and 'gap' as the old pool.
clear :: (Ord addr, Enum ix) => Pool addr ix -> Pool addr ix
clear Pool{addressFromIx,gap} = new addressFromIx gap

-- | Look up an address in the pool.
lookup :: Ord addr => addr -> Pool addr ix -> Maybe ix
lookup addr Pool{addresses} = fst <$> Map.lookup addr addresses

-- | Sorted list of all addresses that are marked 'Used' in the pool.
usedAddresses :: Pool addr ix -> [addr]
usedAddresses pool =
    [ addr | (addr,(_,Used)) <- Map.toList $ addresses pool ]

-- | The first index that is 'Unused' and that comes after any 'Used' index.
nextIndex :: Enum ix => Pool addr ix -> ix
nextIndex Pool{addresses,gap} = toEnum (Map.size addresses - gap)

-- | Number of addresses cached in the pool.
size :: Pool addr ix -> Int
size = Map.size . addresses

-- | Given an index @ix@, return the enumerated successor @Just (succ ix)@
-- as long as the address corresponding to this successor is still
-- in the pool.
--
-- This function is useful for address discovery in a light client setting,
-- where the discovery procedure is:
-- Start with index @ix = 0@, query the corresponding address in an explorer,
-- @update@ address pool and repeat with @successor ix@ until the latter
-- returns 'Nothing'. According to the BIP-44 standard,
-- the account may not contain any other addresses than the ones discovered.
--
-- This function is not useful for generating change addresses,
-- as it does not take 'Used' or 'Unused' status into account.
successor :: Enum ix => Pool addr ix -> ix -> Maybe ix
successor Pool{addresses} ix = let jx = succ ix in
    if fromEnum jx >= Map.size addresses then Nothing else Just jx

-- | Update an address to the 'Used' status
-- and create new 'Unused' addresses in order to satisfy 'prop_fresh'.
--
-- Does nothing if the address was not in the pool.
update :: (Ord addr, Enum ix) => addr -> Pool addr ix -> Pool addr ix
update addr pool@Pool{addresses} =
    case Map.lookup addr addresses of
        Nothing     -> pool
        Just (ix,_) -> ensureFresh (succ ix) $ pool
            { addresses = Map.adjust (\(i,_) -> (i, Used)) addr addresses }

-- | Create additional 'Unused' addresses from larger indices
-- in order to satisfy 'prop_fresh' again.
--
-- Preconditions:
--
-- * The index @ix@ satisfies:
--
--     either @ix = fromEnum 0@
--     or @ix = succ jx@ and the index @jx@ is a 'Used' address.
--
-- * All addresses with index @ix@ or larger are 'Unused'.
ensureFresh :: (Ord addr, Enum ix) => ix -> Pool addr ix -> Pool addr ix
ensureFresh ix pool@Pool{addressFromIx,gap,addresses}
    = pool { addresses = Map.union addresses nexts }
  where
    fresh = toEnum $ Map.size addresses -- first index that is not in the pool
    nexts = Map.fromList
        [ (addressFromIx i, (i, Unused)) | i <- [fresh .. to] ]
      where
        to = toEnum $ fromEnum ix + fromIntegral gap - 1
        -- example:
        --  ix = 0 && fresh = 0 && gap = 20 `implies` [fresh .. to] = [0..19]

{-------------------------------------------------------------------------------
    Address discovery using an address pool
-------------------------------------------------------------------------------}
-- | Discover transactions and addresses by
-- using an efficient query @addr -> m txs@ and an address pool.
discover
    :: (Enum ix, Ord addr, Monad m, Monoid txs, Eq txs)
    => (addr -> m txs) -> Pool addr ix -> m (txs, Pool addr ix)
discover query pool0 =
    go mempty pool0 $ toEnum 0
  where
    go !txs1 !pool1 old = do
        -- TODO: Maybe cache the `addressFromIx` in the Pool using lazy evaluation.
        let addr = addressFromIx pool0 old
        newtxs <- query addr
        let (pool2, txs2) = if mempty == newtxs
                then (pool1, txs1)
                else (update addr pool1, txs1 <> newtxs)
        case successor pool2 old of
            Nothing    -> pure (txs2, pool2)
            Just next  -> go txs2 pool2 next
