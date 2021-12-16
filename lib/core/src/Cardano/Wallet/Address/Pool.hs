{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
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
    , generator
    , addresses
    , gap
    , lookup
    , size
    , next
    , new
    , load
    , update
    , clear

    -- * Internal
    , loadUnsafe
    , prop_sequence
    , prop_gap
    , prop_fresh
    , prop_generator
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

{- HLINT ignore "Avoid restricted qualification" -}
import qualified Data.List as List
import qualified Data.Map.Strict as Map

{-------------------------------------------------------------------------------
    Address Pool, abstract data type
-------------------------------------------------------------------------------}
-- | An address pool caches a collection of addresses (type @addr@)
-- which are derived from a numeric index (type @ix@).
data Pool addr ix = Pool
    { generator :: ix -> addr
    -- ^ Each address is obtained from a numeric index.
    -- The purpose of the 'Pool' data structure is to (partially)
    -- cache this mapping,
    -- because it is expensive to compute (hashing)
    -- and its inverse is practically impossible to compute.
    , gap :: Int
    -- ^ The pool gap determines how 'Used' and 'Unused'
    -- have to be distributed.
    -- See 'prop_gap' and 'prop_fresh'.
    , addresses :: Map addr (ix, AddressState)
    -- ^ Partial, cached inverse of the 'generator'.
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
    indices `List.isPrefixOf` [toEnum 0..]
  where
    indices = List.sort $ map fst $ Map.elems addresses

-- | Internal invariant:
-- If we order the 'addresses' by their indices,
-- then there are always /less than/ 'gap' many 'Unused'
-- addresses between two consecutive 'Used' addresses.
prop_gap :: Ord ix => Pool addr ix -> Bool
prop_gap Pool{gap,addresses}
    = all ((< gap) . length)
    . filter isUnused
    . dropFresh
    $ List.group statuses
  where
    isUnused (Unused:_) = True
    isUnused _ = False
    dropFresh = drop 1 -- drop items that are checked by 'prop_fresh'.
    statuses = map snd $ List.sortOn (Down . fst) $ Map.elems addresses

-- | Internal invariant:
-- If we order the 'addresses' by their indices,
-- there are exactly 'gap' many 'Unused' addresses after the last
-- 'Used' address.
prop_fresh :: Ord ix => Pool addr ix -> Bool
prop_fresh Pool{gap,addresses} =
    takeWhile (== Unused) end == replicate gap Unused
  where
    end = map snd $ List.sortOn (Down . fst) $ Map.elems addresses

-- | Internal invariant:
-- All 'addresses' in the pool have been generated from their index
-- via the pool 'generator'.
prop_generator :: Eq addr => Pool addr ix -> Bool
prop_generator Pool{generator,addresses} =
    and $ Map.mapWithKey isGenerated addresses
  where
    isGenerated addr (ix,_) = generator ix == addr

-- | Internal invariant: The pool satisfies all invariants above.
prop_consistent :: (Ord ix, Enum ix, Eq addr) => Pool addr ix -> Bool
prop_consistent p =
    all ($ p) [prop_sequence, prop_gap, prop_fresh, prop_generator]

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
        <> "{ generator = <<function>>"
        <> ", gap = " <> show (gap pool)
        <> ", addresses = " <> show (addresses pool)
        <> "}"

{-------------------------------------------------------------------------------
    Address Pool, operations
-------------------------------------------------------------------------------}
-- | Create a new address pool.
new :: (Ord addr, Enum ix) => (ix -> addr) -> Int -> Pool addr ix
new generator gap
    = ensureFresh (toEnum 0) $ Pool{ generator, gap, addresses = Map.empty }

-- | Replace the collection of addresses in a pool,
-- but only if this collection satisfies the necessary invariants
-- such as 'prop_sequence' etc.
load
    :: (Ord addr,  Ord ix, Enum ix)
    => Pool addr ix -> Map addr (ix,AddressState) -> Maybe (Pool addr ix)
load addrs pool0 = if prop_consistent pool then Just pool else Nothing
  where pool = loadUnsafe addrs pool0

-- | Replace the collection of addresses in a pool,
-- but skips checking the invariants.
loadUnsafe :: Pool addr ix -> Map addr (ix,AddressState) -> Pool addr ix
loadUnsafe pool addrs = pool{ addresses = addrs }

-- | Remove all previously discovered addresses,
-- i.e. create a new pool with the same 'generator' and 'gap' as the old pool.
clear :: (Ord addr, Enum ix) => Pool addr ix -> Pool addr ix
clear Pool{generator,gap} = new generator gap

-- | Look up an address in the pool.
lookup :: Ord addr => addr -> Pool addr ix -> Maybe ix
lookup addr Pool{addresses} = fst <$> Map.lookup addr addresses

-- | Number of addresses cached in the pool.
size :: Pool addr ix -> Int
size = Map.size . addresses

-- | Given an index, retrieve the next index that is still in the pool.
next :: Enum ix => Pool addr ix -> ix -> Maybe ix
next Pool{addresses} ix = let jx = succ ix in
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
-- Precondition: Either @ix = fromEnum 0@,
-- or the index @jx@ which satisfies to @ix = succ jx@
-- is associated with a 'Used' address.
ensureFresh :: (Ord addr, Enum ix) => ix -> Pool addr ix -> Pool addr ix
ensureFresh ix pool@Pool{generator,gap,addresses}
    = pool { addresses = Map.union addresses nexts }
  where
    fresh = toEnum $ Map.size addresses -- first index that is not in the pool
    nexts = Map.fromList
        [ (generator i, (i, Unused)) | i <- [fresh .. to] ]
      where
        to = toEnum $ fromEnum ix + fromIntegral gap - 1
        -- example:
        --  ix = 0 && fresh = 0 && gap = 20 `implies` [fresh .. to] = [0..19]
