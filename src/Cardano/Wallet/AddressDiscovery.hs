{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- This module contains primitives necessary to perform address discovery. So
-- far, we're only interested in address following a sequential derivation
-- scheme as specified in BIP-0044. Later, we may introduce backward
-- compatibility with random address scheme from the legacy Cardano wallets.

module Cardano.Wallet.AddressDiscovery
    ( -- * Sequential Derivation

    -- ** Address Pool Gap
      AddressPoolGap
    , MkAddressPoolGapError (..)
    , defaultAddressPoolGap
    , mkAddressPoolGap

    -- ** Address Pool
    , AddressPool
    , gap
    , addresses
    , changeChain
    , accountPubKey
    , mkAddressPool
    , lookupAddress
  ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPub )
import Cardano.Wallet.AddressDerivation
    ( ChangeChain (..)
    , Depth (..)
    , DerivationType (..)
    , Index
    , Key
    , deriveAddressPublicKey
    , keyToAddress
    )
import Cardano.Wallet.Primitive
    ( Address, invariant )
import Control.DeepSeq
    ( NFData )
import Data.Function
    ( (&) )
import Data.List
    ( sortOn )
import Data.Map.Strict
    ( Map )
import Data.Word
    ( Word8 )
import GHC.Generics
    ( Generic )

import qualified Data.Map.Strict as Map


{-------------------------------------------------------------------------------
                          Sequential Derivation

    Discovery of addresses in the sequential derivation as specified by BIP-44.
    The management of _accounts_ is left-out for this implementation focuses on
    a single account. In practice, one wants to manage a set of pools, one per
    account.

-------------------------------------------------------------------------------}

-- ** Address Pool Gap

-- | Maximum number of consecutive undiscovered addresses allowed
newtype AddressPoolGap = AddressPoolGap
    { getAddressPoolGap :: Word8 }
    deriving stock (Generic, Show, Eq, Ord)

instance NFData AddressPoolGap

instance Bounded AddressPoolGap where
    minBound = AddressPoolGap 10
    maxBound = AddressPoolGap 100

instance Enum AddressPoolGap where
    fromEnum (AddressPoolGap g) = fromEnum g
    toEnum g
        | AddressPoolGap (toEnum g) < minBound @AddressPoolGap =
            error "AddressPoolGap.toEnum: bad argument"
        | AddressPoolGap (toEnum g) > maxBound @AddressPoolGap =
            error "AddressPoolGap.toEnum: bad argument"
        | otherwise =
            AddressPoolGap (toEnum g)

-- | Smart constructor for 'AddressPoolGap'
mkAddressPoolGap :: Word8 -> Either MkAddressPoolGapError AddressPoolGap
mkAddressPoolGap !g
    | g >= getAddressPoolGap minBound &&
      g <= getAddressPoolGap maxBound = Right $ AddressPoolGap g
    | otherwise = Left $ ErrGapOutOfRange g

-- | Possible errors when casting to an 'AddressPoolGap'
newtype MkAddressPoolGapError = ErrGapOutOfRange Word8
    deriving (Eq, Show)

-- | A default 'AddressPoolGap', as suggested in BIP-0044
defaultAddressPoolGap :: AddressPoolGap
defaultAddressPoolGap =
    AddressPoolGap 20


-- ** Address Pool

-- | An 'AddressPool' which keeps track of sequential addresses within a given
-- Account and change chain. See 'mkAddressPool' to create a new or existing
-- pool:
--
-- >>> mkAddressPool xpub gap changeChain mempty
-- AddressPool { }
data AddressPool = AddressPool
    { accountPubKey
        :: !(Key 'AccountK XPub)
        -- ^ Corresponding key for the pool (a pool is tied to only one account)
    , gap
        :: !AddressPoolGap
        -- ^ The actual gap for the pool. This can't change for a given pool.
    , changeChain
        :: !ChangeChain
        -- ^ Whether this pool tracks addrs on the internal or external chain
    , indexedAddresses
        :: !(Map Address (Index 'Soft 'AddressK))
    } deriving (Generic, Show, Eq)

instance NFData AddressPool

-- | Get all addresses in the pool, sorted from the first address discovered,
-- up until the next one.
--
-- In practice, we always have:
--
-- > mkAddressPool key g cc (addresses pool) == pool
addresses :: AddressPool -> [Address]
addresses = map fst . sortOn snd . Map.toList . indexedAddresses

-- | Create a new Address pool from a list of addresses. Note that, the list is
-- expected to be ordered in sequence (first indexes, first in the list).
--
-- The pool will grow from the start if less than @g :: AddressPoolGap@ are
-- given, such that, there are always @g@ undiscovered addresses in the pool.
mkAddressPool
    :: Key 'AccountK XPub
    -> AddressPoolGap
    -> ChangeChain
    -> [Address]
    -> AddressPool
mkAddressPool key g cc addrs = AddressPool
    { accountPubKey = key
    , gap = g
    , changeChain = cc
    , indexedAddresses = nextAddresses key g cc minBound <>
        Map.fromList (zip addrs [minBound..maxBound])
    }


-- | Lookup an address in the pool. When we find an address in a pool, the pool
-- may be amended if the address was discovered near the edge. It is also
-- possible that the pool is not amended at all - this happens in the case that
-- an address is discovered 'far' from the edge.
lookupAddress
    :: Address
    -> AddressPool
    -> (Maybe (Index 'Soft 'AddressK), AddressPool)
lookupAddress !target !pool =
    case Map.lookup target (indexedAddresses pool) of
        Just ix ->
            (Just ix, extendAddressPool ix pool)
        Nothing ->
            (Nothing, pool)

-- | If an address is discovered near the edge, we extend the address sequence,
-- otherwise we return the pool untouched.
extendAddressPool
    :: Index 'Soft 'AddressK
    -> AddressPool
    -> AddressPool
extendAddressPool !ix !pool
    | isOnEdge  = pool { indexedAddresses = indexedAddresses pool <> next }
    | otherwise = pool
  where
    edge = Map.size (indexedAddresses pool)
    isOnEdge = edge - fromEnum ix <= fromEnum (gap pool)
    next = if ix == maxBound then mempty else nextAddresses
        (accountPubKey pool)
        (gap pool)
        (changeChain pool)
        (succ ix)

-- | Compute the pool extension from a starting index
nextAddresses
    :: Key 'AccountK XPub
    -> AddressPoolGap
    -> ChangeChain
    -> Index 'Soft 'AddressK
    -> Map Address (Index 'Soft 'AddressK)
nextAddresses !key (AddressPoolGap !g) !cc !fromIx =
    [fromIx .. min maxBound toIx]
        & map (\ix -> (newAddress ix, ix))
        & Map.fromList
  where
    toIx = invariant
        "nextAddresses: toIx should be greater than fromIx"
        (toEnum $ fromEnum fromIx + fromEnum g - 1)
        (>= fromIx)
    newAddress = keyToAddress . deriveAddressPublicKey key cc
