{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module contains types for address discovery. The two address discovery
-- schemes implemented are:
--
--  * "Cardano.Wallet.Primitive.AddressDiscovery.Sequential"
--  * "Cardano.Wallet.Primitive.AddressDiscovery.Random"

module Cardano.Wallet.Primitive.AddressDiscovery
    (
    -- * Abstractions
      IsOurs(..)
    , IsOwned(..)
    , GenChange(..)
    , CompareDiscovery(..)
    , KnownAddresses(..)
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPrv )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), Passphrase (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..) )

-- | Checks whether or not a given entity belongs to us.
--
-- This abstraction exists to give us the ability to keep the wallet business
-- logic agnostic to the address derivation and discovery mechanisms.
--
-- This is needed because two different address schemes lives on Cardano:
--
--   - A hierarchical random scheme:
--      rather 'custom' made, with several flaws; this is the original and now
--      legacy address scheme.
--
--   - A hierarchical sequential scheme:
--      a new scheme based on the BIP-0044 specification, which is better suited
--      for our present needs.
--
-- In practice, we will need a wallet that can support both, even if not at the
-- same time, and this little abstraction can buy us this without introducing
-- too much overhead.
class IsOurs s entity where
    isOurs
        :: entity
        -> s
        -> (Bool, s)
        -- ^ Checks whether an entity is ours or not.

-- | More powerful than 'isOurs', this abstractions offer the underlying state
-- the ability to find / compute the address private key corresponding to a
-- given known address.
--
-- Requiring 'IsOwned' as a constraint supposed that there is a way to recover
-- the root private key of a particular wallet. This isn't true for externally
-- owned wallet which would delegate its key management to a third party (like
-- a hardware Ledger or Trezor).
class IsOurs s Address => IsOwned s key where
    isOwned
        :: s
        -> (key 'RootK XPrv, Passphrase "encryption")
        -> Address
        -> Maybe (key 'AddressK XPrv, Passphrase "encryption")
        -- ^ Derive the private key corresponding to an address. Careful, this
        -- operation can be costly. Note that the state is discarded from this
        -- function as we do not intend to discover any addresses from this
        -- operation; This is merely a lookup from known addresses.

-- | Abstracting over change address generation. In theory, this is only needed
-- for sending transactions on a wallet following a particular scheme. This
-- abstractions allows for defining an heuristic to pick new change address. For
-- instance, in BIP-44, change addresses belong to a particular change chain
-- (also called "Internal Chain").
class GenChange s where
    type ArgGenChange s :: *
    genChange
        :: ArgGenChange s
        -> s
        -> (Address, s)
        -- ^ Generate a new change address for the given scheme. The rules for
        -- generating a new change address depends on the underlying scheme.

-- | Ordering addresses by discovery date.
--
-- If `a1` has been discovered before `a2`, then the following equation holds:
--
-- @
-- compareDiscovery s a1 a2 == LT
-- @
--
-- If `a1` has been discovered after `a2`, then the following equation holds:
--
-- @
-- compareDiscovery s a1 a2 == GT
-- @
--
-- Note that, if an address isn't known it is considered not discovered and
-- therefore, is always _greater than_ any known address.
class CompareDiscovery s where
    compareDiscovery
        :: s
        -> Address
        -> Address
        -> Ordering

-- | Extract the list of all known addresses.
--
-- NOTE: Change addresses aren't considered "known" until they've been used. The
-- rationale is that, we don't want users or consumers of the wallet to be using
-- change addresses prematurely.
class KnownAddresses s where
    knownAddresses
        :: s
        -> [Address]
