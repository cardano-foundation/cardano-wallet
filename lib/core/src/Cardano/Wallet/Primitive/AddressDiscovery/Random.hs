{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- An implementation of address discovery for the random address
-- scheme as used by the legacy Cardano wallets.

module Cardano.Wallet.Primitive.AddressDiscovery.Random
    (
    -- ** State
      RndState (..)
    , mkRndState
    , rndGenChange

    -- ** Low-level API
    , addDiscoveredAddress
    , deriveRndStateAddress
    , findUnusedPath
    , unavailablePaths
    ) where
import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Byron.Codec.Cbor
    ( decodeAddressDerivationPath, decodeAddressPayload, deserialiseCbor )
import Cardano.Wallet.Primitive.AddressDerivation
    ( AddressScheme (..)
    , Depth (..)
    , DerivationType (..)
    , Index (..)
    , Passphrase (..)
    , liftIndex
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey (..), deriveAccountPrivateKey, deriveAddressPrivateKey )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery (..), IsOurs (..), IsOwned (..), KnownAddresses (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..), ChimericAccount )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( join )
import Data.Map
    ( Map )
import Data.Maybe
    ( isJust )
import Data.Set
    ( Set )
import Fmt
    ( Buildable (..), blockMapF', indentF, tupleF )
import GHC.Generics
    ( Generic )
import System.Random
    ( RandomGen, StdGen, mkStdGen, randomR )

import qualified Data.Map as Map
import qualified Data.Set as Set

-- | HD random address discovery state and key material for AD.
data RndState = RndState
    { hdPassphrase :: Passphrase "addr-derivation-payload"
    -- ^ The HD derivation passphrase
    , accountIndex :: Index 'Hardened 'AccountK
    -- ^ The account index used for address _generation_ in this wallet. Note
    -- that addresses will be _discovered_ from any and all account indices,
    -- regardless of this value.
    , addresses :: Map DerivationPath Address
    -- ^ The addresses which have so far been discovered, and their
    -- derivation paths.
    , pendingAddresses :: Map DerivationPath Address
    -- ^ The addresses which have been generated for use as change
    -- addresses, and their derivation paths. Once addresses have been
    -- discovered they are removed from this set and added to 'addresses'.
    , gen :: StdGen
    -- ^ The state of the RNG.
    } deriving (Generic)

instance NFData RndState where
    rnf (RndState !_ !_ !_ !_ g) = seq (show g) ()

-- | There's no instance of 'Show' for 'XPrv'
instance Show RndState where
    show (RndState _key ix addrs pending g) = unwords
        [ "RndState <xprv>", p ix, p addrs, p pending, p g ]
      where
        p x = "(" ++ show x ++ ")"

instance Buildable RndState where
    build (RndState _ ix addrs pending g) = "RndState:\n"
        <> indentF 4 ("Account ix:       " <> build ix)
        <> indentF 4 ("Random Generator: " <> build (show g))
        <> indentF 4 ("Known addresses:  " <> blockMapF' tupleF build addrs)
        <> indentF 4 ("Change addresses: " <> blockMapF' tupleF build pending)

-- | Shortcut type alias for HD random address derivation path.
type DerivationPath = (Index 'WholeDomain 'AccountK, Index 'WholeDomain 'AddressK)

-- An address is considered to belong to the 'RndState' wallet if it can be
-- decoded as a Byron HD random address, and where the wallet key can be used
-- to decrypt the address derivation path.
instance IsOurs RndState Address where
    isOurs addr st =
        (isJust path, maybe id (addDiscoveredAddress addr) path st)
      where
        path = addressToPath addr (hdPassphrase st)

instance IsOurs RndState ChimericAccount where
    -- Chimeric accounts are not supported, so always return 'False'.
    isOurs _account state = (False, state)

instance IsOwned RndState ByronKey where
    isOwned st (key, pwd) addr =
        (, pwd) . deriveAddressKeyFromPath key pwd
            <$> addressToPath addr (hdPassphrase st)

addressToPath
    :: Address
    -> Passphrase "addr-derivation-payload"
    -> Maybe DerivationPath
addressToPath (Address addr) pwd = do
    payload <- deserialiseCbor decodeAddressPayload addr
    join $ deserialiseCbor (decodeAddressDerivationPath pwd) payload

-- | Initialize the HD random address discovery state from a root key and RNG
-- seed.
mkRndState :: ByronKey 'RootK XPrv -> Int -> RndState
mkRndState key seed = RndState
    { hdPassphrase = payloadPassphrase key
    , accountIndex = minBound
    , addresses = mempty
    , pendingAddresses = mempty
    , gen = mkStdGen seed
    }

-- | Updates a 'RndState' by adding an address and its derivation path to the
-- set of discovered addresses. If the address was in the 'pendingAddresses' set
-- (i.e. it was a newly generated change address), then it is removed from
-- there.
addDiscoveredAddress :: Address -> DerivationPath -> RndState -> RndState
addDiscoveredAddress addr path st =
    st { addresses = Map.insert path addr (addresses st)
       , pendingAddresses = Map.delete path (pendingAddresses st) }

rndGenChange
    :: AddressScheme ByronKey
    -> (ByronKey 'RootK XPrv, Passphrase "encryption")
    -> RndState
    -> (Address, RndState)
rndGenChange AddressScheme{addressFromKey} (rootXPrv, pwd) st = (address, st')
  where
    address = addressFromKey $ publicKey $ deriveAddressKeyFromPath rootXPrv pwd path
    (path, gen') = findUnusedPath (gen st) (accountIndex st)
        (unavailablePaths st)
    st' = st
        { pendingAddresses = Map.insert path address (pendingAddresses st)
        , gen = gen'
        }

-- | Returns the set of derivation paths that should not be used for new address
-- generation because they are already in use.
unavailablePaths :: RndState -> Set DerivationPath
unavailablePaths st = Map.keysSet $ addresses st <> pendingAddresses st

-- | Randomly generates an address derivation path for a given account. If the
-- path is already in the "blacklist", it will try generating another.
--
-- Note that performance of this function will degrade poorly as more of the
-- account's address space is used up. We may have to improve it later.
findUnusedPath
    :: StdGen
    -> Index 'Hardened 'AccountK
    -> Set DerivationPath
    -> (DerivationPath, StdGen)
findUnusedPath g accIx used
    | Set.notMember path used = (path, gen')
    | otherwise = findUnusedPath gen' accIx used
  where
    path = (liftIndex accIx, liftIndex addrIx)
    (addrIx, gen') = randomIndex g

randomIndex
    :: forall ix g. (RandomGen g, ix ~ Index 'Hardened 'AddressK)
    => g
    -> (ix, g)
randomIndex g = (Index ix, g')
  where
    (ix, g') = randomR (getIndex (minBound @ix), getIndex (maxBound @ix)) g


-- | Use the key material in 'RndState' to derive a change address for a given
-- derivation path.
deriveAddressKeyFromPath
    :: ByronKey 'RootK XPrv
    -> Passphrase "encryption"
    -> DerivationPath
    -> ByronKey 'AddressK XPrv
deriveAddressKeyFromPath rootXPrv passphrase (accIx, addrIx) = addrXPrv
  where
    accXPrv = deriveAccountPrivateKey passphrase rootXPrv accIx
    addrXPrv = deriveAddressPrivateKey passphrase accXPrv addrIx

-- | Use the key material in 'RndState' to derive a change address.
deriveRndStateAddress
    :: AddressScheme ByronKey
    -> ByronKey 'RootK XPrv
    -> Passphrase "encryption"
    -> DerivationPath
    -> Address
deriveRndStateAddress AddressScheme{addressFromKey} k passphrase path =
    addressFromKey $ publicKey $ deriveAddressKeyFromPath k passphrase path

-- Unlike sequential derivation, we can't derive an order from the index only
-- (they are randomly generated), nor anything else in the address itself.
--
-- Therefore, we'll simply consider that addresses using the random address
-- derivation scheme won't be ordered in any particular order.
instance CompareDiscovery RndState where
    compareDiscovery _ _ _ = EQ

instance KnownAddresses RndState where
    knownAddresses s = Map.elems (addresses s)
