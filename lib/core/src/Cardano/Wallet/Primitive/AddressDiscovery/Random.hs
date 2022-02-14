{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    , RndStateLike
    , mkRndState
    , DerivationPath
    , toDerivationIndexes

    -- ** Low-level API
    , importAddress
    , ErrImportAddress(..)
    , addPendingAddress
    , deriveRndStateAddress
    , findUnusedPath
    , unavailablePaths
    , defaultAccountIndex
    , withRNG

    -- ** Benchmarking
    , RndAnyState (..)
    , mkRndAnyState
    ) where
import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Byron.Codec.Cbor
    ( decodeAddressDerivationPath, decodeAddressPayload, deserialiseCbor )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationIndex (..)
    , DerivationType (..)
    , Index (..)
    , NetworkDiscriminant
    , Passphrase (..)
    , PaymentAddress (..)
    , liftIndex
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey (..), deriveAccountPrivateKey, deriveAddressPrivateKey )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery (..)
    , GenChange (..)
    , IsOurs (isOurs)
    , IsOwned (..)
    , KnownAddresses (..)
    , MaybeLight (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..), AddressState (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount )
import Control.Arrow
    ( second )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( join )
import Data.Digest.CRC32
    ( crc32 )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map
    ( Map )
import Data.Proxy
    ( Proxy (..) )
import Data.Set
    ( Set )
import Data.Word
    ( Word32 )
import Fmt
    ( Buildable (..), blockMapF', indentF, tupleF )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( KnownNat, Nat, natVal )
import System.Random
    ( RandomGen, StdGen, mkStdGen, randomR )

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set

class RndStateLike s where
    -- Import an address into the state. This fails if the address does not belong
    -- to the wallet. Import an address that is already known is a no-op.
    importAddress
        :: Address
        -> s
        -> Either ErrImportAddress s

    -- Updates a 'RndState' by adding an address and its derivation path to the
    -- set of pending addresses. (i.e. it was a newly generated change address),
    -- then it is removed from there.
    addPendingAddress
        :: Address
        -> DerivationPath
        -> s
        -> s

    -- | Returns the set of derivation paths that should not be used for new address
    -- generation because they are already in use.
    unavailablePaths
        :: s
        -> Set DerivationPath

    -- | Default account used for generating new derivation paths.
    defaultAccountIndex
        :: s
        -> Index 'Hardened 'AccountK

    -- | Default random number generator.
    withRNG
        :: s
        -> (StdGen -> (a, StdGen))
        -> (a, s)

-- | HD random address discovery state and key material for AD.
data RndState (network :: NetworkDiscriminant) = RndState
    { hdPassphrase :: Passphrase "addr-derivation-payload"
    -- ^ The HD derivation passphrase
    , accountIndex :: Index 'Hardened 'AccountK
    -- ^ The account index used for address _generation_ in this wallet. Note
    -- that addresses will be _discovered_ from any and all account indices,
    -- regardless of this value.
    , discoveredAddresses :: Map DerivationPath (Address, AddressState)
    -- ^ The addresses which have so far been discovered, and their
    -- derivation paths.
    , pendingAddresses :: Map DerivationPath Address
    -- ^ The addresses which have been generated for use as change
    -- addresses, and their derivation paths. Once addresses have been
    -- discovered they are removed from this set and added to 'addresses'.
    , gen :: StdGen
    -- ^ The state of the RNG.
    } deriving (Generic, Eq)

instance NFData (RndState network) where
    rnf (RndState !_ !_ !_ !_ g) = seq (show g) ()

-- | There's no instance of 'Show' for 'XPrv'
instance Show (RndState network) where
    show (RndState _key ix addrs pending g) = unwords
        [ "RndState <xprv>", p ix, p addrs, p pending, p g ]
      where
        p x = "(" ++ show x ++ ")"

instance Buildable (RndState network) where
    build (RndState _ ix addrs pending g) = "RndState:\n"
        <> indentF 4 ("Account ix:       " <> build ix)
        <> indentF 4 ("Random Generator: " <> build (show g))
        <> indentF 4 ("Known addresses:  " <> blockMapF' tupleF tupleF addrs)
        <> indentF 4 ("Change addresses: " <> blockMapF' tupleF build pending)

-- | Shortcut type alias for HD random address derivation path.
type DerivationPath = (Index 'WholeDomain 'AccountK, Index 'WholeDomain 'AddressK)

instance RndStateLike (RndState n) where
    importAddress addr s = do
        case addressToPath addr (hdPassphrase s) of
            Nothing ->
                Left (ErrAddrDoesNotBelong addr)
            Just path | Map.member path (discoveredAddresses s) ->
                Right s
            Just path | Map.member path (pendingAddresses s) ->
                Right s
            Just path ->
                Right (addPendingAddress addr path s)

    addPendingAddress addr path st = st
        { pendingAddresses = Map.insert path addr (pendingAddresses st)
        }

    unavailablePaths st =
        Map.keysSet (discoveredAddresses st) <> Map.keysSet (pendingAddresses st)

    defaultAccountIndex =
        accountIndex

    withRNG s action =
        let (result, gen') = action (gen s) in (result, s { gen = gen' })

-- An address is considered to belong to the 'RndState' wallet if it can be
-- decoded as a Byron HD random address, and where the wallet key can be used
-- to decrypt the address derivation path.
instance IsOurs (RndState n) Address where
    isOurs addr st =
        ( toDerivationIndexes <$> path
        , maybe id (addDiscoveredAddress addr Used) path st
        )
      where
        path = addressToPath addr (hdPassphrase st)

instance IsOurs (RndState n) RewardAccount where
    isOurs _account state = (Nothing, state)

instance IsOwned (RndState n) ByronKey where
    isOwned st (key, pwd) addr =
        (, pwd) . deriveAddressKeyFromPath key pwd
            <$> addressToPath addr (hdPassphrase st)

-- Updates a 'RndState' by adding an address and its derivation path to the
-- set of discovered addresses. If the address was in the 'pendingAddresses' set
-- (i.e. it was a newly generated change address), then it is removed from
-- there.
addDiscoveredAddress
    :: Address
    -> AddressState
    -> DerivationPath
    -> RndState n
    -> RndState n
addDiscoveredAddress addr status path st = st
    { discoveredAddresses = Map.insert path (addr, status) (discoveredAddresses st)
    , pendingAddresses = Map.delete path (pendingAddresses st)
    }

addressToPath
    :: Address
    -> Passphrase "addr-derivation-payload"
    -> Maybe DerivationPath
addressToPath (Address addr) pwd = do
    payload <- deserialiseCbor decodeAddressPayload addr
    join $ deserialiseCbor (decodeAddressDerivationPath pwd) payload

toDerivationIndexes :: DerivationPath -> NonEmpty DerivationIndex
toDerivationIndexes (acctIx, addrIx) = NE.fromList
    [ DerivationIndex $ getIndex acctIx
    , DerivationIndex $ getIndex addrIx
    ]

-- | Initialize the HD random address discovery state from a root key and RNG
-- seed.
mkRndState :: ByronKey 'RootK XPrv -> Int -> RndState n
mkRndState key seed = RndState
    { hdPassphrase = payloadPassphrase key
    , accountIndex = minBound
    , discoveredAddresses = mempty
    , pendingAddresses = mempty
    , gen = mkStdGen seed
    }

newtype ErrImportAddress
    = ErrAddrDoesNotBelong Address
    deriving (Generic, Eq, Show)

instance PaymentAddress n ByronKey => GenChange (RndState n) where
    type ArgGenChange (RndState n) = (ByronKey 'RootK XPrv, Passphrase "encryption")
    genChange (rootXPrv, pwd) st = (address, st')
      where
        address = deriveRndStateAddress @n rootXPrv pwd path
        (path, gen') = findUnusedPath (gen st) (accountIndex st)
            (unavailablePaths st)
        st' = st
            { pendingAddresses = Map.insert path address (pendingAddresses st)
            , gen = gen'
            }

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
    :: forall n. (PaymentAddress n ByronKey)
    => ByronKey 'RootK XPrv
    -> Passphrase "encryption"
    -> DerivationPath
    -> Address
deriveRndStateAddress k passphrase path =
    paymentAddress @n $ publicKey $ deriveAddressKeyFromPath k passphrase path

-- Unlike sequential derivation, we can't derive an order from the index only
-- (they are randomly generated), nor anything else in the address itself.
--
-- Therefore, we'll simply consider that addresses using the random address
-- derivation scheme won't be ordered in any particular order.
instance CompareDiscovery (RndState n) where
    compareDiscovery _ _ _ = EQ


instance KnownAddresses (RndState n) where
    knownAddresses s = mconcat
        [ toListWithPath (\path (addr, state) -> (addr, state, path))
            (discoveredAddresses s)
        , toListWithPath (\path addr -> (addr, Unused, path))
            (pendingAddresses s)
        ]
      where
        toListWithPath
            :: (NonEmpty DerivationIndex -> v -> result)
            -> Map DerivationPath v
            -> [result]
        toListWithPath mk =
            Map.foldrWithKey
                (\path v result -> mk (toDerivationIndexes path) v : result)
                []

instance MaybeLight (RndState n) where
    maybeDiscover = Nothing

--------------------------------------------------------------------------------
--
-- RndAnyState
--
-- For benchmarking and testing arbitrary large random wallets.

-- | An "unsound" alternative that can be used for benchmarking and stress
-- testing. It re-uses the same underlying structure as the `RndState` but
-- it discover addresses based on an arbitrary ratio instead of decrypting the
-- derivation path.
--
-- The proportion is stored as a type-level parameter so that we don't have to
-- alter the database schema to store it. It simply exists and depends on the
-- caller creating the wallet to define it.
newtype RndAnyState (network :: NetworkDiscriminant) (p :: Nat) = RndAnyState
    { innerState :: RndState network
    } deriving (Generic, Show)

instance NFData (RndAnyState n p)

-- | Initialize the HD random address discovery state from a root key and RNG
-- seed.
--
-- The type parameter is expected to be a ratio of addresses we ought to simply
-- recognize as ours. It is expressed in per-myriad, so "1" means 0.01%,
-- "100" means 1% and 10000 means 100%.
mkRndAnyState
    :: forall (p :: Nat) n. ()
    => ByronKey 'RootK XPrv
    -> Int
    -> RndAnyState n p
mkRndAnyState key seed = RndAnyState
    { innerState = RndState
        { hdPassphrase = payloadPassphrase key
        , accountIndex = minBound
        , discoveredAddresses = mempty
        , pendingAddresses = mempty
        , gen = mkStdGen seed
        }
    }

instance RndStateLike (RndAnyState n p) where
    importAddress addr (RndAnyState inner) =
        RndAnyState <$> importAddress addr inner

    addPendingAddress addr path (RndAnyState inner) =
        RndAnyState $ addPendingAddress addr path inner

    unavailablePaths (RndAnyState inner) =
        unavailablePaths inner

    defaultAccountIndex (RndAnyState inner) =
        defaultAccountIndex inner

    withRNG (RndAnyState inner) action =
        second RndAnyState $ withRNG inner action

instance KnownNat p => IsOurs (RndAnyState n p) Address where
    isOurs addr@(Address bytes) st@(RndAnyState inner) =
        case isOurs addr inner of
            (Just path, inner') ->
                (Just path, RndAnyState inner')

            (Nothing, _) | crc32 bytes < p ->
                let
                    (path, gen') = findUnusedPath
                        (gen inner) (accountIndex inner) (unavailablePaths inner)

                    inner' = addDiscoveredAddress
                        addr Used path (inner { gen = gen' })
                in
                (Just (toDerivationIndexes path), RndAnyState inner')

            (Nothing, _) ->
                (Nothing, st)
      where
        p = floor (double (maxBound :: Word32) * double (natVal (Proxy @p)) / 10000)

        double :: Integral a => a -> Double
        double = fromIntegral

instance IsOurs (RndAnyState n p) RewardAccount where
    isOurs _account state = (Nothing, state)

instance KnownNat p => IsOwned (RndAnyState n p) ByronKey where
    isOwned _ _ _ = Nothing

instance PaymentAddress n ByronKey => GenChange (RndAnyState n p) where
    type ArgGenChange (RndAnyState n p) = ArgGenChange (RndState n)
    genChange a (RndAnyState s) = RndAnyState <$> genChange a s

instance CompareDiscovery (RndAnyState n p) where
    compareDiscovery (RndAnyState s) = compareDiscovery s

instance KnownAddresses (RndAnyState n p) where
    knownAddresses (RndAnyState s) = knownAddresses s

instance MaybeLight (RndAnyState n p) where
    maybeDiscover = Nothing
