{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Primitives for performing address derivation for some given schemes. This is
-- where most of the crypto happens in the wallet and, it is quite important to
-- ensure that the following implementation matches with other wallet softwares
-- (like Yoroi/Icarus or the cardano-cli)

module Cardano.Wallet.Primitive.AddressDerivation
    (
    -- * Polymorphic / General Purpose Types
    -- $use
      Key (..)
    , Depth (..)
    , Index (..)
    , DerivationType (..)
    , Passphrase(..)
    , publicKey
    , XPub
    , XPrv

    -- * Sequential Derivation
    , ChangeChain(..)
    , generateKeyFromSeed
    , unsafeGenerateKeyFromSeed
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , deriveAddressPublicKey
    , keyToAddress
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( DerivationScheme (..)
    , XPrv
    , XPub
    , deriveXPrv
    , deriveXPub
    , generateNew
    , toXPub
    )
import Cardano.Wallet.Binary
    ( encodeAddress )
import Cardano.Wallet.Primitive.Types
    ( Address (..) )
import Control.DeepSeq
    ( NFData )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.Maybe
    ( fromMaybe )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( Symbol )

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR


{-------------------------------------------------------------------------------
                        Polymorphic / General Purpose Types
-------------------------------------------------------------------------------}

-- | A cryptographic key, with phantom-types to disambiguate key types.
--
-- @
-- let rootPrivateKey = Key 'RootK XPrv
-- let accountPubKey = Key 'AccountK XPub
-- let addressPubKey = Key 'AddressK XPub
-- @
newtype Key (level :: Depth) key = Key key
    deriving stock (Generic, Show, Eq)

instance (NFData key) => NFData (Key level key)


-- | Key Depth in the derivation path, according to BIP-0039 / BIP-0044
--
-- @m | purpose' | cointype' | account' | change | address@
--
-- We do not manipulate purpose, cointype and change paths directly, so they are
-- left out of the sum type.
data Depth = RootK | AccountK | AddressK

-- | A derivation index, with phantom-types to disambiguate derivation type.
--
-- @
-- let accountIx = Index 'Hardened 'AccountK
-- let addressIx = Index 'Soft 'AddressK
-- @
newtype Index (derivationType :: DerivationType) (level :: Depth) = Index
    { getIndex :: Word32 }
    deriving stock (Generic, Show, Eq, Ord)

instance NFData (Index derivationType level)

instance Bounded (Index 'Hardened level) where
    minBound = Index 0x80000000
    maxBound = Index maxBound

instance Bounded (Index 'Soft level) where
    minBound = Index minBound
    maxBound = let (Index ix) = minBound @(Index 'Hardened _) in Index (ix - 1)

instance Enum (Index 'Hardened level) where
    fromEnum (Index ix) = fromIntegral ix
    toEnum ix
        | Index (fromIntegral ix) < minBound @(Index 'Hardened _) =
            error "Index@Hardened.toEnum: bad argument"
        | otherwise =
            Index (fromIntegral ix)

instance Enum (Index 'Soft level) where
    fromEnum (Index ix) = fromIntegral ix
    toEnum ix
        | Index (fromIntegral ix) > maxBound @(Index 'Soft _) =
            error "Index@Soft.toEnum: bad argument"
        | otherwise =
            Index (fromIntegral ix)


-- | Type of derivation that should be used with the given indexes.
data DerivationType = Hardened | Soft

-- | An encapsulated passphrase. The inner format is free, but the wrapper helps
-- readability in function signatures.
newtype Passphrase (goal :: Symbol) = Passphrase ScrubbedBytes
    deriving stock (Eq, Show)
    deriving newtype (Semigroup, Monoid)

-- | Extract the public key part of a private key.
publicKey
    :: Key level XPrv
    -> Key level XPub
publicKey (Key xprv) =
    Key (toXPub xprv)


{-------------------------------------------------------------------------------
                            Sequential Derivation
-------------------------------------------------------------------------------}

-- | Marker for the change chain. In practice, change of a transaction goes onto
-- the addresses generated on the internal chain, whereas the external chain is
-- used for addresses that are part of the 'advertised' targets of a transaction
data ChangeChain
    = InternalChain
    | ExternalChain
    deriving (Generic, Show, Eq)

instance NFData ChangeChain

-- Not deriving 'Enum' because this could have a dramatic impact if we were
-- to assign the wrong index to the corresponding constructor (by swapping
-- around the constructor above for instance).
instance Enum ChangeChain where
    toEnum = \case
        0 -> ExternalChain
        1 -> InternalChain
        _ -> error "ChangeChain.toEnum: bad argument"
    fromEnum = \case
        ExternalChain -> 0
        InternalChain -> 1

-- | Purpose is a constant set to 44' (or 0x8000002C) following the BIP-44
-- recommendation. It indicates that the subtree of this node is used
-- according to this specification.
--
-- Hardened derivation is used at this level.
purposeIndex :: Word32
purposeIndex = 0x8000002C

-- | One master node (seed) can be used for unlimited number of independent
-- cryptocoins such as Bitcoin, Litecoin or Namecoin. However, sharing the
-- same space for various cryptocoins has some disadvantages.
--
-- This level creates a separate subtree for every cryptocoin, avoiding reusing
-- addresses across cryptocoins and improving privacy issues.
--
-- Coin type is a constant, set for each cryptocoin. For Cardano this constant
-- is set to 1815' (or 0x80000717). 1815 is the birthyear of our beloved Ada
-- Lovelace.
--
-- Hardened derivation is used at this level.
coinTypeIndex :: Word32
coinTypeIndex = 0x80000717

-- | Generate a new key from seed. Note that the @depth@ is left open so that
-- the caller gets to decide what type of key this is. This is mostly for
-- testing, in practice, seeds are used to represent root keys, and one should
-- use 'generateKeyFromSeed'.
unsafeGenerateKeyFromSeed
    :: (Passphrase "seed", Passphrase "generation")
        -- ^ The actual seed and its recovery / generation passphrase
    -> Passphrase "encryption"
    -> Key depth XPrv
unsafeGenerateKeyFromSeed (Passphrase seed, Passphrase gen) (Passphrase pwd) =
    Key $ generateNew seed gen pwd

-- | Generate a root key from a corresponding seed
generateKeyFromSeed
    :: (Passphrase "seed", Passphrase "generation")
        -- ^ The actual seed and its recovery / generation passphrase
    -> Passphrase "encryption"
    -> Key 'RootK XPrv
generateKeyFromSeed = unsafeGenerateKeyFromSeed

-- | Derives account private key from the given root private key, using
-- derivation scheme 2 (see <https://github.com/input-output-hk/cardano-crypto/ cardano-crypto>
-- package for more details).
--
-- NOTE: The caller is expected to provide the corresponding passphrase (and to
-- have checked that the passphrase is valid). Providing a wrong passphrase will
-- not make the function fail but will instead, yield an incorrect new key that
-- doesn't belong to the wallet.
deriveAccountPrivateKey
    :: Passphrase "encryption"
    -> Key 'RootK XPrv
    -> Index 'Hardened 'AccountK
    -> Key 'AccountK XPrv
deriveAccountPrivateKey (Passphrase pwd) (Key rootXPrv) (Index accIx) =
    let
        purposeXPrv = -- lvl1 derivation; hardened derivation of purpose'
            deriveXPrv DerivationScheme2 pwd rootXPrv purposeIndex
        coinTypeXPrv = -- lvl2 derivation; hardened derivation of coin_type'
            deriveXPrv DerivationScheme2 pwd purposeXPrv coinTypeIndex
        acctXPrv = -- lvl3 derivation; hardened derivation of account' index
            deriveXPrv DerivationScheme2 pwd coinTypeXPrv accIx
    in
        Key acctXPrv

-- | Derives address private key from the given account private key, using
-- derivation scheme 2 (see <https://github.com/input-output-hk/cardano-crypto/ cardano-crypto>
-- package for more details).
--
-- It is preferred to use 'deriveAddressPublicKey' whenever possible to avoid
-- having to manipulate passphrases and private keys.
--
-- NOTE: The caller is expected to provide the corresponding passphrase (and to
-- have checked that the passphrase is valid). Providing a wrong passphrase will
-- not make the function fail but will instead, yield an incorrect new key that
-- doesn't belong to the wallet.
deriveAddressPrivateKey
    :: Passphrase "encryption"
    -> Key 'AccountK XPrv
    -> ChangeChain
    -> Index 'Soft 'AddressK
    -> Key 'AddressK XPrv
deriveAddressPrivateKey
        (Passphrase pwd) (Key accXPrv) changeChain (Index addrIx) =
    let
        changeCode =
            fromIntegral $ fromEnum changeChain
        changeXPrv = -- lvl4 derivation; soft derivation of change chain
            deriveXPrv DerivationScheme2 pwd accXPrv changeCode
        addrXPrv = -- lvl5 derivation; soft derivation of address index
            deriveXPrv DerivationScheme2 pwd changeXPrv addrIx
    in
        Key addrXPrv

-- | Derives address public key from the given account public key, using
-- derivation scheme 2 (see <https://github.com/input-output-hk/cardano-crypto/ cardano-crypto>
-- package for more details).
--
-- This is the preferred way of deriving new sequential address public keys.
deriveAddressPublicKey
    :: Key 'AccountK XPub
    -> ChangeChain
    -> Index 'Soft 'AddressK
    -> Key 'AddressK XPub
deriveAddressPublicKey (Key accXPub) changeChain (Index addrIx) =
    fromMaybe errWrongIndex $ do
        let changeCode = fromIntegral $ fromEnum changeChain
        changeXPub <- -- lvl4 derivation in bip44 is derivation of change chain
            deriveXPub DerivationScheme2 accXPub changeCode
        addrXPub <- -- lvl5 derivation in bip44 is derivation of address chain
            deriveXPub DerivationScheme2 changeXPub addrIx
        return $ Key addrXPub
  where
    errWrongIndex = error $
        "Cardano.Wallet.Primitive.AddressDerivation.deriveAddressPublicKey \
        \failed: was given an hardened (or too big) index for soft path \
        \derivation ( " ++ show addrIx ++ "). This is either a programmer \
        \error, or, we may have reached the maximum number of addresses for \
        \a given wallet."

-- | Encode a public key to a (Byron / Legacy) Cardano 'Address'. This is mostly
-- dubious CBOR serializations with no data attributes.
keyToAddress
    :: Key 'AddressK XPub
    -> Address
keyToAddress (Key xpub) =
    Address $ CBOR.toStrictByteString $ encodeAddress xpub encodeAttributes
  where
    encodeAttributes = CBOR.encodeMapLen 0

-- $use
-- 'Key' and 'Index' allow for representing public keys, private keys, hardened
-- indexes and soft (non-hardened) indexes for various level in a non-ambiguous
-- manner. Those types are opaque and can only be created through dedicated
-- constructors.
--
-- Indexes can be created through their `Bounded` and `Enum` instances. Note
-- that, the `Enum` functions are partial and its the caller responsibility to
-- make sure it is safe to call them. For instance, calling @succ maxBound@ for
-- any index would through a runtime error. Similarly, trying to convert an
-- invalid value from an 'Int' to a an 'Index' will result in bad behavior.
--
-- >>> toEnum 0x00000000 :: Index 'Soft 'AddressK
-- Index {getIndex = 0}
--
-- >>> toEnum 0x00000000 :: Index 'Hardened 'AccountK
-- Index {getIndex = *** Exception: Index@Hardened.toEnum: bad argument
--
-- >>> toEnum 0x80000000 :: Index 'Hardened 'AccountK
-- Index {getIndex = 2147483648}
--
-- >>> toEnum 0x80000000 :: Index 'Soft 'AddressK
-- Index {getIndex = *** Exception: Index@Soft.toEnum: bad argument
--
-- Keys have to be created from a seed using 'generateKeyFromSeed' which always
-- gives a root private key. Then, child keys can be created safely using the
-- various key derivation methods:
--
-- - 'publicKey' --> For any @Key _ XPrv@ to @Key _ XPub@
-- - 'deriveAccountPrivateKey' -->
--      From @Key RootK XPrv@ to @Key AccountK XPrv@
-- - 'deriveAddressPrivateKey' -->
--      From @Key AccountK XPrv@ to @Key AddressK XPrv@
-- - 'deriveAddressPublicKey' -->
--      From @Key AccountK XPub@ to @Key AddressK XPub@
--
-- For example:
--
-- @
-- -- Access public key for: m|coinType'|purpose'|0'
-- let seed = "My Secret Seed"
--
-- let rootXPrv :: Key 'RootK XPrv
--     rootXPrv = generateKeyFromSeed (seed, mempty) mempty
--
-- let accIx :: Index 'Hardened 'AccountK
--     accIx = toEnum 0x80000000
--
-- let accXPub :: Key 'AccountL XPub
--     accXPub = publicKey $ deriveAccountPrivateKey mempty rootXPrv accIx
-- @
