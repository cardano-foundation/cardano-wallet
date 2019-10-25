{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Implementation of address derivation for the sequential schemes, as
-- implemented by Yoroi/Icarus and cardano-cli.

module Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( -- * SeqKey types
      SeqKey(..)
    , ChangeChain(..)
    -- * SeqKey generation and derivation
    , generateKeyFromSeed
    , unsafeGenerateKeyFromSeed
    , minSeedLengthBytes
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , deriveAddressPublicKey
    -- * Passphrase
    , changePassphraseSeq
    -- * Storing and retrieving keys
    , deserializeXPubSeq
    , serializeXPubSeq
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
    , unXPrv
    , unXPub
    , xPrvChangePass
    , xprv
    , xpub
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , Index (..)
    , InspectAddress (..)
    , Passphrase (..)
    , PersistKey (..)
    , WalletKey (..)
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..), Hash (..), invariant )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( (<=<) )
import Crypto.Hash
    ( Digest, HashAlgorithm, hash )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Maybe
    ( fromMaybe )
import Data.Text.Class
    ( CaseStyle (..)
    , FromText (..)
    , ToText (..)
    , fromTextToBoundedEnum
    , toTextFromBoundedEnum
    )
import Data.Typeable
    ( Typeable )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

{-------------------------------------------------------------------------------
                            Sequential Derivation
-------------------------------------------------------------------------------}


-- | A cryptographic key for sequential-scheme address derivation, with
-- phantom-types to disambiguate key types.
--
-- @
-- let rootPrivateKey = SeqKey 'RootK XPrv
-- let accountPubKey = SeqKey 'AccountK XPub
-- let addressPubKey = SeqKey 'AddressK XPub
-- @
newtype SeqKey (depth :: Depth) key = SeqKey { getKey :: key }
    deriving stock (Generic, Show, Eq)

instance (NFData key) => NFData (SeqKey depth key)

instance WalletKey SeqKey where
    changePassphrase = changePassphraseSeq
    publicKey = publicKeySeq
    digest = digestSeq
    getRawKey = getKey
    dummyKey = dummyKeySeq
    keyTypeDescriptor _ = "seq"

-- | Marker for the change chain. In practice, change of a transaction goes onto
-- the addresses generated on the internal chain, whereas the external chain is
-- used for addresses that are part of the 'advertised' targets of a transaction
data ChangeChain
    = ExternalChain
    | InternalChain
    deriving (Generic, Typeable, Show, Eq, Ord, Bounded)

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

instance ToText ChangeChain where
    toText = toTextFromBoundedEnum SnakeLowerCase

instance FromText ChangeChain where
    fromText = fromTextToBoundedEnum SnakeLowerCase

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

-- | The minimum seed length for 'generateKeyFromSeed' and
-- 'unsafeGenerateKeyFromSeed'.
minSeedLengthBytes :: Int
minSeedLengthBytes = 16

-- | Generate a root key from a corresponding seed.
-- The seed should be at least 16 bytes.
generateKeyFromSeed
    :: (Passphrase "seed", Passphrase "generation")
       -- ^ The actual seed and its recovery / generation passphrase
    -> Passphrase "encryption"
    -> SeqKey 'RootK XPrv
generateKeyFromSeed = unsafeGenerateKeyFromSeed

-- | Generate a new key from seed. Note that the @depth@ is left open so that
-- the caller gets to decide what type of key this is. This is mostly for
-- testing, in practice, seeds are used to represent root keys, and one should
-- use 'generateKeyFromSeed'.
unsafeGenerateKeyFromSeed
    :: (Passphrase "seed", Passphrase "generation")
        -- ^ The actual seed and its recovery / generation passphrase
    -> Passphrase "encryption"
    -> SeqKey depth XPrv
unsafeGenerateKeyFromSeed (Passphrase seed, Passphrase gen) (Passphrase pwd) =
    let
        seed' = invariant
            ("seed length : " <> show (BA.length seed) <> " in (Passphrase \"seed\") is not valid")
            seed
            (\s -> BA.length s >= minSeedLengthBytes && BA.length s <= 255)
    in SeqKey $ generateNew seed' gen pwd

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
    -> SeqKey 'RootK XPrv
    -> Index 'Hardened 'AccountK
    -> SeqKey 'AccountK XPrv
deriveAccountPrivateKey (Passphrase pwd) (SeqKey rootXPrv) (Index accIx) =
    let
        purposeXPrv = -- lvl1 derivation; hardened derivation of purpose'
            deriveXPrv DerivationScheme2 pwd rootXPrv purposeIndex
        coinTypeXPrv = -- lvl2 derivation; hardened derivation of coin_type'
            deriveXPrv DerivationScheme2 pwd purposeXPrv coinTypeIndex
        acctXPrv = -- lvl3 derivation; hardened derivation of account' index
            deriveXPrv DerivationScheme2 pwd coinTypeXPrv accIx
    in
        SeqKey acctXPrv

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
    -> SeqKey 'AccountK XPrv
    -> ChangeChain
    -> Index 'Soft 'AddressK
    -> SeqKey 'AddressK XPrv
deriveAddressPrivateKey
        (Passphrase pwd) (SeqKey accXPrv) changeChain (Index addrIx) =
    let
        changeCode =
            fromIntegral $ fromEnum changeChain
        changeXPrv = -- lvl4 derivation; soft derivation of change chain
            deriveXPrv DerivationScheme2 pwd accXPrv changeCode
        addrXPrv = -- lvl5 derivation; soft derivation of address index
            deriveXPrv DerivationScheme2 pwd changeXPrv addrIx
    in
        SeqKey addrXPrv

-- | Derives address public key from the given account public key, using
-- derivation scheme 2 (see <https://github.com/input-output-hk/cardano-crypto/ cardano-crypto>
-- package for more details).
--
-- This is the preferred way of deriving new sequential address public keys.
deriveAddressPublicKey
    :: SeqKey 'AccountK XPub
    -> ChangeChain
    -> Index 'Soft 'AddressK
    -> SeqKey 'AddressK XPub
deriveAddressPublicKey (SeqKey accXPub) changeChain (Index addrIx) =
    fromMaybe errWrongIndex $ do
        let changeCode = fromIntegral $ fromEnum changeChain
        changeXPub <- -- lvl4 derivation in bip44 is derivation of change chain
            deriveXPub DerivationScheme2 accXPub changeCode
        addrXPub <- -- lvl5 derivation in bip44 is derivation of address chain
            deriveXPub DerivationScheme2 changeXPub addrIx
        return $ SeqKey addrXPub
  where
    errWrongIndex = error $
        "Cardano.Wallet.Primitive.AddressDerivation.deriveAddressPublicKey \
        \failed: was given an hardened (or too big) index for soft path \
        \derivation ( " ++ show addrIx ++ "). This is either a programmer \
        \error, or, we may have reached the maximum number of addresses for \
        \a given wallet."


{-------------------------------------------------------------------------------
                                   Passphrase
-------------------------------------------------------------------------------}

-- | Re-encrypt a private key using a different passphrase.
--
-- **Important**:
-- This function doesn't check that the old passphrase is correct! Caller is
-- expected to have already checked that. Using an incorrect passphrase here
-- will lead to very bad thing.
changePassphraseSeq
    :: Passphrase "encryption-old"
    -> Passphrase "encryption-new"
    -> SeqKey purpose XPrv
    -> SeqKey purpose XPrv
changePassphraseSeq (Passphrase oldPwd) (Passphrase newPwd) (SeqKey prv) =
    SeqKey $ xPrvChangePass oldPwd newPwd prv

{-------------------------------------------------------------------------------
                            WalletKey implementation
-------------------------------------------------------------------------------}

-- | Extract the public key part of a private key.
publicKeySeq
    :: SeqKey depth XPrv
    -> SeqKey depth XPub
publicKeySeq (SeqKey k) =
    SeqKey (toXPub k)

-- | Hash a public key to some other representation.
digestSeq
    :: HashAlgorithm a
    => SeqKey depth XPub
    -> Digest a
digestSeq (SeqKey k) =
    hash (unXPub k)

dummyKeySeq :: SeqKey 'AddressK XPub
dummyKeySeq = SeqKey key
    where Right key = xpub (BS.replicate 64 0)

{-------------------------------------------------------------------------------
                          Storing and retrieving keys
-------------------------------------------------------------------------------}

instance PersistKey SeqKey where
    serializeXPrv = serializeXPrvSeq
    deserializeXPrv = deserializeXPrvSeq

-- | Convert a private key and its password hash into hexadecimal strings
-- suitable for storing in a text file or database column.
serializeXPrvSeq
    :: (SeqKey 'RootK XPrv, Hash "encryption")
    -> (ByteString, ByteString)
serializeXPrvSeq (k, h) =
    ( toHexText . unXPrv . getRawKey $ k
    , toHexText . getHash $ h )

-- | The reverse of 'serializeXPrvSeq'. This may fail if the inputs are not
-- valid hexadecimal strings, or if the key is of the wrong length.
deserializeXPrvSeq
    :: (ByteString, ByteString)
       -- ^ Hexadecimal encoded private key and password hash
    -> Either String (SeqKey purpose XPrv, Hash "encryption")
deserializeXPrvSeq (k, h) = (,)
    <$> fmap SeqKey (xprvFromText k)
    <*> fmap Hash (fromHexText h)
  where
    xprvFromText = xprv <=< fromHexText

-- FIXME
-- 'SeqKey' (as well as 'RndKey') was actually a wrong division for separating
-- HD derivations schemes.
-- Keys are actually the same entities, but the division operates at the address
-- level (Byron addresses have a different structure than Shelley ones).
-- This class here makes strong assumptions on the structure of the address and
-- addresses are the in the new Shelley format -- which is an okay-ish
-- assumption since we've been treating 'SeqKey' as key associated with Shelley
-- addresses so far. But this will change as soon as we decide to support Yoroi
-- legacy wallets.
instance InspectAddress SeqKey where
    type SpendingKey SeqKey = ByteString
    getSpendingKey (Address bytes)
        | let l = BS.length bytes in l == addrLenSingle || l == addrLenGrouped =
            BS.take (addrLenSingle - 1) $ BS.drop 1 bytes
        | otherwise =
            error "InspectAddress: tried to inspect an incompatible address"

    type DelegationKey SeqKey = ByteString
    getDelegationKey (Address bytes)
        | BS.length bytes == addrLenSingle =
            Nothing

        | BS.length bytes == addrLenGrouped =
            Just $ BS.drop addrLenSingle bytes

        | otherwise =
            error "InspectAddress: tried to inspect an incompatible address"

-- Serialized length in bytes of a Single Address
addrLenSingle :: Int
addrLenSingle = 33

-- Serialized length in bytes of a Grouped Address
addrLenGrouped :: Int
addrLenGrouped = 65

-- | Convert a public key into a hexadecimal string suitable for storing in a
-- text file or database column.
serializeXPubSeq :: SeqKey purpose XPub -> ByteString
serializeXPubSeq = toHexText . unXPub . getRawKey

-- | The reverse of 'serializeXPub'. This will fail if the input is not
-- hexadecimal string of the correct length.
deserializeXPubSeq :: ByteString -> Either String (SeqKey purpose XPub)
deserializeXPubSeq = fmap SeqKey . (xpub <=< fromHexText)

toHexText :: ByteString -> ByteString
toHexText = convertToBase Base16

fromHexText :: ByteString -> Either String ByteString
fromHexText = convertFromBase Base16

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
-- - 'publicKey' --> For any @SeqKey _ XPrv@ to @SeqKey _ XPub@
-- - 'deriveAccountPrivateKey' -->
--      From @SeqKey RootK XPrv@ to @SeqKey AccountK XPrv@
-- - 'deriveAddressPrivateKey' -->
--      From @SeqKey AccountK XPrv@ to @SeqKey AddressK XPrv@
-- - 'deriveAddressPublicKey' -->
--      From @SeqKey AccountK XPub@ to @SeqKey AddressK XPub@
--
-- For example:
--
-- @
-- -- Access public key for: m|coinType'|purpose'|0'
-- let seed = "My Secret Seed"
--
-- let rootXPrv :: SeqKey 'RootK XPrv
--     rootXPrv = generateKeyFromSeed (seed, mempty) mempty
--
-- let accIx :: Index 'Hardened 'AccountK
--     accIx = toEnum 0x80000000
--
-- let accXPub :: SeqKey 'AccountL XPub
--     accXPub = publicKey $ deriveAccountPrivateKey mempty rootXPrv accIx
-- @
