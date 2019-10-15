{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Implementation of address derivation for the random scheme, as
-- implemented by the legacy Cardano wallets.
--
-- For full documentation of the key derivation schemes,
-- see the "Cardano.Crypto.Wallet" module, and the implementation in
-- <https://github.com/input-output-hk/cardano-crypto/blob/4590efa638397e952a51a8994b5543e4ea3c1ecd/cbits/encrypted_sign.c cardano-crypto>.

module Cardano.Wallet.Primitive.AddressDerivation.Random
    ( -- * Types
      RndKey(..)

      -- * Generation
    , unsafeGenerateKeyFromSeed
    , generateKeyFromSeed
    , minSeedLengthBytes
    , nullKey

      -- * Derivation
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( DerivationScheme (DerivationScheme1)
    , XPrv
    , XPub
    , deriveXPrv
    , generate
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
    , Passphrase (..)
    , PersistKey (..)
    , WalletKey (..)
    )
import Cardano.Wallet.Primitive.Types
    ( Hash (..), invariant )
import Control.DeepSeq
    ( NFData )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_256, SHA512 (..) )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import GHC.Generics
    ( Generic )

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Crypto.KDF.PBKDF2 as PBKDF2
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as B8

{-------------------------------------------------------------------------------
                                   Key Types
-------------------------------------------------------------------------------}

-- | Material for deriving HD random scheme keys, which can be used for making
-- addresses.
data RndKey (depth :: Depth) key = RndKey
    { getKey :: key
    -- ^ The raw private or public key.
    , derivationPath :: DerivationPath depth
    -- ^ The address derivation indices for the level of this key.
    , payloadPassphrase :: Passphrase "addr-derivation-payload"
    -- ^ Used for encryption of payload containing address derivation path.
    } deriving stock (Generic)

instance (NFData key, NFData (DerivationPath depth)) => NFData (RndKey depth key)
deriving instance (Show key, Show (DerivationPath depth)) => Show (RndKey depth key)
deriving instance (Eq key, Eq (DerivationPath depth)) => Eq (RndKey depth key)

-- | The hierarchical derivation indices for a given level/depth.
type family DerivationPath (depth :: Depth) :: * where
    -- The root key is generated from the seed.
    DerivationPath 'RootK =
        ()
    -- The account key is generated from the root key and account index.
    DerivationPath 'AccountK =
        Index 'Hardened 'AccountK
    -- The address key is generated from the account key and address index.
    DerivationPath 'AddressK =
        (Index 'Hardened 'AccountK, Index 'Hardened 'AddressK)

instance WalletKey RndKey where
    changePassphrase = changePassphraseRnd
    -- Extract the public key part of a private key.
    publicKey = mapKey toXPub
    -- Hash a public key to some other representation.
    digest = hash . unXPub . getKey
    getRawKey = getKey
    dummyKey = dummyKeyRnd
    keyTypeDescriptor _ = "rnd"

{-------------------------------------------------------------------------------
                                 Key generation
-------------------------------------------------------------------------------}

-- | The amount of entropy carried by a BIP-39 12-word mnemonic is 16 bytes.
minSeedLengthBytes :: Int
minSeedLengthBytes = 16

-- | Generate a root key from a corresponding seed.
-- The seed should be at least 16 bytes.
generateKeyFromSeed
    :: Passphrase "seed"
    -> Passphrase "encryption"
    -> RndKey 'RootK XPrv
generateKeyFromSeed = unsafeGenerateKeyFromSeed ()

-- | Generate a new key from seed. Note that the @depth@ is left open so that
-- the caller gets to decide what type of key this is. This is mostly for
-- testing, in practice, seeds are used to represent root keys, and one should
-- use 'generateKeyFromSeed'.
unsafeGenerateKeyFromSeed
    :: DerivationPath depth
    -> Passphrase "seed"
    -> Passphrase "encryption"
    -> RndKey depth XPrv
unsafeGenerateKeyFromSeed derivationPath (Passphrase seed) (Passphrase pwd) = RndKey
    { getKey = masterKey
    , derivationPath
    , payloadPassphrase = hdPassphrase (toXPub masterKey)
    }
  where
    masterKey = generate (hashSeed seed') pwd
    seed' = invariant
        ("seed length : " <> show (BA.length seed)
            <> " in (Passphrase \"seed\") is not valid")
        seed
        (\s -> BA.length s >= minSeedLengthBytes && BA.length s <= 255)

-- | Hash the seed entropy (generated from mnemonic) used to initiate a HD
-- wallet. This increases the key length to 34 bytes, selectKey is greater than the
-- minimum for 'generate' (32 bytes).
--
-- Note that our current implementation deviates from BIP-39 because we use a
-- hash function (Blake2b) rather than key stretching with PBKDF2.
--
-- There are two methods of hashing the seed entropy, for different use cases.
--
-- 1. Normal random derivation wallet seeds. The seed entropy is hashed using
--    Blake2b_256, inside a double CBOR serialization sandwich.
--
-- 2. Seeds for redeeming paper wallets. The seed entropy is hashed using
--    Blake2b_256, without any serialization.
hashSeed :: ScrubbedBytes -> ScrubbedBytes
hashSeed = serialize . blake2b256 . serialize
  where
    serialize = BA.convert . cbor . BA.convert
    cbor = CBOR.toStrictByteString . CBOR.encodeBytes

blake2b256 :: ScrubbedBytes -> ScrubbedBytes
blake2b256 = BA.convert . hash @ScrubbedBytes @Blake2b_256

-- | Derive a symmetric key for encrypting and authenticating the address
-- derivation path.
hdPassphrase :: XPub -> Passphrase "addr-derivation-payload"
hdPassphrase masterKey = Passphrase $
    PBKDF2.generate
    (PBKDF2.prfHMAC SHA512)
    (PBKDF2.Parameters 500 32)
    (unXPub masterKey)
    ("address-hashing" :: ByteString)

dummyKeyRnd :: RndKey 'AddressK XPub
dummyKeyRnd = RndKey key (minBound, minBound) pwd
  where
    Right key = xpub (B8.replicate 64 '\0')
    -- The 'hdPassphrase' result is 256 bits
    pwd = Passphrase (BA.convert $ B8.replicate 32 '\0')

{-------------------------------------------------------------------------------
                                   Passphrase
-------------------------------------------------------------------------------}

-- | Re-encrypt the private key using a different passphrase, and regenerate
-- the payload passphrase.
--
-- **Important**:
-- This function doesn't check that the old passphrase is correct! Caller is
-- expected to have already checked that. Using an incorrect passphrase here
-- will lead to very bad thing.
changePassphraseRnd
    :: Passphrase "encryption-old"
    -> Passphrase "encryption-new"
    -> RndKey depth XPrv
    -> RndKey depth XPrv
changePassphraseRnd (Passphrase oldPwd) (Passphrase newPwd) key = RndKey
    { getKey = masterKey
    , derivationPath = derivationPath key
    , payloadPassphrase = hdPassphrase (toXPub masterKey)
    }
  where
    masterKey = xPrvChangePass oldPwd newPwd (getKey key)

{-------------------------------------------------------------------------------
                                 HD derivation
-------------------------------------------------------------------------------}

-- | Derives account private key from the given root private key, using
-- derivation scheme 1.
--
-- NOTE: The caller is expected to provide the corresponding passphrase (and to
-- have checked that the passphrase is valid). Providing a wrong passphrase will
-- not make the function fail but will instead, yield an incorrect new key that
-- doesn't belong to the wallet.
deriveAccountPrivateKey
    :: Passphrase "encryption"
    -> RndKey 'RootK XPrv
    -> Index 'Hardened 'AccountK
    -> RndKey 'AccountK XPrv
deriveAccountPrivateKey (Passphrase pwd) masterKey idx@(Index accIx) = RndKey
    { getKey = deriveXPrv DerivationScheme1 pwd (getKey masterKey) accIx
    , derivationPath = idx
    , payloadPassphrase = payloadPassphrase masterKey
    }

-- | Derives address private key from the given account private key, using
-- derivation scheme 1.
--
-- NOTE: The caller is expected to provide the corresponding passphrase (and to
-- have checked that the passphrase is valid). Providing a wrong passphrase will
-- not make the function fail but will instead, yield an incorrect new key that
-- doesn't belong to the wallet.
deriveAddressPrivateKey
    :: Passphrase "encryption"
    -> RndKey 'AccountK XPrv
    -> Index 'Hardened 'AddressK
    -> RndKey 'AddressK XPrv
deriveAddressPrivateKey (Passphrase pwd) accountKey idx@(Index addrIx) = RndKey
    { getKey = deriveXPrv DerivationScheme1 pwd (getKey accountKey) addrIx
    , derivationPath = (derivationPath accountKey, idx)
    , payloadPassphrase = payloadPassphrase accountKey
    }

{-------------------------------------------------------------------------------
                          Storing and retrieving keys
-------------------------------------------------------------------------------}

instance PersistKey RndKey where
    serializeXPrv = serializeXPrvRnd
    deserializeXPrv = deserializeXPrvRnd

-- | Serialize the key with its payload encryption passphrase.
serializeKey :: (ByteString, Passphrase "addr-derivation-payload") -> ByteString
serializeKey (k, Passphrase p) =
    convertToBase Base16 k <> ":" <> convertToBase Base16 p

-- | Deserialize the key and its payload encryption passphrase.
deserializeKey
    :: (ByteString -> Either String key)
    -> ByteString
    -> Either String (key, Passphrase "addr-derivation-payload")
deserializeKey f b = case map (convertFromBase Base16) (B8.split ':' b) of
    [Right k, Right p] -> case f k of
        Right k' -> Right (k', Passphrase (BA.convert p))
        Left e -> Left e
    _ -> Left "Key input must be two hex strings separated by :"

serializeXPrvRnd
    :: (RndKey 'RootK XPrv, Hash "encryption")
    -> (ByteString, ByteString)
serializeXPrvRnd (RndKey k _ p, h) =
    ( serializeKey (unXPrv k, p)
    , convertToBase Base16 . getHash $ h )

-- | The reverse of 'serializeXPrv'. This may fail if the inputs are not valid
-- hexadecimal strings, or if the key is of the wrong length.
deserializeXPrvRnd
    :: (ByteString, ByteString)
       -- ^ Hexadecimal encoded private key and password hash
    -> Either String (RndKey 'RootK XPrv, Hash "encryption")
deserializeXPrvRnd (k, h) = (,)
    <$> fmap mkKey (rootKeyFromText k)
    <*> fmap Hash (convertFromBase Base16 h)
  where
    rootKeyFromText = deserializeKey xprv
    mkKey (key, pwd) = RndKey key () pwd

-- | A root key of all zeroes that is used when restoring 'RndState' from the
-- database before a root key has been saved.
nullKey :: RndKey 'RootK XPrv
nullKey = RndKey k () pwd
  where
    Right k = xprv $ B8.replicate 128 '\0'
    pwd = Passphrase (BA.convert $ B8.replicate 32 '\0')

{-------------------------------------------------------------------------------
                                     Utils
-------------------------------------------------------------------------------}

-- | Transform the wrapped key.
mapKey :: (key -> key') -> RndKey depth key -> RndKey depth key'
mapKey f rnd = rnd { getKey = f (getKey rnd) }
