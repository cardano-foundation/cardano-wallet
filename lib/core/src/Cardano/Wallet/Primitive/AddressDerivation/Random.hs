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
-- License: MIT
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

      -- * Derivation
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( DerivationScheme (DerivationScheme1)
    , XPrv
    , deriveXPrv
    , generate
    , toXPub
    , unXPub
    , xPrvChangePass
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , Index (..)
    , Passphrase (..)
    , PersistKey (..)
    , WalletKey (..)
    , deserializeXPrv
    , serializeXPrv
    )
import Cardano.Wallet.Primitive.Types
    ( invariant )
import Control.DeepSeq
    ( NFData )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_256, SHA512 (..) )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.ByteString
    ( ByteString )
import GHC.Generics
    ( Generic )

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Crypto.KDF.PBKDF2 as PBKDF2
import qualified Data.ByteArray as BA

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

instance PersistKey RndKey where
    serializeXPrv = error "PersistKey RndKey unimplemented"
    deserializeXPrv = error "PersistKey RndKey unimplemented"
    serializeXPub = error "PersistKey RndKey unimplemented"
    deserializeXPub = error "PersistKey RndKey unimplemented"

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
    , payloadPassphrase = hdPassphrase masterKey
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
hdPassphrase
    :: XPrv
    -> Passphrase "addr-derivation-payload"
hdPassphrase masterKey = Passphrase $
    PBKDF2.generate
    (PBKDF2.prfHMAC SHA512)
    (PBKDF2.Parameters 500 32)
    (unXPub . toXPub $ masterKey)
    ("address-hashing" :: ByteString)

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
    , payloadPassphrase = hdPassphrase masterKey
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
                                     Utils
-------------------------------------------------------------------------------}

-- | Transform the wrapped key.
mapKey :: (key -> key') -> RndKey depth key -> RndKey depth key'
mapKey f rnd = rnd { getKey = f (getKey rnd) }
