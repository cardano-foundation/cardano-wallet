{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

module Cardano.Wallet.Primitive.AddressDerivation.Random
    ( deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , unsafeGenerateKeyFromSeed
    , generateKeyFromSeed
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( DerivationScheme (DerivationScheme1), XPrv, deriveXPrv, generateNew )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DerivationType (..), Index (..), Passphrase (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Common
    ( Depth (..), Key (..) )
import Cardano.Wallet.Primitive.Types
    ( invariant )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_256 )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.ByteString
    ( ByteString )

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Lazy as BL

-- | Derives account private key from the given root private key, using
-- derivation scheme 1 (see the
-- <https://github.com/input-output-hk/cardano-crypto cardano-crypto>
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
deriveAccountPrivateKey (Passphrase pwd) (Key masterXPrv) (Index accIx) =
    Key $ deriveXPrv DerivationScheme1 pwd masterXPrv accIx

-- | Derives address private key from the given account private key, using
-- derivation scheme 1 (see the
-- <https://github.com/input-output-hk/cardano-crypto/ cardano-crypto>
-- package for more details).
--
-- NOTE: The caller is expected to provide the corresponding passphrase (and to
-- have checked that the passphrase is valid). Providing a wrong passphrase will
-- not make the function fail but will instead, yield an incorrect new key that
-- doesn't belong to the wallet.
deriveAddressPrivateKey
    :: Passphrase "encryption"
    -> Key 'AccountK XPrv
    -> Index 'Soft 'AddressK
    -> Key 'AddressK XPrv
deriveAddressPrivateKey (Passphrase pwd) (Key accXPrv) (Index addrIx) =
    Key $ deriveXPrv DerivationScheme1 pwd accXPrv addrIx

-- | Generate a root key from a corresponding seed.
-- The seed should be at least 32 bytes
generateKeyFromSeed
    :: Passphrase "seed"
    -> Passphrase "encryption"
    -> Key 'RootK XPrv
generateKeyFromSeed = unsafeGenerateKeyFromSeed

-- | See 'generateKeyFromSeed'.
unsafeGenerateKeyFromSeed
    :: Passphrase "seed"
    -> Passphrase "encryption"
    -> Key depth XPrv
unsafeGenerateKeyFromSeed (Passphrase seed) (Passphrase pwd) =
    let
        seed' = invariant
            ("seed length : " <> show (BA.length seed) <> " in (Passphrase \"seed\") is not valid")
            seed
            (\s -> BA.length s >= 32 && BA.length s <= 255)
        genPwd = mempty :: ByteString
    in Key $ generateNew (hashSeed seed') genPwd pwd

-- | Hash the seed entropy (generated from mnemonic) used to initiate a HD
-- wallet.
--
-- Note that our current implementation deviates from BIP-39 because we use a
-- fast blake2b hashing function rather than a slow PBKDF2.
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
    cbor = BL.toStrict . CBOR.toLazyByteString . CBOR.encodeBytes

-- hashSeedForPaperWallet :: ScrubbedBytes -> ScrubbedBytes
-- hashSeedForPaperWallet = blake2b256

blake2b256 :: ScrubbedBytes -> ScrubbedBytes
blake2b256 = BA.convert . hash @ScrubbedBytes @Blake2b_256
