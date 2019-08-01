{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
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
--
-- For full documentation of the key derivation schemes,
-- see the "Cardano.Crypto.Wallet" module, and the implementation in
-- <https://github.com/input-output-hk/cardano-crypto/blob/4590efa638397e952a51a8994b5543e4ea3c1ecd/cbits/encrypted_sign.c cardano-crypto>.

module Cardano.Wallet.Primitive.AddressDerivation.Random
    ( -- * Key derivation and generation
      deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , generateKeyFromSeed
    , unsafeGenerateKeyFromSeed
    , minSeedLengthBytes
    -- * Address encoding/decoding
    , encodeDerivationPath
    , decodeDerivationPath
    , decodeAddressDerivationPath
    , addrToPayload
    , deserialise
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( DerivationScheme (DerivationScheme1)
    , XPrv
    , XPub (..)
    , deriveXPrv
    , generate
    , unXPub
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DerivationType (..), Index (..), Passphrase (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Common
    ( Depth (..), Key (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..), invariant )
import Cardano.Wallet.Unsafe
    ( unsafeDeserialiseFromBytes )
import Control.Monad
    ( replicateM, when )
import Crypto.Error
    ( CryptoError (..), CryptoFailable (..) )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_256, SHA512 (..) )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58 )
import Data.Maybe
    ( fromJust )
import Data.Word
    ( Word8 )

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Crypto.Cipher.ChaChaPoly1305 as Poly
import qualified Crypto.KDF.PBKDF2 as PBKDF2
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

-- | Derives account private key from the given root private key, using
-- derivation scheme 1.
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
-- derivation scheme 1.
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
-- The seed should be at least 16 ('minSeedLengthBytes') bytes.
generateKeyFromSeed
    :: Passphrase "seed"
    -> Passphrase "encryption"
    -> Key 'RootK XPrv
generateKeyFromSeed = unsafeGenerateKeyFromSeed

-- | Generate a new key from seed. Note that the @depth@ is left open so that
-- the caller gets to decide what type of key this is. This is mostly for
-- testing, in practice, seeds are used to represent root keys, and one should
-- use 'generateKeyFromSeed'.
unsafeGenerateKeyFromSeed
    :: Passphrase "seed"
    -> Passphrase "encryption"
    -> Key depth XPrv
unsafeGenerateKeyFromSeed (Passphrase seed) (Passphrase pwd) =
    Key $ generate (hashSeed seed') pwd
  where
    seed' = invariant
        ("seed length : " <> show (BA.length seed)
            <> " in (Passphrase \"seed\") is not valid")
        seed
        (\s -> BA.length s >= minSeedLengthBytes && BA.length s <= 255)

-- | The amount of entropy carried by a BIP-39 12-word mnemonic is 16 bytes.
minSeedLengthBytes :: Int
minSeedLengthBytes = 16

-- | Hash the seed entropy (generated from mnemonic) used to initiate a HD
-- wallet. This increases the key length to 34 bytes, which is greater than the
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

-- hashSeedForPaperWallet :: ScrubbedBytes -> ScrubbedBytes
-- hashSeedForPaperWallet = blake2b256

blake2b256 :: ScrubbedBytes -> ScrubbedBytes
blake2b256 = BA.convert . hash @ScrubbedBytes @Blake2b_256

{-------------------------------------------------------------------------------
                         Derivation path serialization

In the composition of a Cardano address, the following functions concern the
"Derivation Path" box.

+--------------------------------------------------------------------------------+
|              Base-58 Encoded CBOR-Serialized Object with CRC*                  |
|                                                                                |
|      DdzFFzCqrhstiVdBdYEAmpLPtSWxFYy...rYcBLq29xJD4xZw16REKyhJC9PFGgPSbX       |
|                                                                                |
+--------------------------------------------------------------------------------+
                                    |
                                    |
                                    v
+--------------------------------------------------------------------------------+
|     Address Root    |     Address Attributes    |           AddrType           |
|                     |                           |                              |
|   Hash (224 bits)   |  Der. Path* + Stake + NM  |  PubKey | (Script) | Redeem  |
|                     |    (open for extension)   |     (open for extension)     |
+--------------------------------------------------------------------------------+
             |                 |
             |                 |     +----------------------------------+
             v                 |     |        Derivation Path           |
+---------------------------+  |---->|                                  |
|  SHA3-256 >> Blake2b_224  |  |     | ChaChaPoly* AccountIx/AddressIx  |
|                           |  |     +----------------------------------+
|  -AddrType                |  |
|  -ASD* (~AddrType+PubKey) |  |     +----------------------------------+
|  -Address Attributes      |  |     |       Stake Distribution         |
+---------------------------+  |     |                                  |
                               |---->|  BootstrapEra | (Single | Multi) |
                               |     +----------------------------------+
                               |
                               |     +----------------------------------+
                               |     |          Network Magic           |
                               |---->|                                  |
                                     | Addr Discr: MainNet vs TestNet   |
                                     +----------------------------------+

-------------------------------------------------------------------------------}

-- | Encode the derivation path (account index and address index) part of a
-- random HD address.
--
-- This is the opposite of 'decodeDerivationPath'.
--
-- NOTE: The caller must ensure that the key length is 32 bytes, which can be
-- done by using the 'generateKeyFromSeed' and
-- 'Cardano.Wallet.Primitive.AddressDerivation.publicKey' functions.
encodeDerivationPath
    :: Key 'RootK XPub
    -> Index 'Hardened 'AccountK
    -> Index 'Soft 'AddressK
    -> CBOR.Encoding
encodeDerivationPath rootKey accIx addrIx =
    CBOR.encodeBytes $
    useInvariant $
    encryptDerPath (hdPassphrase rootKey) $
    CBOR.toStrictByteString $
    encodeDerPath accIx addrIx
  where
    -- Encryption will fail if the key is the wrong size, but that won't happen
    -- if the key was created with 'generateKeyFromSeed'.
    useInvariant (CryptoPassed res) = res
    useInvariant (CryptoFailed err) = error $ "encodeDerivationPath: " ++ show err

encodeDerPath :: Index 'Hardened 'AccountK -> Index 'Soft 'AddressK -> CBOR.Encoding
encodeDerPath (Index accIx) (Index addrIx) = mempty
    <> CBOR.encodeListLenIndef
    <> CBOR.encodeWord32 accIx
    <> CBOR.encodeWord32 addrIx
    <> CBOR.encodeBreak

-- | Decode the Addr Root + Attributes + Type section of a HD random scheme
-- address, and return the derivation path.
--
-- It will fail to parse if the address is not in the random scheme. If the
-- public key is incorrect, the decode result will be 'Nothing'.

decodeAddressDerivationPath
    :: Key 'RootK XPub
    -> CBOR.Decoder s (Maybe (Index 'Hardened 'AccountK, Index 'Soft 'AddressK))
decodeAddressDerivationPath rootKey = do
    _ <- CBOR.decodeListLenCanonicalOf 3
    _ <- CBOR.decodeBytes -- Address Root
    attrs <- decodeAllAttributes
    path <- decodePathAttribute rootKey attrs
    addrType <- CBOR.decodeWord8 -- Type
    when (addrType /= 0) $
        fail $ "decodeAddressDerivationPath: type is not 0 (public key), it is " ++ show addrType
    pure path

-- | The attributes are pairs of numeric tags and bytes, where the bytes will be
-- CBOR-encoded stuff. This decoder does not enforce "canonicity" of entries.
decodeAllAttributes :: CBOR.Decoder s [(Word8, ByteString)]
decodeAllAttributes = do
    n <- CBOR.decodeMapLenCanonical -- Address Attributes length
    replicateM n decodeAttr
  where
    decodeAttr = (,) <$> CBOR.decodeWord8 <*> CBOR.decodeBytes

decodePathAttribute
    :: Key 'RootK XPub
    -> [(Word8, ByteString)]
    -> CBOR.Decoder s (Maybe (Index 'Hardened 'AccountK, Index 'Soft 'AddressK))
decodePathAttribute rootKey attrs = case lookup derPathTag attrs of
    Just payload -> decodeNestedBytes (decodeDerivationPath rootKey) payload
    Nothing -> fail $ "decodeAddressDerivationPath: Missing attribute "
        ++ show derPathTag
  where
    derPathTag = 1 -- derivation path

-- | Decode the HD random derivation path section of an address.
-- If the public key is incorrect, the decode result will be 'Nothing'.
--
-- This is the opposite of 'encodeDerivationPath'.
decodeDerivationPath
    :: Key 'RootK XPub
    -> CBOR.Decoder s (Maybe (Index 'Hardened 'AccountK, Index 'Soft 'AddressK))
decodeDerivationPath rootKey = do
    payload <- CBOR.decodeBytes
    case decryptDerPath (hdPassphrase rootKey) payload of
        CryptoPassed plaintext ->
            Just <$> decodeNestedBytes decodeDerPath plaintext
        CryptoFailed _ ->
            pure Nothing

decodeDerPath :: CBOR.Decoder s (Index 'Hardened 'AccountK, Index 'Soft 'AddressK)
decodeDerPath = decodeListIndef CBOR.decodeWord32 >>= \case
    [accIx, addrIx] -> pure (Index accIx, Index addrIx)
    ps -> fail $ "decodeDerPath: Unexpected derivation path length ("
        ++ show (length ps) ++ ")"

  where
    -- | Decode an arbitrary long list. CBOR introduce a "break" character to
    -- mark the end of the list, so we simply decode each item until we encounter
    -- a break character.
    --
    -- Example:
    --
    --     myDecoder :: CBOR.Decoder s [MyType]
    --     myDecoder = decodeListIndef decodeOne
    --       where
    --         decodeOne :: CBOR.Decoder s MyType
    --
    decodeListIndef :: forall s a. CBOR.Decoder s a -> CBOR.Decoder s [a]
    decodeListIndef decodeOne = do
        _ <- CBOR.decodeListLenIndef
        CBOR.decodeSequenceLenIndef (flip (:)) [] reverse decodeOne

-- | Derive a symmetric key for encrypting and authenticating the address
-- derivation path.
hdPassphrase :: Key 'RootK XPub -> Passphrase "address-der"
hdPassphrase (Key rootXPub) = Passphrase $
    PBKDF2.generate
    (PBKDF2.prfHMAC SHA512)
    (PBKDF2.Parameters 500 32)
    (unXPub rootXPub)
    ("address-hashing" :: ByteString)

{-------------------------------------------------------------------------------
                    HD payload encryption and authentication
-------------------------------------------------------------------------------}

cardanoNonce :: ByteString
cardanoNonce = "serokellfore"

-- | ChaCha20/Poly1305 encrypting and signing the HD payload of addresses.
encryptDerPath
    :: Passphrase "address-der"
       -- ^ Symmetric key / passphrase, 32-byte long
    -> ByteString -- Payload to be encrypted
    -> CryptoFailable ByteString -- Ciphertext with a 128-bit crypto-tag appended.
encryptDerPath (Passphrase passphrase) payload = do
    nonce <- Poly.nonce12 cardanoNonce
    st1 <- Poly.finalizeAAD <$> Poly.initialize passphrase nonce
    let (out, st2) = Poly.encrypt payload st1
    return $ out <> BA.convert (Poly.finalize st2)

-- | ChaCha20/Poly1305 decrypting and authenticating the HD payload of
-- addresses.
decryptDerPath
    :: Passphrase "address-der"
       -- ^ Symmetric key / passphrase, 32-byte long
    -> ByteString -- Payload to be encrypted
    -> CryptoFailable ByteString
decryptDerPath (Passphrase passphrase) bytes = do
    let (payload, tag) = BS.splitAt (BS.length bytes - 16) bytes
    nonce <- Poly.nonce12 cardanoNonce
    st1 <- Poly.finalizeAAD <$> Poly.initialize passphrase nonce
    let (out, st2) = Poly.decrypt payload st1
    when (BA.convert (Poly.finalize st2) /= tag) $ CryptoFailed CryptoError_MacKeyInvalid
    return out

{-------------------------------------------------------------------------------
                                     Utils
-------------------------------------------------------------------------------}

decodeNestedBytes
    :: (forall s. CBOR.Decoder s r)
    -> ByteString
    -> CBOR.Decoder s' r
decodeNestedBytes dec bytes =
    case CBOR.deserialiseFromBytes dec (BL.fromStrict bytes) of
        Right ("", res) -> pure res
        Right _ -> fail "Leftovers when decoding nested bytes"
        _ -> fail "Could not decode nested bytes"

addrToPayload :: Address -> ByteString
addrToPayload (Address addr) =
    unsafeDeserialiseFromBytes decodeAddressPayload $ b58decode addr

-- | Decode a bitcoin base-58 address to a LBS, without error handling.
b58decode :: ByteString -> BL.ByteString
b58decode = BL.fromStrict . fromJust . decodeBase58 bitcoinAlphabet

-- | Extract the HD payload part of an legacy scheme Address, which has already
-- been base-58 decoded.
decodeAddressPayload :: CBOR.Decoder s ByteString
decodeAddressPayload = do
    _ <- CBOR.decodeListLenCanonicalOf 2
    _ <- CBOR.decodeTag
    bytes <- CBOR.decodeBytes
    _ <- CBOR.decodeWord32 -- CRC
    return bytes

-- | CBOR deserialise a strict bytestring
deserialise :: (forall s. CBOR.Decoder s a) -> ByteString -> Either CBOR.DeserialiseFailure a
deserialise dec = fmap snd . CBOR.deserialiseFromBytes dec . BL.fromStrict
