{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Cryptography.Cipher.AES256CBC
    ( encrypt
    , decrypt
    , paddingPKCS7
    , unpaddingPKCS7
    ) where

import Prelude

import Crypto.Cipher.AES
    ( AES256
    )
import Crypto.Cipher.Types
    ( BlockCipher (..)
    , Cipher (..)
    , IV
    , makeIV
    )
import Crypto.Error
    ( CryptoError (..)
    , CryptoFailable (..)
    )
import Data.ByteString
    ( ByteString
    )

-- | AES is a subset of the family of block ciphers known as Rijndael.
-- That family includes no less than 15 variants, for three possible block sizes (128, 192 and 256 bits).
-- Three are supported by cryptonite, AES128, AES192 and AES256 (used here).
-- There are five possible key sizes (128, 160, 192, 224 and 256 bits).
-- In case of cryptonite implementation we have the following constraints:
-- (AES128 - 16 bytes, AES192 - 24 bytes, AES 256 - 32 bytes).
-- AES, as standardized by NIST, includes only three variants, all with 128-bit blocks,
-- and with keys of 128, 192 or 256 bits.
-- The same as adopted in cryptonite.
-- The block size of AES is always 128 bits, so a 256 bit IV is not possible for most modes of operation.
-- These are the reasons we require the secret key to be 32 bytes and iv 16 bytes for AES256 used here.
-- Additionally, input length must be a multiple of block size, ie., 16 bytes.
-- In order to realize this PKCS#7 padding is available as an option.
-- This padding is preferred because it is unambiguous and widely used (for example in OpenSSL).
-- One can always read the last byte and remove that number of padding bytes,
-- and then detect if the decryption was successful by checking that all of
-- the bytes removed have the same value.

-- | Initialize a block cipher
initCipher
    :: ByteString
    -> Either CryptoError AES256
initCipher k = case cipherInit k of
   CryptoFailed e -> Left e
   CryptoPassed a -> Right a

-- | Initialize IV
initIV
    :: ByteString
    -> Either CryptoError (IV AES256)
initIV iv =
    case makeIV iv of
        Nothing -> Left CryptoError_IvSizeInvalid
        Just ivOk -> pure ivOk

-- | Encrypt using AES256 using CBC mode
encrypt
    :: ByteString
    -- ^ secret key, needs to be 32 bytes
    -> ByteString
    -- ^ iv, needs to be 16 bytes
    -> ByteString
    -- ^ payload, must be a multiple of a block size, ie., 16 bytes
    -> Either CryptoError ByteString
encrypt key iv msg = do
   initedIV <- initIV iv
   case initCipher key of
     Left e -> Left e
     Right c -> Right $ cbcEncrypt c initedIV msg

-- | Decrypt using AES256 using CBC mode
decrypt
    :: ByteString
    -- ^ secret key, needs to be 32 bytes
    -> ByteString
    -- ^ iv, needs to be 16 bytes
    -> ByteString
    -- ^ payload, must be a multiple of a block size, ie., 16 bytes
    -> Either CryptoError ByteString
decrypt key iv msg = do
   initedIV <- initIV iv
   case initCipher key of
     Left e -> Left e
     Right c -> Right $ cbcDecrypt c initedIV msg

-- | Apply PKCS#7 padding to payload and end up with a multiple of a block size, i.e., 16 bytes,
-- according to https://datatracker.ietf.org/doc/html/rfc5652#section-6.3 .
-- The padding value is the number of padding bytes.
-- If 1 byte of padding is required, the padding is "01".
-- If 2 bytes of padding, it's "02 02".
-- If no padding is required, an extra block of 0x10 bytes is added,
-- i.e., sixteen copies of 16, which is the blocksize.
-- This means that payload can only fit 15 bytes into a single block with padding.
-- A 16 byte payload requires 2 blocks with padding.
paddingPKCS7
    :: ByteString
    -> ByteString
paddingPKCS7 = undefined

unpaddingPKCS7
    :: ByteString
    -> ByteString
unpaddingPKCS7 = undefined
