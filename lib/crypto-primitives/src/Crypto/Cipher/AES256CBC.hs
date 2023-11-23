{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Crypto.Cipher.AES256CBC
    ( encrypt
    , decrypt

    , CryptoError (..)
    , CryptoFailable (..)
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
-- | That family includes no less than 15 variants, for three possible block sizes (128, 192 and 256 bits).
-- | Three are supported by cryptonite, AES128, AES192 and AES256 (chosen here).
-- | There are five possible key sizes (128, 160, 192, 224 and 256 bits).
-- | In case of cryptonite impl we have the following constraints: (AES128 - 16 bytes, AES192 - 24 bytes, AES 256 - 32 bytes).
-- | AES, as standardized by NIST, includes only three variants, all with 128-bit blocks, and with keys of 128, 192 or 256 bits.
-- | The same as adopted in cryptonite.
-- | The block size of AES is always 128 bits, so a 256 bit IV is not possible for most modes of operation.
-- | These are the reasons we requires secret key to be 32 bytes and iv 16 bytes for AES256 used here.
-- | Additionally, input length must be a multiple of block size, ie., 16 bytes.

-- | Relation to openSSL
-- | Let's prepare 32-byte secret key and 16-byte iv from passphrase "metadata-secret"
-- | using PBKDF2 for 10000 iterations and salt "00000000". openSSL requires salt to be
-- | represented as hex encoded with will be here "3030303030303030"
-- |
-- | $ openssl enc -e -aes-256-cbc -pbkdf2 -iter 10000 -a -k "metadata-secret" -S 3030303030303030 -P
-- | salt=3030303030303030
-- | key=57FCB522B950BCB78138EECFE7FBE07881E5B49AAA2D1CD761D4495A09A5F16C
-- | iv =41D88E094C8F202C7DF6654B7F40E5AF
-- |
-- | Now let's encrypt 16-byte payload "0000000000000000". Both secret key and iv must be hex-decoded.
-- |
-- | $ openssl enc -e -aes-256-cbc \
-- | -K '57FCB522B950BCB78138EECFE7FBE07881E5B49AAA2D1CD761D4495A09A5F16C' \
-- | -iv '41D88E094C8F202C7DF6654B7F40E5AF' -base64 -nosalt  <<< '0000000000000000'
-- | /Nu/x5WeHn++YkLbIozfZHdt3sxYGhGNERXuX6hqnQU=
-- | $ openssl enc -d -aes-256-cbc \
-- | -K '57FCB522B950BCB78138EECFE7FBE07881E5B49AAA2D1CD761D4495A09A5F16C' \
-- | -iv '41D88E094C8F202C7DF6654B7F40E5AF' -base64 -nosalt  <<< '/Nu/x5WeHn++YkLbIozfZHdt3sxYGhGNERXuX6hqnQU='
-- | 0000000000000000
-- |
-- | Encryption and decryption works.
-- | When we use the module's encrypt and decrypt for the same key, iv, and payload we have
-- | payload === decrypt . encrypt $ payload
-- | and encrypted payload is
-- | /Nu/x5WeHn++YkLbIozfZA==
-- | TO_DO understand this discrepancy + sort out how to deal with payload that are not multitude of 16-bytes.

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
    -- ^ payload, must be a multiple of block size, ie., 16 bytes
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
    -- ^ payload, must be a multiple of block size, ie., 16 bytes
    -> Either CryptoError ByteString
decrypt key iv msg = do
   initedIV <- initIV iv
   case initCipher key of
     Left e -> Left e
     Right c -> Right $ cbcDecrypt c initedIV msg
