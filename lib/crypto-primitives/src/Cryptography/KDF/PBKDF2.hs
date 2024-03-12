{-# LANGUAGE FlexibleContexts #-}

module Cryptography.KDF.PBKDF2
    ( Parameters (..)
    , fastPBKDF2_SHA512
    , generate
    , prfHMAC

    , generateKey
    ) where

import Crypto.KDF.PBKDF2
    ( Parameters (..)
    , fastPBKDF2_SHA512
    , generate
    , prfHMAC
    )

import Prelude

import Crypto.Hash.Algorithms
    ( HashAlgorithm
    )
import Data.ByteString
    ( ByteString
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Word
    ( Word16
    , Word8
    )

import qualified Crypto.KDF.PBKDF2 as KDF
import qualified Data.ByteString as BS

-- | PBKDF2 key generation
-- | This is 'hashing' that is used to take a user password, and expand it into
-- | a binary 'cryptographic key' needed for the actual encryption work.
-- | The aim is to slow down potential attackers by being slow and memory hungry
-- | (unlike checksumming).
-- | As some encryption algorithms require both 'key' and 'iv' (for example, aes-256-cbc)
-- | the function returns both. In case the encryption algorithm does not need it
-- | like in case of elliptic encryption (for example, aes-256-ecb) one can take only 'key'
-- | and omit 'iv'.
-- | A number of pass iterations translates into slowness of the algorithm (and its security).
-- | Salting is used to randomize the hashing.
-- |
-- | OpenSSL correspondence:
-- | 1. 'key' and 'iv' needed for aes-256-cbc with sha256 no salt
-- | $ echo -n "password" | openssl enc -aes-256-cbc -pbkdf2 -pass stdin -P -nosalt -iter 10000 -P
-- | key=E11244295150E6713CD76E9A5112347093BDB6ACBF0C8021ABAE29881130B210
-- | iv =6B7F0C406297F0D90E3BD65AD1FB94BA
-- | -> generateKey SHA256 10000 (32,16) "password" Nothing
-- |
-- | 2. 'key' needed for aes-256-ecb with sha256 no salt
-- | $ echo -n "password" | openssl enc -aes-256-ecb -pbkdf2 -pass stdin -P -nosalt -iter 10000 -P
-- | key=E11244295150E6713CD76E9A5112347093BDB6ACBF0C8021ABAE29881130B210
-- | -> fst $ generateKey SHA256 10000 (32,0) "password" Nothing
-- |
-- | 3. 'key' needed for aes-256-ecb with sha512 no salt
-- | $ echo -n "password" | openssl enc -aes-256-ecb -pbkdf2 -pass stdin -P -md sha512 -nosalt -iter 10000 -P
-- | key=F4CA507C07D0BD31BC779A08756826A6FD9DD97D43AC25E4B29A0933ABEA03F3
-- | -> fst $ generateKey SHA512 10000 (32,0) "password" Nothing
-- |
-- | 4. 'key' and 'iv' needed for aes-256-cbc with sha256 and salt='00000000' (which is in hex equivalent to '3030303030303030')
-- | $ echo -n "metadata-secret" | openssl enc -aes-256-cbc -pbkdf2 -pass stdin -P -S 3030303030303030 -iter 10000 -P
-- | salt=3030303030303030
-- | key=57FCB522B950BCB78138EECFE7FBE07881E5B49AAA2D1CD761D4495A09A5F16C
-- | iv =41D88E094C8F202C7DF6654B7F40E5AF
-- | generateKey SHA512 10000 (32,16) "metadata-secret" (Just "00000000")
generateKey
    :: HashAlgorithm h
    => h
    -- ^ hash algorithm type, like SHA128, SHA256, SHA512
    -> Word16
    -- ^ number of iterations algorithm runs through, more iterations mean slower
    -> (Word8, Word8)
    -- ^ key length in bytes, iv length in bytes
    -> ByteString
    -- ^ payload
    -> Maybe ByteString
    -- ^ salt
    -> (ByteString, ByteString)
    -- ^ (key, iv)
generateKey hashAlgoType iterations (keyLen, ivLen) payload saltM =
    BS.splitAt (fromIntegral keyLen) whole
  where
    whole = KDF.generate
        (KDF.prfHMAC hashAlgoType)
        (KDF.Parameters (fromIntegral iterations) (fromIntegral $ keyLen + ivLen))
        payload
        (fromMaybe BS.empty saltM)
