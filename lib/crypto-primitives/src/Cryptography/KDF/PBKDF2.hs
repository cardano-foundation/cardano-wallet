{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cryptography.KDF.PBKDF2
    ( PBKDF2Config (..)
    , generateKey

    , Parameters (..)
    , fastPBKDF2_SHA512
    ) where

import Prelude

import Crypto.Hash.Algorithms
    ( HashAlgorithm
    )
import Crypto.KDF.PBKDF2
    ( Parameters (..)
    , fastPBKDF2_SHA512
    , generate
    , prfHMAC
    )
import Data.ByteString
    ( ByteString
    )
import Data.Maybe
    ( fromMaybe
    )

import qualified Data.ByteString as BS

-- | PBKDF2 key generation
-- The PBKDF2 key generation function turns a plain user password or passphrase
-- into a cryptographic key suitable for encrypting data.
-- This function adds computational work intentionally,
-- so that it becomes more difficult to find the password
-- through systematic and automated guessing.
-- As some encryption algorithms require both 'key' and 'iv' (for example, aes-256-cbc)
-- the function returns both. In case the encryption algorithm does not need it
-- like in case of elliptic encryption (for example, aes-256-ecb) one can take only 'key'
-- and omit 'iv'.
-- A number of pass iterations translates into slowness of the algorithm (and its security).
-- A salt is used to randomize the hashing and overall is advised to be used.
data PBKDF2Config h = PBKDF2Config
    { hash :: h
    -- ^ Hash algorithm type, e.g. SHA128, SHA256, SHA512.
    , iterations :: Int
    -- ^ Number of iterations of the hash algorithm. More iterations means slower.
    , keyLength :: Int
    -- ^ key length in bytes
    , ivLength :: Int
    -- ^ iv length in bytes
    }

generateKey
    :: HashAlgorithm h
    => PBKDF2Config h
    -> ByteString
    -- ^ payload
    -> Maybe ByteString
    -- ^ salt
    -> (ByteString, ByteString)
    -- ^ (key, iv)
generateKey PBKDF2Config{hash,iterations,keyLength,ivLength} payload saltM =
    BS.splitAt keyLength whole
  where
    whole = generate
        (prfHMAC hash)
        (Parameters iterations (keyLength + ivLength))
        payload
        (fromMaybe BS.empty saltM)
