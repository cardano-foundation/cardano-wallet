{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Cryptography.Hash.Blake
    ( Blake2b_160
    , Blake2b_224
    , Blake2b_256

    , blake2b256
    , blake2b224
    , hashSizeBlake2b224
    ) where

import Prelude

import Crypto.Hash
    ( hash
    )
import Crypto.Hash.Algorithms
    ( Blake2b_160 (..)
    , Blake2b_224 (..)
    , Blake2b_256 (..)
    )
import Crypto.Hash.IO
    ( HashAlgorithm (hashDigestSize)
    )
import Data.ByteArray
    ( ByteArrayAccess
    )
import Data.ByteString
    ( ByteString
    )

import qualified Data.ByteArray as BA

-- | Hash a byte string using Blake2b with a 256-bit (32-byte) digest.
blake2b256 :: ByteArrayAccess a => a -> ByteString
blake2b256 = BA.convert . hash @_ @Blake2b_256

-- | Hash a byte string using Blake2b with a 224-bit (28-byte) digest.
blake2b224 :: ByteArrayAccess a => a -> ByteString
blake2b224 = BA.convert . hash @_ @Blake2b_224

hashSizeBlake2b224 :: Int
hashSizeBlake2b224 = hashDigestSize Blake2b_224
