{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Common hashing functions used and re-used across the codebase.
module Crypto.Hash.Utils
    ( blake2b256
    , blake2b224
    ) where

import Prelude

import Crypto.Hash
    ( hash
    )
import Crypto.Hash.Algorithms
    ( Blake2b_224
    , Blake2b_256
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
