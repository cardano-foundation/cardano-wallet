{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

Abstract and specific Hash functionality.
-}
module Cardano.Wallet.Read.Hash
    ( -- * Core operations
      H.Hash
    , H.HashAlgorithm (H.digest, H.hashAlgorithmName)
    , H.sizeHash
    , H.hashWith

    -- * Conversions
    , H.castHash
    , H.hashToBytes
    , H.hashFromBytes
    , H.hashToBytesShort
    , H.hashFromBytesShort

    -- * Rendering and parsing
    , H.hashToBytesAsHex
    , H.hashFromBytesAsHex
    , H.hashToTextAsHex
    , H.hashFromTextAsHex
    , H.hashToStringAsHex
    , H.hashFromStringAsHex

    -- * Specific Hash algorithms
    , Blake2b_224
    , Blake2b_256
    ) where

import Cardano.Crypto.Hash.Blake2b
    ( Blake2b_224
    , Blake2b_256
    )
import qualified Cardano.Crypto.Hash.Class as H
