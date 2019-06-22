{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Unsafe
    ( unsafeFromHex
    , unsafeDecodeAddress
    , unsafeDecodeHex
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( Address, DecodeAddress (..) )
import Data.Binary.Get
    ( Get, runGet )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.Proxy
    ( Proxy )
import Data.Text
    ( Text )

import qualified Data.ByteString.Lazy as BL

-- | Decode an hex-encoded 'ByteString' into raw bytes, or fail.
unsafeFromHex :: ByteString -> ByteString
unsafeFromHex =
    either (error . show) id . convertFromBase @ByteString @ByteString Base16

-- | Decode a bech32-encoded 'Text' into an 'Address', or fail.
unsafeDecodeAddress :: DecodeAddress t => Proxy t -> Text -> Address
unsafeDecodeAddress proxy =
    either (error . show ) id . decodeAddress proxy

-- | Run a decoder on a hex-encoded 'ByteString', or fail.
unsafeDecodeHex :: Get a -> ByteString -> a
unsafeDecodeHex get = runGet get . BL.fromStrict . unsafeFromHex
