{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Unsafe
    ( unsafeFromHex
    , unsafeDecodeAddress
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( Address, DecodeAddress (..) )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.Proxy
    ( Proxy )
import Data.Text
    ( Text )

unsafeFromHex :: ByteString -> ByteString
unsafeFromHex =
    either (error . show) id . convertFromBase @ByteString @ByteString Base16

unsafeDecodeAddress :: DecodeAddress t => Proxy t -> Text -> Address
unsafeDecodeAddress proxy =
    either (error . show ) id . decodeAddress proxy
