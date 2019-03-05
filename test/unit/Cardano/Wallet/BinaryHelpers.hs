{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.BinaryHelpers
    ( hash16
    , addr58
    , unsafeDeserialiseFromBytes
    ) where

import Cardano.Wallet.Primitive
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58 )
import qualified Data.ByteString.Lazy as BL
import Prelude

-- | Make a Hash from a Base16 encoded string, without error handling.
hash16 :: ByteString -> Hash a
hash16 = either bomb Hash . convertFromBase Base16
    where
        bomb msg = error ("Could not decode test string: " <> msg)

-- | Make an Address from a Base58 encoded string, without error handling.
addr58 :: ByteString -> Address
addr58 = maybe (error "addr58: Could not decode") Address . decodeBase58 bitcoinAlphabet

-- | CBOR deserialise without error handling - handy for prototypes or testing.
unsafeDeserialiseFromBytes :: (forall s. CBOR.Decoder s a) -> BL.ByteString -> a
unsafeDeserialiseFromBytes decoder bytes =
    either (\e -> error $ "unsafeDeserialiseFromBytes: " <> show e) snd $
        CBOR.deserialiseFromBytes decoder bytes
