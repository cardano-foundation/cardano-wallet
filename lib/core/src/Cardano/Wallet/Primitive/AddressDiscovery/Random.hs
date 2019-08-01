{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- An implementation of address discovery for the random address
-- scheme as used by the legacy Cardano wallets.

module Cardano.Wallet.Primitive.AddressDiscovery.Random
    (
    -- ** State
      RndState (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), Key, XPrv, publicKey )
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( decodeAddressDerivationPath )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery (..)
    , GenChange (..)
    , IsOurs (..)
    , IsOwned (..)
    , KnownAddresses (..)
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..) )
import Control.DeepSeq
    ( NFData )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58 )
import Data.Maybe
    ( fromJust )
import GHC.Generics
    ( Generic )

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Data.ByteString.Lazy as BL

newtype RndState = RndState { getRndState :: Key 'RootK XPrv }
    deriving (Generic)

instance NFData RndState

instance IsOurs RndState where
    isOurs (Address addr) (RndState s) = do
        let payload = unsafeDeserialiseFromBytes decodeAddressPayload $ b58decode addr
        case deserialise (decodeAddressDerivationPath $ publicKey s) payload of
            Right (Just _) -> (True, RndState s)
            _ -> (False, RndState s)

instance IsOwned RndState where
    isOwned _ _ _ = Nothing

instance GenChange RndState where
    genChange s = (error "GenChange RndState unimplemented", s)

-- Unlike sequential derivation, we can't derive an order from the index only
-- (they are randomly generated), nor anything else in the address itself.
--
-- Therefore, we'll simply consider that addresses using the random address
-- derivation scheme won't be ordered in any particular order.
instance CompareDiscovery RndState where
    compareDiscovery _ _ _ = EQ

instance KnownAddresses RndState where
    knownAddresses _ = []


-- | Extract the HD payload part of an legacy scheme Address, which has already
-- been base-58 decoded.
decodeAddressPayload :: CBOR.Decoder s ByteString
decodeAddressPayload = do
    _ <- CBOR.decodeListLenCanonicalOf 2
    _ <- CBOR.decodeTag
    bytes <- CBOR.decodeBytes
    _ <- CBOR.decodeWord32 -- CRC
    return bytes

-- | Decode a bitcoin base-58 address to a LBS, without error handling.
b58decode :: ByteString -> BL.ByteString
b58decode = BL.fromStrict . fromJust . decodeBase58 bitcoinAlphabet

-- | CBOR deserialise without error handling - handy for prototypes or testing.
unsafeDeserialiseFromBytes :: (forall s. CBOR.Decoder s a) -> BL.ByteString -> a
unsafeDeserialiseFromBytes decoder bytes =
    either (\e -> error $ "unsafeDeserialiseFromBytes: " <> show e) snd $
        CBOR.deserialiseFromBytes decoder bytes

-- | CBOR deserialise a strict bytestring
deserialise :: (forall s. CBOR.Decoder s a) -> ByteString -> Either CBOR.DeserialiseFailure a
deserialise dec = fmap snd . CBOR.deserialiseFromBytes dec . BL.fromStrict
