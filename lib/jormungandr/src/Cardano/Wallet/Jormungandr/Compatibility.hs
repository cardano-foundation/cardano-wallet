{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- Sadly, we have to work with orhpans here because addresses are encoded and
-- decoded based on type-classes, but the implementation differs from a backend
-- to another...
--
-- Alternatively, we could also use an explicit type parameter to represent and
-- captures the type of backend, but this has many ramification through the API.
-- An orphan instance here is probably slightly less annoying, in particular if
-- we assume that the jormungandr as a target could soon be removed; we would
-- only have to move the instances back in the right place whereas going for a
-- type-level backend would require again, going through all the API-type
-- ramification necessary to make it work.
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Contains various implementation decision that are specific to a particular
-- network / protocol. This allows us to easily select a particular backend
-- (Byron, Shelley-Rust, Shelley-Haskell) and isolate the bits that vary between
-- those backends.

module Cardano.Wallet.Jormungandr.Compatibility
    ( -- * Target
      Jormungandr
    , softTxMaxSize

      -- * Node's Configuration
    , BaseUrl (..)
    , Scheme (..)
    , localhostBaseUrl
    , baseUrlToText
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( DecodeAddress (..), EncodeAddress (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( decodeLegacyAddress )
import Cardano.Wallet.Primitive.AddressDerivation.Jormungandr
    ( decodeJormungandrAddress )
import Cardano.Wallet.Primitive.Types
    ( Address (..), testnetMagic )
import Codec.Binary.Bech32
    ( dataPartFromBytes, dataPartToBytes )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58, encodeBase58 )
import Data.Either.Extra
    ( maybeToEither )
import Data.Function
    ( (&) )
import Data.Maybe
    ( isJust )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( TextDecodingError (..) )
import Data.Word
    ( Word16 )
import GHC.TypeLits
    ( KnownNat )
import Servant.Client.Core
    ( BaseUrl (..), Scheme (..), showBaseUrl )

import qualified Cardano.Byron.Codec.Cbor as CBOR
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | A type representing the Jormungandr as a backend target. This has an
-- influence on binary serializer & network primitives. See also 'DefineTx'
data Jormungandr

-- | Jörmugandr's chain parameter doesn't include a transaction max size. The
-- actual hard-limit for the size is constrained by the binary format and
-- numbers used to represent the number of inputs and outputs (Word8), yet
-- there's also a soft-limit of 8kb which results in much smaller transactions
-- in the end.
softTxMaxSize :: Quantity "byte" Word16
softTxMaxSize = Quantity 8192

{-------------------------------------------------------------------------------
                                     Base URL
-------------------------------------------------------------------------------}

localhostBaseUrl :: Int -> BaseUrl
localhostBaseUrl port = BaseUrl Http "127.0.0.1" port ""

-- | Format an API 'BaseUrl', for logging, etc.
baseUrlToText :: BaseUrl -> T.Text
baseUrlToText = T.pack . showBaseUrl

{-------------------------------------------------------------------------------
                      Address Encoding / Decoding
-------------------------------------------------------------------------------}

-- | Encode an 'Address' to a human-readable format. This produces two kinds of
-- encodings:
--
-- - [Base58](https://en.wikipedia.org/wiki/Base58)
--   for legacy / Byron addresses
-- - [Bech32](https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki)
--   for Jormungandr/Shelley addresses
--
-- The right encoding is picked by looking at the raw 'Address' representation
-- in order to figure out to which class the address belongs.
instance EncodeAddress 'Mainnet where
    encodeAddress = gEncodeAddress

instance EncodeAddress ('Testnet pm) where
    encodeAddress = gEncodeAddress

gEncodeAddress :: Address -> Text
gEncodeAddress (Address bytes) =
    if isJust (CBOR.deserialiseCbor CBOR.decodeAddressPayload bytes)
        then base58
        else bech32
  where
    base58 = T.decodeUtf8 $ encodeBase58 bitcoinAlphabet bytes
    bech32 = Bech32.encodeLenient hrp (dataPartFromBytes bytes)
    hrp = [Bech32.humanReadablePart|addr|]

-- | Decode text string into an 'Address'. Jörmungandr recognizes two kind of
-- addresses:
--
-- - Legacy / Byron addresses encoded as `Base58`
-- - Jormungandr addresses, encoded as `Bech32`
-- - Shelley addresses, encoded as `Bech32`
--
-- See also 'EncodeAddress Jormungandr'
instance DecodeAddress 'Mainnet where
    decodeAddress = gDecodeAddress
        (decodeJormungandrAddress @'Mainnet)
        (decodeLegacyAddress Nothing)

instance KnownNat pm => DecodeAddress ('Testnet pm) where
    decodeAddress = gDecodeAddress
        (decodeJormungandrAddress @('Testnet pm))
        (decodeLegacyAddress $ Just $ testnetMagic @pm)

gDecodeAddress
    :: (ByteString -> Either TextDecodingError Address)
    -> (ByteString -> Maybe Address)
    -> Text
    -> Either TextDecodingError Address
gDecodeAddress decodeJormungandr decodeByron text =
    case (tryBech32, tryBase58) of
        (Just bytes, _) -> decodeJormungandr bytes
        (_, Just bytes) -> decodeByron bytes
            & maybeToEither (TextDecodingError
            "Unable to decode Address: neither Bech32-encoded nor a \
            \valid Byron Address.")
        (Nothing, Nothing) -> Left $ TextDecodingError
            "Unable to decode Address: encoding is neither Bech32 nor \
            \Base58."
  where
    -- | Attempt decoding a legacy 'Address' using a Base58 encoding.
    tryBase58 :: Maybe ByteString
    tryBase58 =
        decodeBase58 bitcoinAlphabet (T.encodeUtf8 text)

    -- | Attempt decoding an 'Address' using a Bech32 encoding.
    tryBech32 :: Maybe ByteString
    tryBech32 = do
        (_, dp) <- either (const Nothing) Just (Bech32.decodeLenient text)
        dataPartToBytes dp
