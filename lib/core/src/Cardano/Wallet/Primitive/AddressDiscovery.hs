{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- This module contains types for address discovery. The two address discovery
-- schemes implemented are:
--
--  * "Cardano.Wallet.Primitive.AddressDiscovery.Sequential"
--  * "Cardano.Wallet.Primitive.AddressDiscovery.Random"

module Cardano.Wallet.Primitive.AddressDiscovery
    (
    -- * Abstractions
      IsOurs(..)
    , IsOwned(..)
    , GenChange(..)
    , CompareDiscovery(..)
    , KnownAddresses(..)
    , EncodeAddress(..)
    , DecodeAddress(..)
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPrv )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), NetworkDiscriminant (..), Passphrase (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( decodeLegacyAddress )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( decodeShelleyAddress )
import Cardano.Wallet.Primitive.Types
    ( Address (..) )
import Codec.Binary.Bech32
    ( HumanReadablePart
    , dataPartFromBytes
    , dataPartToBytes
    , unsafeHumanReadablePartFromText
    )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58, encodeBase58 )
import Data.Function
    ( (&) )
import Data.Maybe
    ( isJust )
import Data.Text
    ( Text )
import Data.Text.Class
    ( TextDecodingError (..) )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.Text.Encoding as T

-- | This abstraction exists to give us the ability to keep the wallet business
-- logic agnostic to the address derivation and discovery mechanisms.
--
-- This is needed because two different address schemes lives on Cardano:
--
--   - A hierarchical random scheme:
--      rather 'custom' made, with several flaws; this is the original and now
--      legacy address scheme.
--
--   - A hierarchical sequential scheme:
--      a new scheme based on the BIP-0044 specification, which is better suited
--      for our present needs.
--
-- In practice, we will need a wallet that can support both, even if not at the
-- same time, and this little abstraction can buy us this without introducing
-- too much overhead.
class IsOurs s where
    isOurs
        :: Address
        -> s
        -> (Bool, s)
        -- ^ Checks whether an address is ours or not.

-- | More powerful than 'isOurs', this abstractions offer the underlying state
-- the ability to find / compute the address private key corresponding to a
-- given known address.
--
-- Requiring 'IsOwned' as a constraint supposed that there is a way to recover
-- the root private key of a particular wallet. This isn't true for externally
-- owned wallet which would delegate its key management to a third party (like
-- a hardware Ledger or Trezor).
class IsOurs s => IsOwned s key where
    isOwned
        :: s
        -> (key 'RootK XPrv, Passphrase "encryption")
        -> Address
        -> Maybe (key 'AddressK XPrv, Passphrase "encryption")
        -- ^ Derive the private key corresponding to an address. Careful, this
        -- operation can be costly. Note that the state is discarded from this
        -- function as we do not intend to discover any addresses from this
        -- operation; This is merely a lookup from known addresses.

-- | Abstracting over change address generation. In theory, this is only needed
-- for sending transactions on a wallet following a particular scheme. This
-- abstractions allows for defining an heuristic to pick new change address. For
-- instance, in BIP-44, change addresses belong to a particular change chain
-- (also called "Internal Chain").
class GenChange s where
    type ArgGenChange s :: *
    genChange
        :: ArgGenChange s
        -> s
        -> (Address, s)
        -- ^ Generate a new change address for the given scheme. The rules for
        -- generating a new change address depends on the underlying scheme.

-- | Ordering addresses by discovery date.
--
-- If `a1` has been discovered before `a2`, then the following equation holds:
--
-- @
-- compareDiscovery s a1 a2 == LT
-- @
--
-- If `a1` has been discovered after `a2`, then the following equation holds:
--
-- @
-- compareDiscovery s a1 a2 == GT
-- @
--
-- Note that, if an address isn't known it is considered not discovered and
-- therefore, is always _greater than_ any known address.
class CompareDiscovery s where
    compareDiscovery
        :: s
        -> Address
        -> Address
        -> Ordering

-- | Extract the list of all known addresses.
--
-- NOTE: Change addresses aren't considered "known" until they've been used. The
-- rationale is that, we don't want users or consumers of the wallet to be using
-- change addresses prematurely.
class KnownAddresses s where
    knownAddresses
        :: s
        -> [Address]

-- | An abstract class to allow encoding of addresses depending on the target
-- backend used.
class EncodeAddress (n :: NetworkDiscriminant) where
    encodeAddress :: Address -> Text

-- | An abstract class to allow decoding of addresses depending on the target
-- backend used.
class DecodeAddress (n :: NetworkDiscriminant) where
    decodeAddress :: Text -> Either TextDecodingError Address

{-------------------------------------------------------------------------------
                                  Instances
-------------------------------------------------------------------------------}

-- | Encode an 'Address' to a human-readable format. This produces two kinds of
-- encodings:
--
-- - [Base58](https://en.wikipedia.org/wiki/Base58)
--   for legacy / Byron addresses
-- - [Bech32](https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki)
--   for Shelley addresses
--
-- The right encoding is picked by looking at the raw 'Address' representation
-- in order to figure out to which class the address belongs.
instance EncodeAddress 'Mainnet where
    encodeAddress = gEncodeAddress (unsafeHumanReadablePartFromText "ca")

instance EncodeAddress 'Testnet where
    encodeAddress = gEncodeAddress (unsafeHumanReadablePartFromText "ta")

gEncodeAddress :: HumanReadablePart -> Address -> Text
gEncodeAddress hrp (Address bytes) =
    if isJust (decodeLegacyAddress bytes) then base58 else bech32
  where
    base58 = T.decodeUtf8 $ encodeBase58 bitcoinAlphabet bytes
    bech32 = Bech32.encodeLenient hrp (dataPartFromBytes bytes)

-- | Decode text string into an 'Address'. Jörmungandr recognizes two kind of
-- addresses:
--
-- - Legacy / Byron addresses encoded as `Base58`
-- - Shelley addresses, encoded as `Bech32`
--
-- See also 'EncodeAddress Jormungandr'
instance DecodeAddress 'Mainnet where
    decodeAddress = gDecodeAddress (decodeShelleyAddress @'Mainnet)

instance DecodeAddress 'Testnet where
    decodeAddress = gDecodeAddress (decodeShelleyAddress @'Testnet)

gDecodeAddress
    :: (ByteString -> Either TextDecodingError Address)
    -> Text
    -> Either TextDecodingError Address
gDecodeAddress decodeShelley text =
    case (tryBech32, tryBase58) of
        (Just bytes, _) -> decodeShelley bytes
        (_, Just bytes) -> decodeLegacyAddress bytes
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

    -- | Convert a 'Maybe' to a 'Either e' with the given error @e@
    maybeToEither :: e -> Maybe a -> Either e a
    maybeToEither e = maybe (Left e) Right
