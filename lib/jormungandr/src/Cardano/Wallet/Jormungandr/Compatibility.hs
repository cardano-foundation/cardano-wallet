{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Contains various implementation decision that are specific to a particular
-- network / protocol. This allows us to easily select a particular backend
-- (Byron, Shelley-Rust, Shelley-Haskell) and isolate the bits that vary between
-- those backends.

module Cardano.Wallet.Jormungandr.Compatibility
    ( -- * Target
      Jormungandr
    , Network (..)
    , genesis
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPub (..), unXPub )
import Cardano.Wallet.Jormungandr.Binary
    ( decodeLegacyAddress )
import Cardano.Wallet.Jormungandr.Environment
    ( KnownNetwork (..), Network (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( KeyToAddress (..), getKey )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , BlockHeader (..)
    , DecodeAddress (..)
    , EncodeAddress (..)
    , Hash (..)
    , SlotId (..)
    , TxId (..)
    )
import Codec.Binary.Bech32
    ( HumanReadablePart, dataPartFromBytes, dataPartToBytes )
import Control.Monad
    ( when )
import Data.Binary.Put
    ( Put, putByteString, putWord8, runPut )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58, encodeBase58 )
import Data.Maybe
    ( isJust )
import Data.Text.Class
    ( TextDecodingError (..) )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as T

-- | A type representing the Jormungandr as a network target. This has an
-- influence on binary serializer & network primitives. See also 'TxId'
data Jormungandr (network :: Network)

-- | Genesis block header, i.e. very first block header of the chain
genesis :: BlockHeader
genesis = BlockHeader
    { slotId = (SlotId 0 0)
    , prevBlockHash = Hash (BS.replicate 32 0)
    }

instance TxId (Jormungandr n) where
    txId = undefined

instance forall n. KnownNetwork n => KeyToAddress (Jormungandr n) where
    keyToAddress key = Address
        . BL.toStrict
        . runPut
        $ putSingleAddress (getKey key)
      where
        putSingleAddress :: XPub -> Put
        putSingleAddress xpub = do
        -- NOTE: only single address supported for now
            putWord8 (single @n)
            putByteString $ unXPub xpub


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
instance KnownNetwork n => EncodeAddress (Jormungandr n) where
    encodeAddress _ (Address bytes) = do
        if isJust (decodeLegacyAddress bytes) then base58 else bech32
      where
        base58 = T.decodeUtf8 $ encodeBase58 bitcoinAlphabet bytes
        bech32 = Bech32.encodeLenient (hrp @n) (dataPartFromBytes bytes)

-- | Decode text string into an 'Address'. Jörmungandr recognizes two kind of
-- addresses:
--
-- - Legacy / Byron addresses encoded as `Base58`
-- - Shelley addresses, encoded as `Bech32`
--
-- See also 'EncodeAddress Jormungandr'
instance KnownNetwork n => DecodeAddress (Jormungandr n) where
    decodeAddress _ x =
        case (tryBech32, tryBase58) of
            (Just bytes, _) -> bech32 bytes
            (_, Just bytes) -> base58 bytes
            (Nothing, Nothing) -> Left $ TextDecodingError
                "Unable to decode address: encoding is neither Bech32 nor \
                \Base58."
      where
        -- | Attempt decoding a legacy 'Address' using a Base58 encoding.
        tryBase58 :: Maybe ByteString
        tryBase58 = decodeBase58 bitcoinAlphabet (T.encodeUtf8 x)

        -- | Verify the structure of a payload decoded from a Base58 text string
        base58 :: ByteString -> Either TextDecodingError Address
        base58 bytes = maybe (Left $ TextDecodingError errByron) Right $
            decodeLegacyAddress bytes
          where
            errByron =
                "Unable to decode address: neither Bech32-encoded nor a valid \
                \Byron address."

        -- | Attempt decoding an 'Address' using a Bech32 encoding.
        tryBech32 :: Maybe (HumanReadablePart, ByteString)
        tryBech32 = do
            (hrp', dp) <- either (const Nothing) Just (Bech32.decodeLenient x)
            (hrp',) <$> dataPartToBytes dp

        -- | Verify the structure of a payload decoded from a Bech32 text string
        bech32
            :: (HumanReadablePart, ByteString)
            -> Either TextDecodingError Address
        bech32 (hrp', bytes) = do
            when (hrp @n /= hrp') $ Left $ TextDecodingError $
                "This address belongs to another network. Network is: "
                <> show (networkVal @n) <> "."
            case BS.length bytes of
                n | n == singleAddressLength ->
                    when (BS.take 1 bytes /= BS.pack [single @n]) $
                        Left (invalidFirstByte (single @n))
                n | n == groupedAddressLength ->
                    when (BS.take 1 bytes /= BS.pack [grouped @n]) $
                        Left (invalidFirstByte (grouped @n))
                _ ->
                    Left $ TextDecodingError $
                        "Invalid address length (" <> show (BS.length bytes)
                        <> "): expected either "
                        <> show singleAddressLength
                        <> " or "
                        <> show groupedAddressLength
                        <> " bytes."
            return (Address bytes)
          where
            singleAddressLength = 33
            groupedAddressLength = 65
            invalidFirstByte discriminant = TextDecodingError
                $ "Invalid address first byte: "
                <> B8.unpack (BS.take 1 bytes)
                <> " =/= "
                <> B8.unpack (BS.pack [discriminant])
                <> "."
