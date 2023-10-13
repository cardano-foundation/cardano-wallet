{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- Orphan instances for {Encode,Decode}Address until we get rid of the
-- Jörmungandr dual support.
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Conversion functions and static chain settings for Shelley.
module Cardano.Wallet.Address.Encoding
    ( fromStakeCredential
    , shelleyEncodeStakeAddress
    , shelleyDecodeStakeAddress
    , shelleyEncodeAddress
    , shelleyDecodeAddress
    , inspectAddress
    , toHDPayloadAddress
    , encodeStakeAddress
    , decodeStakeAddress
    , encodeAddress
    , decodeAddress
    ) where

import Prelude

import Cardano.Address
    ( unsafeMkAddress )
import Cardano.Crypto.Hash.Class
    ( hashToBytes )
import Cardano.Wallet.Primitive.NetworkId
    ( SNetworkId (..) )
import Codec.Binary.Bech32
    ( dataPartFromBytes, dataPartToBytes )
import Control.Applicative
    ( (<|>) )
import Control.Arrow
    ( left )
import Control.Lens
    ( (&) )
import Control.Monad
    ( when, (>=>) )
import Data.Binary.Put
    ( putByteString, putWord8, runPut )
import Data.Bits
    ( (.&.), (.|.) )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, encodeBase58 )
import Data.Maybe
    ( isJust )
import Data.Text
    ( Text )
import Data.Text.Class
    ( TextDecodingError (..) )
import Data.Word
    ( Word8 )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardCrypto )

import qualified Cardano.Address as CA
import qualified Cardano.Address.Style.Shelley as CA
import qualified Cardano.Byron.Codec.Cbor as CBOR
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Ledger.Address as SL
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Credential as SL
import qualified Cardano.Ledger.Crypto as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.RewardAccount as W
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Codec.CBOR.Decoding as CBOR
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as T


{-------------------------------------------------------------------------------
                      Address Encoding / Decoding
-------------------------------------------------------------------------------}

networkIdMask :: Word8
networkIdMask = 0x0F

toNetworkId :: SL.Network -> Word8
toNetworkId = \case
    SL.Testnet -> 0
    SL.Mainnet -> 1

keyhashStakeAddressPrefix :: Word8
keyhashStakeAddressPrefix = 0xE0

scripthashStakeAddressPrefix :: Word8
scripthashStakeAddressPrefix = 0xF0

-- | Convert a stake credentials to a 'RewardAccount' type.
--
-- Unlike with Jörmungandr, the reward account payload doesn't represent a
-- public key but a HASH of a public key.
--
fromStakeCredential :: SL.Credential 'SL.Staking crypto -> W.RewardAccount
fromStakeCredential = \case
    SL.ScriptHashObj (SL.ScriptHash h) ->
        W.FromScriptHash (hashToBytes h)
    SL.KeyHashObj (SL.KeyHash h) ->
        W.FromKeyHash (hashToBytes h)


shelleyEncodeStakeAddress :: SL.Network -> W.RewardAccount -> Text
shelleyEncodeStakeAddress network acct =
    Bech32.encodeLenient hrp (dataPartFromBytes (bytes acct))
  where
    hrp = case network of
        SL.Testnet -> [Bech32.humanReadablePart|stake_test|]
        SL.Mainnet -> [Bech32.humanReadablePart|stake|]
    bytes = \case
        W.FromKeyHash bs ->
            BL.toStrict $ runPut $ do
                putWord8
                    $ (networkIdMask .&. toNetworkId network)
                        .|. keyhashStakeAddressPrefix
                putByteString bs
        W.FromScriptHash bs ->
            BL.toStrict $ runPut $ do
                putWord8
                    $ (networkIdMask .&. toNetworkId network)
                        .|. scripthashStakeAddressPrefix
                putByteString bs


-- necessary evil for use of MonadFail in a library
newtype ReportFailure a = ReportFailure { reportFailure :: Either String a }
    deriving (Functor, Applicative, Monad) via (Either String)

instance MonadFail ReportFailure where fail = ReportFailure . Left

shelleyDecodeStakeAddress ::
    SL.Network -> Text -> Either TextDecodingError W.RewardAccount
shelleyDecodeStakeAddress serverNetwork txt = do
    (_, dp) <- left (const errBech32) $ Bech32.decodeLenient txt
    bytes <- maybe (Left errBech32) Right $ dataPartToBytes dp
    rewardAcnt <- SL.decodeRewardAcnt @StandardCrypto bytes
        & left (TextDecodingError . show @String) . reportFailure
    guardNetwork (SL.getRwdNetwork rewardAcnt) serverNetwork
    pure $ fromStakeCredential $ SL.getRwdCred rewardAcnt
  where
    errBech32 = TextDecodingError
        "Unable to decode stake-address: must be a valid bech32 string."

shelleyEncodeAddress :: SL.Network -> W.Address -> Text
shelleyEncodeAddress network (W.Address bytes) =
    if isJust (CBOR.deserialiseCbor CBOR.decodeAddressPayload bytes)
        then base58
        else bech32
  where
    base58 = T.decodeUtf8 $ encodeBase58 bitcoinAlphabet bytes
    bech32 = Bech32.encodeLenient hrp (dataPartFromBytes bytes)
    hrp = case network of
        SL.Testnet -> [Bech32.humanReadablePart|addr_test|]
        SL.Mainnet -> [Bech32.humanReadablePart|addr|]

decodeBytes :: Text -> Either TextDecodingError ByteString
decodeBytes t =
    case tryBech32 t <|> tryBase58 t of
        Just bytes ->
            Right bytes
        _ ->
            Left $ TextDecodingError $ unwords
                [ "Unrecognized address encoding: must be either bech32 or base58."
                , "Perhaps your address is not entirely correct?"
                , "Please double-check each character within the address and try again."
                ]

-- | Attempt to decode a Shelley 'Address' using a Bech32 encoding.
tryBech32 :: Text -> Maybe ByteString
tryBech32 = fmap CA.unAddress . CA.fromBech32

-- | Attempt to decode a legacy Byron 'Address' using a Base58 encoding.
--
-- NOTE: As of Oct 2021, the Shelley ledger does *not* check whether
-- a Byron address is in valid Byron binary format. This implies that
-- an invalid Base58 Byron address can be interpreted as a valid Shelly
-- address, which results in unexpected loss of user funds.
--
-- Here, the 'tryBase58' function uses 'Cardano.Address',
-- which performs the additional check of deserializing the
-- address from Byron CBOR format.
--
-- Even so, we strongly recommend the Bech32 format,
-- as it includes error detection
-- and is more robust against typos and misspellings.
tryBase58 :: Text -> Maybe ByteString
tryBase58 = fmap CA.unAddress . CA.fromBase58

errMalformedAddress :: TextDecodingError
errMalformedAddress = TextDecodingError
    "Unable to decode address: not a well-formed Shelley nor Byron address."

-- Note that for 'Byron', we always assume no discrimination. In
-- practice, there is one discrimination for 'Shelley' addresses, and one for
-- 'Byron' addresses. Yet, on Mainnet, 'Byron' addresses have no explicit
-- discrimination.
shelleyDecodeAddress :: SL.Network -> Text -> Either TextDecodingError W.Address
shelleyDecodeAddress serverNetwork =
    decodeBytes >=> decodeShelleyAddress @StandardCrypto
  where
    decodeShelleyAddress :: forall c.
        (SL.Crypto c) => ByteString -> Either TextDecodingError W.Address
    decodeShelleyAddress bytes = do
        case SL.deserialiseAddr @c bytes of
            Just (SL.Addr addrNetwork _ _) -> do
                guardNetwork addrNetwork serverNetwork
                pure (W.Address bytes)

            Just (SL.AddrBootstrap (SL.BootstrapAddress addr)) -> do
                guardNetwork
                    (fromByronNetworkMagic (Byron.addrNetworkMagic addr))
                    serverNetwork
                pure (W.Address bytes)

            Nothing -> Left errMalformedAddress

      where
        fromByronNetworkMagic :: Byron.NetworkMagic -> SL.Network
        fromByronNetworkMagic = \case
            Byron.NetworkMainOrStage -> SL.Mainnet
            Byron.NetworkTestnet{}   -> SL.Testnet

-- FIXME: 'cardano-addresses' currently gives us an opaque 'Value'. It'd be
-- nicer to model this as a proper Haskell type and to serialize in due times.
inspectAddress
    :: Text
    -> Either TextDecodingError Aeson.Value
inspectAddress =
    decodeBytes >=> inspect
  where
    inspect :: ByteString -> Either TextDecodingError Aeson.Value
    inspect = maybe (Left errMalformedAddress) Right
        . CA.inspectAddress mRootPub
        . unsafeMkAddress
    -- TODO: It's possible to inspect a byron address, given a root XPub.
    -- However, this is not yet exposed by the API.
    mRootPub = Nothing

toHDPayloadAddress :: W.Address -> Maybe Byron.HDAddressPayload
toHDPayloadAddress (W.Address addr) = do
    payload <- CBOR.deserialiseCbor CBOR.decodeAddressPayload addr
    attributes <- CBOR.deserialiseCbor decodeAllAttributes' payload
    case filter (\(tag,_) -> tag == 1) attributes of
        [(1, bytes)] ->
            Byron.HDAddressPayload <$> CBOR.decodeNestedBytes CBOR.decodeBytes bytes
        _ ->
            Nothing
  where
    decodeAllAttributes' = do
        _ <- CBOR.decodeListLenCanonicalOf 3
        _ <- CBOR.decodeBytes
        CBOR.decodeAllAttributes

guardNetwork :: SL.Network -> SL.Network -> Either TextDecodingError ()
guardNetwork addrNetwork serverNetwork =
    when (addrNetwork /= serverNetwork) $
        Left $ TextDecodingError $
            "Invalid network discrimination on address. Expecting "
            <> show serverNetwork
            <> " but got "
            <> show addrNetwork
            <> "."


encodeStakeAddress :: SNetworkId n -> W.RewardAccount -> Text
encodeStakeAddress = shelleyEncodeStakeAddress . sToNetwork

decodeStakeAddress ::
    SNetworkId n -> Text -> Either TextDecodingError W.RewardAccount
decodeStakeAddress = shelleyDecodeStakeAddress . sToNetwork

encodeAddress :: SNetworkId n -> W.Address -> Text
encodeAddress = shelleyEncodeAddress . sToNetwork

decodeAddress :: SNetworkId n -> Text -> Either TextDecodingError W.Address
decodeAddress = shelleyDecodeAddress . sToNetwork

sToNetwork :: SNetworkId n -> SL.Network
sToNetwork = \case
    SMainnet -> SL.Mainnet
    STestnet _ -> SL.Testnet
