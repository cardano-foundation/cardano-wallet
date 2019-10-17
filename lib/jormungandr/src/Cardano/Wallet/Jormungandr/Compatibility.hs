{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Contains various implementation decision that are specific to a particular
-- network / protocol. This allows us to easily select a particular backend
-- (Byron, Shelley-Rust, Shelley-Haskell) and isolate the bits that vary between
-- those backends.

module Cardano.Wallet.Jormungandr.Compatibility
    ( -- * Target
      Jormungandr
    , Network (..)
    , softTxMaxSize

      -- * Node's Configuration
    , BaseUrl (..)
    , Scheme (..)
    , genConfigFile
    , genConfigFileYaml
    , localhostBaseUrl
    , baseUrlToText
    ) where

import Prelude

import Cardano.Wallet.DB.Sqlite
    ( PersistTx (..) )
import Cardano.Wallet.Jormungandr.Binary
    ( decodeLegacyAddress, singleAddressFromKey )
import Cardano.Wallet.Jormungandr.Environment
    ( KnownNetwork (..), Network (..) )
import Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx (..) )
import Cardano.Wallet.Network.Ports
    ( PortNumber )
import Cardano.Wallet.Primitive.AddressDerivation
    ( KeyToAddress (..), getRawKey )
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( RndKey, derivationPath, payloadPassphrase )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( SeqKey (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , DecodeAddress (..)
    , DefineTx
    , EncodeAddress (..)
    , invariant
    )
import Codec.Binary.Bech32
    ( HumanReadablePart, dataPartFromBytes, dataPartToBytes )
import Control.Arrow
    ( second )
import Control.Monad
    ( when )
import Data.Aeson
    ( Value (..), object, (.=) )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58, encodeBase58 )
import Data.Function
    ( (&) )
import Data.List
    ( foldl' )
import Data.Maybe
    ( fromJust, isJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( TextDecodingError (..) )
import Data.Word
    ( Word16 )
import Servant.Client.Core
    ( BaseUrl (..), Scheme (..), showBaseUrl )
import System.FilePath
    ( FilePath, (</>) )

import qualified Cardano.Byron.Codec.Cbor as CBOR
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.CBOR.Write as CBOR
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Yaml

-- | A type representing the Jormungandr as a network target. This has an
-- influence on binary serializer & network primitives. See also 'TxId'
data Jormungandr (network :: Network)

-- | Jörmugandr's chain parameter doesn't include a transaction max size. The
-- actual hard-limit for the size is constrained by the binary format and
-- numbers used to represent the number of inputs and outputs (Word8), yet
-- there's also a soft-limit of 8kb which results in much smaller transactions
-- in the end.
softTxMaxSize :: Quantity "byte" Word16
softTxMaxSize = Quantity 8192

instance DefineTx (Jormungandr network) where
    type Tx (Jormungandr network) = Tx
    inputs = fmap fst . inputs
    outputs = outputs
    -- The corresponding rust implementation is:
    -- https://github.com/input-output-hk/rust-cardano/blob/e5d974f7bedeb00c9c9d688ac66094a34bf8f40d/chain-impl-mockchain/src/transaction/transaction.rs#L115-L119
    txId = txid

instance PersistTx (Jormungandr network) where
    resolvedInputs = map (second Just) . inputs
    mkTx tid inps = Tx tid ((second unsafeFromMaybe) <$> inps)
      where
        unsafeFromMaybe amt = fromJust $ invariant
            ("PersistTx (Jormungandr network): invariant violation, tried to \
            \reconstruct a 'Tx' from the database that has resolved inputs \
            \without any amount: " <> show inps)
            amt
            isJust

instance KeyToAddress (Jormungandr n) RndKey where
    keyToAddress k = Address
        $ CBOR.toStrictByteString
        $ CBOR.encodeAddress (getRawKey k)
            [ CBOR.encodeDerivationPathAttr pwd acctIx addrIx ]
            -- Note that we do NOT include a protocol magic attribute here,
            -- as we do not discriminate between Testnet and Mainnet: we'll
            -- be using Byron Mainnet addresses on Jormungndr Testnet.
      where
        (acctIx, addrIx) = derivationPath k
        pwd = payloadPassphrase k

instance forall n. KnownNetwork n => KeyToAddress (Jormungandr n) SeqKey where
    keyToAddress key = singleAddressFromKey (Proxy @n) (getKey key)

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
                "Unable to decode Address: encoding is neither Bech32 nor \
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
                "Unable to decode Address: neither Bech32-encoded nor a valid \
                \Byron Address."

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
                "This Address belongs to another network. Network is: "
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
                        "Invalid Address length (" <> show (BS.length bytes)
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
                $ "Invalid Address first byte: "
                <> B8.unpack (BS.take 1 bytes)
                <> " =/= "
                <> B8.unpack (BS.pack [discriminant])
                <> "."

-- | Generate a configuration file for Jörmungandr@0.6
-- Will throw "YamlException" or "IOException" when files cannot be
-- read/written.
genConfigFileYaml
    :: FilePath
    -- ^ State directory
    -> PortNumber
    -- ^ P2P port
    -> BaseUrl
    -- ^ Rest API base URL
    -> Maybe FilePath
    -- ^ User's config file
    -> IO FilePath
genConfigFileYaml stateDir addressPort restApiUrl baseConfig = do
    let nodeConfigFile = stateDir </> "jormungandr-config.yaml"
    base <- maybe (pure Aeson.Null) Yaml.decodeFileThrow baseConfig
    genConfigFile stateDir addressPort restApiUrl base
        & Yaml.encodeFile nodeConfigFile
    pure nodeConfigFile

-- | Create a Jormungandr config by first making some defaults, then adding the
-- user's config file (if provided), then setting the API port to the value that
-- we have chosen.
genConfigFile
    :: FilePath
    -> PortNumber
    -> BaseUrl
    -> Aeson.Value
    -> Aeson.Value
genConfigFile stateDir addressPort (BaseUrl _ host port _) user =
    mergeObjects [defaults, user, override]
  where
    defaults = object
        [ "storage" .= (stateDir </> "chain")
        , "p2p" .= object
            [ "trusted_peers" .= ([] :: [()])
            , "topics_of_interest" .= object
                [ "messages" .= String "low"
                , "blocks" .= String "normal"
                ]
            , "public_address" .= String publicAddress
            ]
        ]
    override = object
        [ "rest" .= object
            [ "listen" .= String listen ]
        ]
    listen = T.pack $ mconcat [host, ":", show port]
    publicAddress = T.pack $ mconcat ["/ip4/127.0.0.1/tcp/", show addressPort]

-- | Recursively merge JSON objects, with the rightmost values taking
-- precedence.
mergeObjects :: [Aeson.Value] -> Aeson.Value
mergeObjects = foldl' merge Aeson.Null
  where
    merge :: Aeson.Value -> Aeson.Value -> Aeson.Value
    merge (Aeson.Object a) (Aeson.Object b) = Aeson.Object $
        HM.unionWith merge a b
    merge a Aeson.Null = a
    merge _ b = b

{-------------------------------------------------------------------------------
                                     Base URL
-------------------------------------------------------------------------------}

localhostBaseUrl :: Int -> BaseUrl
localhostBaseUrl port = BaseUrl Http "127.0.0.1" port ""

-- | Format an API 'BaseUrl', for logging, etc.
baseUrlToText :: BaseUrl -> T.Text
baseUrlToText = T.pack . showBaseUrl
