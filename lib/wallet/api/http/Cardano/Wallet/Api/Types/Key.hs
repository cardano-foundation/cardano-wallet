{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: © 2018-2022 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Api.Types.Key
    ( ApiAccountKey (..)
    , ApiAccountKeyShared (..)
    , ApiPolicyKey (..)
    , ApiVerificationKeyShared (..)
    , ApiVerificationKeyShelley (..)
    , KeyFormat (..)
    , VerificationKeyHashing (..)
    , computeKeyPayload
    , parseBech32
    )
where

import Prelude

import Cardano.Address.Derivation
    ( XPub
    , xpubPublicKey
    )
import Cardano.Wallet.Address.Derivation
    ( Depth (..)
    , DerivationType (..)
    , Index (..)
    , Role (..)
    )
import Cardano.Wallet.Address.Derivation.SharedKey
    ( purposeCIP1854
    )
import Cardano.Wallet.Address.Discovery.Sequential
    ( purposeCIP1852
    )
import Cardano.Wallet.Api.Lib.Options
    ( DefaultSum (..)
    )
import Codec.Binary.Bech32
    ( dataPartFromBytes
    , dataPartToBytes
    )
import Codec.Binary.Bech32.TH
    ( humanReadablePart
    )
import Control.DeepSeq
    ( NFData
    )
import Crypto.Hash.Utils
    ( blake2b224
    )
import Data.Aeson.Types
    ( FromJSON (..)
    , ToJSON (..)
    )
import Data.ByteString
    ( ByteString
    )
import Data.String
    ( IsString
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( FromText (..)
    , TextDecodingError (TextDecodingError)
    , ToText (..)
    )
import GHC.Generics
    ( Generic
    )
import Servant.API
    ( ToHttpApiData
    )
import Web.Internal.HttpApiData
    ( ToHttpApiData (..)
    )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.Text as T

parseBech32
    :: Text
    -> Text
    -> Aeson.Parser (Bech32.HumanReadablePart, ByteString)
parseBech32 err =
    either (const $ fail errBech32) parseDataPart . Bech32.decodeLenient
  where
    errBech32 =
        T.unpack err <> ". Expected a bech32-encoded key."

parseDataPart
    :: (Bech32.HumanReadablePart, Bech32.DataPart)
    -> Aeson.Parser (Bech32.HumanReadablePart, ByteString)
parseDataPart =
    maybe (fail errDataPart) pure . traverse dataPartToBytes
  where
    errDataPart =
        "Couldn't decode data-part to valid UTF-8 bytes."

parsePubVer :: (MonadFail f) => ByteString -> f ByteString
parsePubVer bytes
    | BS.length bytes == 32 =
        pure bytes
    | otherwise =
        fail "Not a valid Ed25519 public key. Must be 32 bytes, without chain code"

parsePubVerHash :: (MonadFail f) => ByteString -> f ByteString
parsePubVerHash bytes
    | BS.length bytes == 28 =
        pure bytes
    | otherwise =
        fail "Not a valid hash of Ed25519 public key. Must be 28 bytes."

parsePubErr :: (IsString p) => KeyFormat -> p
parsePubErr = \case
    Extended ->
        "Not a valid Ed25519 extended public key. Must be 64 bytes, with chain code"
    NonExtended ->
        "Not a valid Ed25519 normal public key. Must be 32 bytes, without chain code"

parsePub :: (MonadFail f) => ByteString -> KeyFormat -> f ByteString
parsePub bytes extd
    | BS.length bytes == bytesExpectedLength =
        pure bytes
    | otherwise =
        fail $ parsePubErr extd
  where
    bytesExpectedLength = case extd of
        Extended -> 64
        NonExtended -> 32

computeKeyPayload :: Maybe Bool -> XPub -> (ByteString, VerificationKeyHashing)
computeKeyPayload hashed' k = case hashing of
    WithoutHashing -> (xpubPublicKey k, WithoutHashing)
    WithHashing -> (blake2b224 $ xpubPublicKey k, WithHashing)
  where
    hashing = case hashed' of
        Nothing -> WithoutHashing
        Just v -> if v then WithHashing else WithoutHashing

-- ApiPolicyKey

data VerificationKeyHashing = WithHashing | WithoutHashing
    deriving (Eq, Generic, Show)
    deriving anyclass (NFData)

data ApiPolicyKey = ApiPolicyKey
    { getApiPolicyKey :: ByteString
    , hashed :: VerificationKeyHashing
    }
    deriving (Eq, Generic, Show)
    deriving anyclass (NFData)

instance ToJSON ApiPolicyKey where
    toJSON (ApiPolicyKey pub hashed') =
        toJSON $ Bech32.encodeLenient hrp $ dataPartFromBytes pub
      where
        hrp = case hashed' of
            WithHashing -> [humanReadablePart|policy_vkh|]
            WithoutHashing -> [humanReadablePart|policy_vk|]

instance FromJSON ApiPolicyKey where
    parseJSON value = do
        (hrp, bytes) <- parseJSON value >>= (parseBech32 "Malformed policy key")
        hashing <- parseHashing hrp
        payload <- case hashing of
            WithoutHashing -> parsePubVer bytes
            WithHashing -> parsePubVerHash bytes
        pure $ ApiPolicyKey payload hashing
      where
        parseHashing = \case
            hrp | hrp == [humanReadablePart|policy_vk|] -> pure WithoutHashing
            hrp | hrp == [humanReadablePart|policy_vkh|] -> pure WithHashing
            _ -> fail errRole
          where
            errRole =
                "Unrecognized human-readable part. Expected either\
                \ \"policy_vkh\" or \"policy_vk\"."

-- ApiVerificationKeyShared

data ApiVerificationKeyShared = ApiVerificationKeyShared
    { getApiVerificationKey :: (ByteString, Role)
    , hashed :: VerificationKeyHashing
    }
    deriving (Eq, Generic, Show)
    deriving anyclass (NFData)

instance ToJSON ApiVerificationKeyShared where
    toJSON (ApiVerificationKeyShared (pub, role_) hashed') =
        toJSON $ Bech32.encodeLenient hrp $ dataPartFromBytes pub
      where
        hrp = case role_ of
            UtxoExternal -> case hashed' of
                WithHashing -> [humanReadablePart|addr_shared_vkh|]
                WithoutHashing -> [humanReadablePart|addr_shared_vk|]
            UtxoInternal -> case hashed' of
                WithHashing -> [humanReadablePart|addr_shared_vkh|]
                WithoutHashing -> [humanReadablePart|addr_shared_vk|]
            MutableAccount -> case hashed' of
                WithHashing -> [humanReadablePart|stake_shared_vkh|]
                WithoutHashing -> [humanReadablePart|stake_shared_vk|]

instance FromJSON ApiVerificationKeyShared where
    parseJSON value = do
        (hrp, bytes) <- parseJSON value >>= (parseBech32 "Malformed verification key")
        (role, hashing) <- parseRoleHashing hrp
        payload <- case hashing of
            WithoutHashing -> parsePubVer bytes
            WithHashing -> parsePubVerHash bytes
        pure $ ApiVerificationKeyShared (payload, role) hashing
      where
        parseRoleHashing = \case
            hrp | hrp == [humanReadablePart|addr_shared_vk|] -> pure (UtxoExternal, WithoutHashing)
            hrp | hrp == [humanReadablePart|stake_shared_vk|] -> pure (MutableAccount, WithoutHashing)
            hrp | hrp == [humanReadablePart|addr_shared_vkh|] -> pure (UtxoExternal, WithHashing)
            hrp | hrp == [humanReadablePart|stake_shared_vkh|] -> pure (MutableAccount, WithHashing)
            _ -> fail errRole
          where
            errRole =
                "Unrecognized human-readable part. Expected one of:\
                \ \"addr_shared_vkh\", \"stake_shared_vkh\",\"addr_shared_vk\" or \"stake_shared_vk\"."

-- ApiAccountKey

data ApiAccountKey = ApiAccountKey
    { getApiAccountKey :: ByteString
    , format :: KeyFormat
    , purpose :: Index 'Hardened 'PurposeK
    }
    deriving (Eq, Generic, Show)
    deriving anyclass (NFData)

instance ToJSON ApiAccountKey where
    toJSON (ApiAccountKey pub extd purpose') =
        toJSON $ Bech32.encodeLenient (hrp purpose') $ dataPartFromBytes pub
      where
        hrp p
            | p == purposeCIP1854 = case extd of
                Extended -> [humanReadablePart|acct_shared_xvk|]
                NonExtended -> [humanReadablePart|acct_shared_vk|]
            | otherwise = case extd of
                Extended -> [humanReadablePart|acct_xvk|]
                NonExtended -> [humanReadablePart|acct_vk|]

instance FromJSON ApiAccountKey where
    parseJSON value = do
        (hrp, bytes) <- parseJSON value >>= (parseBech32 "Malformed extended/normal account public key")
        (extended', purpose') <- parseHrp hrp
        pub <- parsePub bytes extended'
        pure $ ApiAccountKey pub extended' purpose'
      where
        parseHrp = \case
            hrp | hrp == [humanReadablePart|acct_xvk|] -> pure (Extended, purposeCIP1852)
            hrp | hrp == [humanReadablePart|acct_vk|] -> pure (NonExtended, purposeCIP1852)
            hrp | hrp == [humanReadablePart|acct_shared_xvk|] -> pure (Extended, purposeCIP1854)
            hrp | hrp == [humanReadablePart|acct_shared_vk|] -> pure (NonExtended, purposeCIP1854)
            _ -> fail errHrp
          where
            errHrp =
                "Unrecognized human-readable part. Expected one of:\
                \ \"acct_xvk\", \"acct_vk\", \"acct_shared_xvk\" or \"acct_shared_vk\"."

-- KeyFormat

data KeyFormat = Extended | NonExtended
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultSum KeyFormat
    deriving anyclass (NFData)

instance ToText KeyFormat where
    toText Extended = "extended"
    toText NonExtended = "non_extended"

instance ToHttpApiData KeyFormat where
    toUrlPiece = toText

instance FromText KeyFormat where
    fromText txt = case txt of
        "extended" -> Right Extended
        "non_extended" -> Right NonExtended
        _ ->
            Left
                $ TextDecodingError
                $ unwords
                    [ "I couldn't parse the given key format."
                    , "I am expecting one of the words 'extended' or"
                    , "'non_extended'."
                    ]

-- ApiAccountKeyShared

data ApiAccountKeyShared = ApiAccountKeyShared
    { getApiAccountKey :: ByteString
    , format :: KeyFormat
    , purpose :: Index 'Hardened 'PurposeK
    }
    deriving (Eq, Generic, Show)
    deriving anyclass (NFData)

instance ToJSON ApiAccountKeyShared where
    toJSON (ApiAccountKeyShared pub extd _) =
        toJSON $ Bech32.encodeLenient hrp $ dataPartFromBytes pub
      where
        hrp = case extd of
            Extended -> [humanReadablePart|acct_shared_xvk|]
            NonExtended -> [humanReadablePart|acct_shared_vk|]

instance FromJSON ApiAccountKeyShared where
    parseJSON value = do
        (hrp, bytes) <- parseJSON value >>= (parseBech32 "Malformed extended/normal account public key")
        extended' <- parseHrp hrp
        pub <- parsePub bytes extended'
        pure $ ApiAccountKeyShared pub extended' purposeCIP1854
      where
        parseHrp = \case
            hrp | hrp == [humanReadablePart|acct_shared_xvk|] -> pure Extended
            hrp | hrp == [humanReadablePart|acct_shared_vk|] -> pure NonExtended
            _ -> fail errHrp
          where
            errHrp =
                "Unrecognized human-readable part. Expected one of:\
                \ \"acct_shared_xvk\" or \"acct_shared_vk\"."

-- ApiVerificationKetShelley

data ApiVerificationKeyShelley = ApiVerificationKeyShelley
    { getApiVerificationKey :: (ByteString, Role)
    , hashed :: VerificationKeyHashing
    }
    deriving (Eq, Generic, Show)
    deriving anyclass (NFData)

instance ToJSON ApiVerificationKeyShelley where
    toJSON (ApiVerificationKeyShelley (pub, role_) hashed') =
        toJSON $ Bech32.encodeLenient hrp $ dataPartFromBytes pub
      where
        hrp = case role_ of
            UtxoExternal -> case hashed' of
                WithHashing -> [humanReadablePart|addr_vkh|]
                WithoutHashing -> [humanReadablePart|addr_vk|]
            UtxoInternal -> case hashed' of
                WithHashing -> [humanReadablePart|addr_vkh|]
                WithoutHashing -> [humanReadablePart|addr_vk|]
            MutableAccount -> case hashed' of
                WithHashing -> [humanReadablePart|stake_vkh|]
                WithoutHashing -> [humanReadablePart|stake_vk|]

instance FromJSON ApiVerificationKeyShelley where
    parseJSON value = do
        (hrp, bytes) <- parseJSON value >>= (parseBech32 "Malformed verification key")
        (role, hashing) <- parseRoleHashing hrp
        payload <- case hashing of
            WithoutHashing -> parsePubVer bytes
            WithHashing -> parsePubVerHash bytes
        pure $ ApiVerificationKeyShelley (payload, role) hashing
      where
        parseRoleHashing = \case
            hrp | hrp == [humanReadablePart|addr_vk|] -> pure (UtxoExternal, WithoutHashing)
            hrp | hrp == [humanReadablePart|stake_vk|] -> pure (MutableAccount, WithoutHashing)
            hrp | hrp == [humanReadablePart|addr_vkh|] -> pure (UtxoExternal, WithHashing)
            hrp | hrp == [humanReadablePart|stake_vkh|] -> pure (MutableAccount, WithHashing)
            _ -> fail errRole
          where
            errRole =
                "Unrecognized human-readable part. Expected one of:\
                \ \"addr_vkh\", \"stake_vkh\",\"addr_vk\" or \"stake_vk\"."
