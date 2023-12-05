{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: © 2018-2022 IOHK, 2023 Cardano Foundation
-- License: Apache-2.0

module Cardano.Wallet.Api.Types.Primitive () where

import Prelude

import Cardano.Address.Script
    ( ScriptHash (..)
    )
import Cardano.Api
    ( TxMetadataJsonSchema (..)
    , displayError
    , metadataFromJson
    , metadataToJson
    )
import Cardano.Pool.Metadata.Types
    ( StakePoolMetadataHash
    , StakePoolMetadataUrl
    )
import Cardano.Pool.Types
    ( PoolId
    , PoolOwner
    , decodePoolIdBech32
    , encodePoolIdBech32
    )
import Cardano.Wallet.Address.Derivation
    ( DerivationIndex
    )
import Cardano.Wallet.Api.Aeson
    ( eitherToParser
    )
import Cardano.Wallet.Api.Hex
    ( fromHexText
    , hexText
    )
import Cardano.Wallet.Api.Lib.ApiT
    ( ApiT (..)
    , fromTextApiT
    , toTextApiT
    )
import Cardano.Wallet.Api.Types.Key
    ( parseBech32
    )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    )
import Cardano.Wallet.Primitive.Types
    ( EpochNo (..)
    , NonWalletCertificate (..)
    , SlotInEpoch (..)
    , SlotNo (..)
    , unsafeEpochNo
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( coinIsValidForTxOut
    , txOutMaxCoin
    )
import Cardano.Wallet.Primitive.Types.Tx.Tx
    ( TxMetadata (..)
    , TxScriptValidity
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..)
    )
import Cardano.Wallet.Transaction
    ( AnyExplicitScript (..)
    , AnyScript (..)
    , PlutusScriptInfo (..)
    , ReferenceInput (..)
    , ScriptReference (..)
    )
import Cardano.Wallet.Util
    ( ShowFmt (..)
    )
import Codec.Binary.Bech32
    ( dataPartFromBytes
    )
import Codec.Binary.Bech32.TH
    ( humanReadablePart
    )
import Control.Monad
    ( when
    , (>=>)
    )
import Data.Aeson
    ( FromJSON (parseJSON)
    , KeyValue (..)
    , Options (..)
    , ToJSON (toJSON)
    , Value (..)
    , camelTo2
    , genericParseJSON
    , genericToJSON
    , object
    , withObject
    , withText
    , (.!=)
    , (.:)
    , (.:?)
    , (.=)
    )
import Data.Aeson.Types
    ( prependFailure
    )
import Data.Bifunctor
    ( Bifunctor (..)
    )
import Data.Quantity
    ( Quantity (..)
    )
import Data.Text.Class
    ( FromText (..)
    , TextDecodingError (..)
    , ToText (..)
    )
import Data.Word
    ( Word32
    , Word64
    )
import Data.Word.Odd
    ( Word31
    )
import Numeric.Natural
    ( Natural
    )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as W
import qualified Cardano.Wallet.Primitive.Types.TokenFingerprint as W
import qualified Cardano.Wallet.Primitive.Types.TokenMap as W
import qualified Cardano.Wallet.Primitive.Types.TokenName as W
import qualified Cardano.Wallet.Primitive.Types.TokenPolicy as W
import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS

instance ToJSON (ApiT DerivationIndex) where
    toJSON = toTextApiT
instance FromJSON (ApiT DerivationIndex) where
    parseJSON = fromTextApiT "DerivationIndex"

instance (PassphraseMaxLength purpose, PassphraseMinLength purpose)
    => FromJSON (ApiT (Passphrase purpose)) where
    parseJSON = fromTextApiT "Passphrase"
instance ToJSON (ApiT (Passphrase purpose)) where
    toJSON = toTextApiT

instance FromJSON (ApiT W.TokenPolicyId) where
    parseJSON = fromTextApiT "PolicyId"
instance ToJSON (ApiT W.TokenPolicyId) where
    toJSON = toTextApiT

instance FromJSON (ApiT PlutusScriptInfo) where
    parseJSON = (fmap. fmap) ApiT . withObject "PlutusScriptInfo" $ \obj ->
        (obj .: "script_hash") >>= (\case
           Left str -> fail
               $ "PlutusScriptInfo: script_hash should be hex-encoded \
                 \56-character string, but got " ++ str
           Right hash -> do
               ver <- obj .: "language_version"
               case fromText ver of
                   Left (TextDecodingError err)
                       -> fail $ "PlutusScriptInfo: language_version " ++ err
                   Right plutusVersion
                       -> pure $ PlutusScriptInfo plutusVersion (ScriptHash hash))
        . fromHexText

instance ToJSON (ApiT PlutusScriptInfo) where
    toJSON (ApiT (PlutusScriptInfo v (ScriptHash h))) =
        object
            [ "script_hash" .= String (hexText h)
            , "language_version" .= String (toText v)
            ]

instance ToJSON ReferenceInput where
    toJSON (ReferenceInput (TxIn txId ix)) =
        object
        [ "id" .=toJSON (ApiT txId)
        , "index" .= toJSON ix
        ]

instance FromJSON ReferenceInput where
    parseJSON = withObject "ReferenceInput" $ \v -> ReferenceInput <$>
        (TxIn <$> fmap getApiT (v .: "id") <*> v .: "index")

instance FromJSON (ApiT AnyScript) where
    parseJSON = (fmap . fmap) ApiT . withObject "AnyScript" $ \obj -> do
        scriptType <- obj .:? "script_type"
        reference <- obj .:? "reference"
        case (scriptType :: Maybe String, reference :: Maybe ReferenceInput) of
            (Just t , Nothing) -> case t of
                "plutus" ->
                    flip PlutusScript ViaSpending . getApiT <$> obj .: "script_info"
                "native" ->
                    flip NativeScript ViaSpending <$> obj .: "script"
                "reference script" -> do
                    scriptH <- (obj .: "script_hash") >>= (\case
                               Left str -> fail $
                                   "AnyScript: script_hash should be hex-encoded \
                                   \56-character string, but got " ++ str
                               Right hash -> pure $ ScriptHash hash)
                               . fromHexText
                    AnyScriptReference scriptH <$> obj .: "references"
                _ -> fail
                    "AnyScript needs either 'native', 'plutus' or 'reference script' in 'script_type'"
            (Just t , Just ref) -> case t of
                "plutus" ->
                    flip PlutusScript (ViaReferenceInput ref) . getApiT <$>
                    obj .: "script_info"
                "native" ->
                    flip NativeScript (ViaReferenceInput ref) <$> obj .: "script"
                _ -> fail
                    "AnyScript needs either 'native' or 'plutus' in 'script_type'"
            _ -> fail
                "AnyScript needs to have 'script_type' field"

instance ToJSON (ApiT AnyScript) where
    toJSON (ApiT anyScript) = case anyScript of
        NativeScript s ViaSpending ->
            object
                [ "script_type" .= String "native"
                , "script" .= toJSON s
                ]
        PlutusScript s ViaSpending ->
            object
                [ "script_type" .= String "plutus"
                , "script_info" .= toJSON (ApiT s)
                ]
        NativeScript s (ViaReferenceInput refInput) ->
            object
                [ "script_type" .= String "native"
                , "script" .= toJSON s
                , "reference" .= toJSON refInput
                ]
        PlutusScript s (ViaReferenceInput refInput) ->
            object
                [ "script_type" .= String "plutus"
                , "script_info" .= toJSON (ApiT s)
                , "reference" .= toJSON refInput
                ]
        AnyScriptReference (ScriptHash h) refs ->
            object
                [ "script_type" .= String "reference script"
                , "script_hash" .= String (hexText h)
                , "references" .= toJSON refs
                ]

instance FromJSON (ApiT AnyExplicitScript) where
    parseJSON = (fmap . fmap) ApiT . withObject "AnyExplicitScript" $ \obj -> do
        scriptType <- obj .:? "script_type"
        reference <- obj .:? "reference"
        case (scriptType :: Maybe String, reference :: Maybe ReferenceInput) of
            (Just t , Nothing) -> case t of
                "plutus" ->
                    flip PlutusExplicitScript ViaSpending . getApiT <$> obj .: "script_info"
                "native" ->
                    flip NativeExplicitScript ViaSpending <$> obj .: "script"
                _ -> fail
                    "AnyExplicitScript needs either 'native' or 'plutus' in 'script_type'"
            (Just t , Just ref) -> case t of
                "plutus" ->
                    flip PlutusExplicitScript (ViaReferenceInput ref) . getApiT <$>
                    obj .: "script_info"
                "native" ->
                    flip NativeExplicitScript (ViaReferenceInput ref) <$> obj .: "script"
                _ -> fail
                    "AnyExplicitScript needs either 'native' or 'plutus' in 'script_type'"
            _ -> fail
                "AnyExplicitScript needs to have 'script_type' field"

instance ToJSON (ApiT AnyExplicitScript) where
    toJSON (ApiT anyScript) = case anyScript of
        NativeExplicitScript s ViaSpending ->
            object
                [ "script_type" .= String "native"
                , "script" .= toJSON s
                ]
        PlutusExplicitScript s ViaSpending ->
            object
                [ "script_type" .= String "plutus"
                , "script_info" .= toJSON (ApiT s)
                ]
        NativeExplicitScript s (ViaReferenceInput refInput) ->
            object
                [ "script_type" .= String "native"
                , "script" .= toJSON s
                , "reference" .= toJSON refInput
                ]
        PlutusExplicitScript s (ViaReferenceInput refInput) ->
            object
                [ "script_type" .= String "plutus"
                , "script_info" .= toJSON (ApiT s)
                , "reference" .= toJSON refInput
                ]

instance FromJSON (ApiT W.TokenName) where
    parseJSON = withText "AssetName"
        (fmap (ApiT . W.UnsafeTokenName) . eitherToParser . fromHexText)
instance ToJSON (ApiT W.TokenName) where
    toJSON = toJSON . hexText . W.unTokenName . getApiT

instance FromJSON (ApiT W.TokenFingerprint) where
    parseJSON = fromTextApiT "TokenFingerprint"
instance ToJSON (ApiT W.TokenFingerprint) where
    toJSON = toTextApiT

instance FromJSON (ApiT EpochNo) where
    parseJSON = fmap (ApiT . unsafeEpochNo) . parseJSON
instance ToJSON (ApiT EpochNo) where
    toJSON (ApiT (EpochNo en)) = toJSON $ fromIntegral @Word31 @Word32 en

instance FromJSON (ApiT SlotInEpoch) where
    parseJSON = fmap (ApiT . SlotInEpoch) . parseJSON
instance ToJSON (ApiT SlotInEpoch) where
    toJSON (ApiT (SlotInEpoch sn)) = toJSON sn

instance FromJSON (ApiT SlotNo) where
    parseJSON = fmap (ApiT . SlotNo) . parseJSON
instance ToJSON (ApiT SlotNo) where
    toJSON (ApiT (SlotNo sn)) = toJSON sn

instance FromJSON (ApiT W.TokenMap) where
    parseJSON = fmap (ApiT . W.getFlat) . parseJSON
instance ToJSON (ApiT W.TokenMap) where
    toJSON = toJSON . W.Flat . getApiT
instance ToJSON (ApiT W.TokenBundle) where
    -- TODO: consider other structures
    toJSON (ApiT (W.TokenBundle c ts)) = object
        [ "amount" .= Coin.toQuantity @Natural c
        , "assets" .= toJSON (ApiT ts)
        ]

instance FromJSON (ApiT W.TokenBundle) where
    -- TODO: reject unknown fields
    parseJSON = withObject "Value " $ \v ->
        prependFailure "parsing Value failed, " $
        fmap ApiT $ W.TokenBundle
            <$> (v .: "amount" >>= validateCoin)
            <*> fmap getApiT (v .: "assets" .!= mempty)
      where
        validateCoin :: Quantity "lovelace" Word64 -> Aeson.Parser Coin
        validateCoin (Coin.fromQuantity -> c)
            | coinIsValidForTxOut c = pure c
            | otherwise = fail $
                "invalid coin value: value has to be lower than or equal to "
                <> show (unCoin txOutMaxCoin) <> " lovelace."

instance FromJSON (ApiT TxIn) where
    parseJSON = withObject "TxIn" $ \v -> ApiT <$>
        (TxIn <$> fmap getApiT (v .: "id") <*> v .: "index")

instance ToJSON (ApiT TxIn) where
    toJSON (ApiT (TxIn txid ix)) = object
        [ "id" .= toJSON (ApiT txid)
        , "index" .= toJSON ix ]

instance FromJSON (ApiT (Hash "Tx")) where
    parseJSON = fromTextApiT "Tx Hash"
instance ToJSON (ApiT (Hash "Tx")) where
    toJSON = toTextApiT

instance FromJSON (ApiT TxScriptValidity) where
    parseJSON = fmap ApiT . genericParseJSON Aeson.defaultOptions
        { constructorTagModifier = camelTo2 '_' . drop 8 }

instance ToJSON (ApiT TxScriptValidity) where
    toJSON = genericToJSON Aeson.defaultOptions
        { constructorTagModifier = camelTo2 '_' . drop 8 } . getApiT

instance FromJSON (ApiT PoolId) where
    parseJSON = parseJSON >=> eitherToParser
           . bimap ShowFmt ApiT
           . decodePoolIdBech32
instance ToJSON (ApiT PoolId) where
    toJSON = toJSON . encodePoolIdBech32 . getApiT

instance FromJSON (ApiT StakePoolMetadataHash) where
    parseJSON = fromTextApiT "ApiT StakePoolMetadataHash"
instance ToJSON (ApiT StakePoolMetadataHash) where
    toJSON = toTextApiT

instance FromJSON (ApiT PoolOwner) where
    parseJSON = fromTextApiT "ApiT PoolOwner"
instance ToJSON (ApiT PoolOwner) where
    toJSON = toTextApiT

instance FromJSON (ApiT StakePoolMetadataUrl) where
    parseJSON = fromTextApiT "ApiT StakePoolMetadataUrl"
instance ToJSON (ApiT StakePoolMetadataUrl) where
    toJSON = toTextApiT

instance FromJSON (ApiT W.NonWalletCertificate) where
  parseJSON val
    | val == object ["certificate_type" .= String "mir"]
    = pure $ ApiT MIRCertificate
    | val == object ["certificate_type" .= String "genesis"]
    = pure $ ApiT GenesisCertificate
    | otherwise
    = fail
        "expected object with key 'certificate_type' and value either 'mir' or 'genesis'"
instance ToJSON (ApiT W.NonWalletCertificate) where
    toJSON (ApiT cert) = object ["certificate_type" .= String (toText cert)]

instance FromJSON (ApiT TxMetadata) where
    parseJSON = fmap ApiT
        . either (fail . displayError) pure
        . metadataFromJson TxMetadataJsonDetailedSchema

instance ToJSON (ApiT TxMetadata) where
    toJSON = metadataToJson TxMetadataJsonDetailedSchema . getApiT

instance FromJSON (ApiT (Hash "ScriptIntegrity")) where
    parseJSON value = do
        (hrp, bytes) <- parseJSON value >>=
            parseBech32 "Malformed policy key"
        when
            (hrp /= [humanReadablePart|script_data|])
            $ fail "expected a bech32 script_data hash"
        when
            (BS.length bytes /= 32)
            $ fail "expected a bech32 script_data hash of 32 bytes"
        pure $ ApiT $ Hash bytes
instance ToJSON (ApiT (Hash "ScriptIntegrity")) where
    toJSON (ApiT (Hash hashed')) =
        toJSON $ Bech32.encodeLenient
            [humanReadablePart|script_data|]
             $ dataPartFromBytes hashed'

instance FromJSON (ApiT (Hash "ExtraSignature")) where
    parseJSON value = do
        (hrp, bytes) <- parseJSON value >>=
            parseBech32 "Malformed policy key"
        when
            (hrp /= [humanReadablePart|req_signer_vkh|])
            $ fail "expected a bech32 req_signer_vkh hash"
        when
            (BS.length bytes /= 28)
            $ fail "expected a bech32 req_signer_vkh hash of 28 bytes"
        pure $ ApiT $ Hash bytes
instance ToJSON (ApiT (Hash "ExtraSignature")) where
    toJSON (ApiT (Hash hashed')) =
        toJSON $ Bech32.encodeLenient
            [humanReadablePart|req_signer_vkh|]
             $ dataPartFromBytes hashed'
