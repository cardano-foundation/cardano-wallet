{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: © 2018-2022 IOHK
-- License: Apache-2.0

module Cardano.Wallet.Api.Types.Primitive () where

import Prelude

import Cardano.Api
    ( TxMetadataJsonSchema (..)
    , displayError
    , metadataFromJson
    , metadataToJson
    )
import Cardano.Wallet.Api.Aeson
    ( eitherToParser )
import Cardano.Wallet.Api.Hex
    ( fromHexText, hexText )
import Cardano.Wallet.Api.Lib.ApiT
    ( ApiT (..), fromTextApiT, toTextApiT )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DerivationIndex, RewardAccount )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    )
import Cardano.Wallet.Primitive.Types
    ( EpochNo (..)
    , NonWalletCertificate (..)
    , PoolId (..)
    , SlotInEpoch (..)
    , SlotNo (..)
    , decodePoolIdBech32
    , encodePoolIdBech32
    , unsafeEpochNo
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..), coinFromQuantity, coinToQuantity )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( coinIsValidForTxOut, txOutMaxCoin )
import Cardano.Wallet.Primitive.Types.Tx.Tx
    ( TxIn (..), TxMetadata (..), TxScriptValidity )
import Cardano.Wallet.Shelley.Network.Discriminant
    ( DecodeAddress (..)
    , DecodeStakeAddress (..)
    , EncodeAddress (..)
    , EncodeStakeAddress (..)
    )
import Cardano.Wallet.Transaction
    ( AnyScript (..) )
import Cardano.Wallet.Util
    ( ShowFmt (..) )
import Control.Monad
    ( (>=>) )
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
    )
import Data.Aeson.Types
    ( prependFailure )
import Data.Bifunctor
    ( Bifunctor (..) )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( ToText (..) )
import Data.Word
    ( Word32, Word64 )
import Data.Word.Odd
    ( Word31 )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as W
import qualified Cardano.Wallet.Primitive.Types.TokenMap as W
import qualified Cardano.Wallet.Primitive.Types.TokenPolicy as W
import qualified Data.Aeson.Types as Aeson

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

instance FromJSON (ApiT AnyScript) where
    parseJSON = withObject "ApiT AnyScript" $ \obj -> do
        scriptType <- obj .:? "script_type"
        case (scriptType :: Maybe String) of
            Just t | t == "plutus"  ->
                ApiT . PlutusScript <$> obj .: "language_version"
            Just t | t == "native" ->
                ApiT . NativeScript <$> obj .: "script"
            _ -> fail "AnyScript needs either 'native' or 'plutus' in 'script_type'"

instance ToJSON (ApiT AnyScript) where
    toJSON (ApiT (NativeScript s)) =
        object [ "script_type" .= String "native"
               , "script" .= toJSON s]
    toJSON (ApiT (PlutusScript v)) =
        object [ "script_type" .= String "plutus"
               , "language_version" .= toJSON v]

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
        [ "amount" .= coinToQuantity @Word c
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
        validateCoin (coinFromQuantity -> c)
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

instance {-# OVERLAPS #-} DecodeAddress n => FromJSON (ApiT Address, Proxy n)
  where
    parseJSON x = do
        let proxy = Proxy @n
        addr <- parseJSON x >>= eitherToParser
            . bimap ShowFmt ApiT
            . decodeAddress @n
        return (addr, proxy)
instance {-# OVERLAPS #-} EncodeAddress n => ToJSON (ApiT Address, Proxy n)
  where
    toJSON (addr, _) = toJSON . encodeAddress @n . getApiT $ addr

instance {-# OVERLAPS #-} (DecodeStakeAddress n)
    => FromJSON (ApiT RewardAccount, Proxy n)
  where
    parseJSON x = do
        let proxy = Proxy @n
        acct <- parseJSON x >>= eitherToParser
            . bimap ShowFmt ApiT
            . decodeStakeAddress @n
        return (acct, proxy)

instance {-# OVERLAPS #-} EncodeStakeAddress n
    => ToJSON (ApiT RewardAccount, Proxy n)
  where
    toJSON (acct, _) = toJSON . encodeStakeAddress @n . getApiT $ acct

instance FromJSON (ApiT PoolId) where
    parseJSON = parseJSON >=> eitherToParser
           . bimap ShowFmt ApiT
           . decodePoolIdBech32
instance ToJSON (ApiT PoolId) where
    toJSON = toJSON . encodePoolIdBech32 . getApiT

instance FromJSON (ApiT W.StakePoolMetadataHash) where
    parseJSON = fromTextApiT "ApiT StakePoolMetadataHash"
instance ToJSON (ApiT W.StakePoolMetadataHash) where
    toJSON = toTextApiT

instance FromJSON (ApiT W.PoolOwner) where
    parseJSON = fromTextApiT "ApiT PoolOwner"
instance ToJSON (ApiT W.PoolOwner) where
    toJSON = toTextApiT

instance FromJSON (ApiT W.StakePoolMetadataUrl) where
    parseJSON = fromTextApiT "ApiT StakePoolMetadataUrl"
instance ToJSON (ApiT W.StakePoolMetadataUrl) where
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
