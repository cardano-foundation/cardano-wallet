{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Wallet.Primitive.Types.TokenPolicy
    (
      -- * Token Policies
      TokenPolicyId (..)

      -- * Token Names
    , TokenName (..)
    , mkTokenName
    , nullTokenName
    , maxLengthTokenName

      -- * Token Fingerprints
    , TokenFingerprint (..)
    , mkTokenFingerprint

      -- * Token Metadata
    , AssetMetadata (..)
    , AssetURL (..)
    , AssetLogo (..)
    , AssetDecimals (..)
    , validateMetadataDecimals
    , validateMetadataName
    , validateMetadataTicker
    , validateMetadataDescription
    , validateMetadataURL
    , validateMetadataLogo
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( (>=>) )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_160 )
import Data.Aeson
    ( FromJSON (..), ToJSON (..) )
import Data.Bifunctor
    ( first )
import Data.ByteArray
    ( convert )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Function
    ( (&) )
import Data.Hashable
    ( Hashable )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Fmt
    ( Buildable (..) )
import GHC.Generics
    ( Generic )
import Network.URI
    ( URI, parseAbsoluteURI, uriScheme )
import Quiet
    ( Quiet (..) )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Token policy identifiers, represented by the hash of the monetary policy
-- script.
newtype TokenPolicyId =
    -- | Construct a 'TokenPolicyId' without any validation.
    UnsafeTokenPolicyId { unTokenPolicyId :: Hash "TokenPolicy" }
    deriving stock (Eq, Ord, Generic)
    deriving (Read, Show) via (Quiet TokenPolicyId)
    deriving anyclass Hashable

instance NFData TokenPolicyId

instance Buildable TokenPolicyId where
    build = build . toText . unTokenPolicyId

instance FromJSON TokenPolicyId where
    parseJSON = parseJSON >=> either (fail . show) pure . fromText

instance ToJSON TokenPolicyId where
    toJSON = toJSON . toText

instance ToText TokenPolicyId where
    toText = toText . unTokenPolicyId

instance FromText TokenPolicyId where
    fromText = fmap UnsafeTokenPolicyId . fromText

-- | Token names, defined by the monetary policy script.
newtype TokenName =
    -- | Construct a 'TokenName' without any validation.
    UnsafeTokenName { unTokenName :: ByteString }
    deriving stock (Eq, Ord, Generic)
    deriving (Read, Show) via (Quiet TokenName)
    deriving anyclass Hashable

-- | Construct a 'TokenName', validating that the length does not exceed
-- 'maxLengthTokenName'.
mkTokenName :: ByteString -> Either String TokenName
mkTokenName bs
    | BS.length bs <= maxLengthTokenName = Right $ UnsafeTokenName bs
    | otherwise = Left $ "TokenName length " ++ show (BS.length bs)
        ++ " exceeds maximum of " ++ show maxLengthTokenName

-- | The empty asset name.
--
-- Asset names may be empty, where a monetary policy script only mints a single
-- asset, or where one asset should be considered as the "default" token for the
-- policy.
nullTokenName :: TokenName
nullTokenName = UnsafeTokenName ""

maxLengthTokenName :: Int
maxLengthTokenName = 32

instance NFData TokenName

instance Buildable TokenName where
    build = build . toText

instance FromJSON TokenName where
    parseJSON = parseJSON >=> either (fail . show) pure . fromText

instance ToJSON TokenName where
    toJSON = toJSON . toText

instance ToText TokenName where
    toText = T.decodeLatin1 . convertToBase Base16 . unTokenName

instance FromText TokenName where
    fromText = first TextDecodingError
        . either (Left . ("TokenName is not hex-encoded: " ++)) mkTokenName
        . convertFromBase Base16
        . T.encodeUtf8

newtype TokenFingerprint =
    UnsafeTokenFingerprint { unTokenFingerprint :: Text }
    deriving stock (Eq, Ord, Generic)
    deriving (Read, Show) via (Quiet TokenFingerprint)
    deriving anyclass Hashable

instance NFData TokenFingerprint

-- | Construct a fingerprint from a 'TokenPolicyId' and 'TokenName'. The
-- fingerprint is not necessarily unique, but can be used in user-facing
-- interfaces as a comparison mechanism.
mkTokenFingerprint :: TokenPolicyId -> TokenName -> TokenFingerprint
mkTokenFingerprint (UnsafeTokenPolicyId (Hash p)) (UnsafeTokenName n)
    = (p <> n)
    & convert . hash @_ @Blake2b_160
    & Bech32.encodeLenient tokenFingerprintHrp . Bech32.dataPartFromBytes
    & UnsafeTokenFingerprint

tokenFingerprintHrp :: Bech32.HumanReadablePart
tokenFingerprintHrp = [humanReadablePart|asset|]

instance ToText TokenFingerprint where
    toText = unTokenFingerprint

instance FromText TokenFingerprint where
    fromText txt = case Bech32.decodeLenient txt of
        Left{} -> Left invalidBech32String
        Right (hrp, dp)
            | hrp /= tokenFingerprintHrp -> Left unrecognizedHrp
            | otherwise -> case BS.length <$> Bech32.dataPartToBytes dp of
                Just 20 -> Right (UnsafeTokenFingerprint txt)
                _ -> Left invalidDatapart
      where
        invalidBech32String = TextDecodingError
            "A 'TokenFingerprint' must be a valid bech32-encoded string."
        unrecognizedHrp = TextDecodingError
            "Expected 'asset' as a human-readable part, but got something else."
        invalidDatapart = TextDecodingError
            "Expected a Blake2b-160 digest as data payload, but got something else."

-- | Information about an asset, from a source external to the chain.
data AssetMetadata = AssetMetadata
    { name :: Text
    , description :: Text
    , ticker :: Maybe Text
    , url :: Maybe AssetURL
    , logo :: Maybe AssetLogo
    , decimals :: Maybe AssetDecimals
    } deriving stock (Eq, Ord, Generic)
    deriving (Show) via (Quiet AssetMetadata)

instance NFData AssetMetadata

-- | Specify an asset logo as an image data payload
newtype AssetLogo = AssetLogo
    { unAssetLogo :: ByteString
    } deriving (Eq, Ord, Generic)
    deriving (Show) via (Quiet AssetLogo)

instance NFData AssetLogo

-- | The validated URL for the asset.
newtype AssetURL = AssetURL
    { unAssetURL :: URI
    } deriving (Eq, Ord, Generic)
    deriving (Show) via (Quiet AssetURL)

instance NFData AssetURL

instance ToText AssetURL where
    toText = T.pack . show . unAssetURL

instance FromText AssetURL where
    fromText = first TextDecodingError . validateMetadataURL

newtype AssetDecimals = AssetDecimals
  { unAssetDecimals :: Int
  } deriving (Eq, Ord, Generic)
  deriving (Show) via (Quiet AssetDecimals)

instance NFData AssetDecimals

instance ToText AssetDecimals where
  toText = T.pack . show . unAssetDecimals

instance FromText AssetDecimals where
  fromText t = do
    unvalidated <- AssetDecimals <$> fromText t
    first TextDecodingError $ validateMetadataDecimals unvalidated

validateMinLength :: Int -> Text -> Either String Text
validateMinLength n text
    | len >= n = Right text
    | otherwise = Left $ "Length must be at least " ++ show n ++ " characters, got " ++ show len
  where
    len = T.length text

validateMaxLength :: Int -> Text -> Either String Text
validateMaxLength n text
    | len <= n = Right text
    | otherwise = Left $ "Length must be no more than " ++ show n ++ " characters, got " ++ show len
  where
    len = T.length text

validateMetadataName :: Text -> Either String Text
validateMetadataName = validateMinLength 1 >=> validateMaxLength 50

validateMetadataTicker :: Text -> Either String Text
validateMetadataTicker = validateMinLength 2 >=> validateMaxLength 6

validateMetadataDescription :: Text -> Either String Text
validateMetadataDescription = validateMaxLength 500

validateMetadataURL :: Text -> Either String AssetURL
validateMetadataURL = fmap AssetURL .
    (validateMaxLength 250 >=> validateURI >=> validateHttps)
  where
      validateURI = maybe (Left "Not an absolute URI") Right
          . parseAbsoluteURI
          . T.unpack
      validateHttps u@(uriScheme -> scheme)
          | scheme == "https:" = Right u
          | otherwise = Left $ "Scheme must be https: but got " ++ scheme

validateMetadataLogo :: AssetLogo -> Either String AssetLogo
validateMetadataLogo logo
    | len <= maxLen = Right logo
    | otherwise = Left $ "Length must be no more than " ++ show maxLen ++ " bytes, got " ++ show len
  where
    len = BS.length $ unAssetLogo logo
    maxLen = 65536

validateMetadataDecimals :: AssetDecimals -> Either String AssetDecimals
validateMetadataDecimals (AssetDecimals n)
  | n >= 0 && n <= 255 = Right $ AssetDecimals n
  | otherwise          = Left "Decimal value must be between [0, 255] inclusive."
