{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

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
    , AssetLogo (..)
    , AssetUnit (..)
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
import Numeric.Natural
    ( Natural )
import Quiet
    ( Quiet (..) )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.ByteString as BS
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
    , acronym :: Maybe Text
    , url :: Maybe Text
    , logo :: Maybe AssetLogo
    , unit :: Maybe AssetUnit
    } deriving stock (Eq, Ord, Generic)
    deriving (Show) via (Quiet AssetMetadata)

instance NFData AssetMetadata

-- | Specification of a larger unit for an asset. For example, the "lovelace"
-- asset has the larger unit "ada" with 6 zeroes.
data AssetUnit = AssetUnit
    { name :: Text -- ^ Name of the larger asset.
    , decimals :: Natural  -- ^ Number of zeroes to add to base unit.
    } deriving (Generic, Show, Eq, Ord)

instance NFData AssetUnit

-- | Specify an asset logo as an image data payload
newtype AssetLogo = AssetLogo
    { unAssetLogo :: ByteString
    } deriving (Eq, Ord, Generic)
    deriving (Show) via (Quiet AssetLogo)

instance NFData AssetLogo
