{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Cardano.Wallet.Primitive.Types.TokenPolicy
    (
      -- * Token Policies
      TokenPolicyId (..)

      -- * Token Names
    , TokenName (..)
    , mkTokenName
    , nullTokenName
    , maxLengthTokenName

      -- * Token Metadata
    , AssetMetadata (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( (>=>) )
import Data.Aeson
    ( FromJSON (..), ToJSON (..) )
import Data.Bifunctor
    ( first )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
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
import Quiet
    ( Quiet (..) )

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

-- | Information about an asset, from a source external to the chain.
data AssetMetadata = AssetMetadata
    { name :: Text
    -- , acronym :: Text  -- TODO: needs metadata-server support
    , description :: Text   -- TODO: needs metadata-server support
    -- , url :: Text   -- TODO: needs metadata-server support
    -- , logoBase64 :: ByteString   -- TODO: needs metadata-server support
    -- , unit :: AssetUnit   -- TODO: needs metadata-server support
    } deriving stock (Eq, Ord, Generic)
    deriving (Read, Show) via (Quiet AssetMetadata)

instance NFData AssetMetadata
