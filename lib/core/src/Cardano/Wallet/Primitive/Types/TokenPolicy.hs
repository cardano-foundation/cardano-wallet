{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Cardano.Wallet.Primitive.Types.TokenPolicy
    (
      -- * Token Policies
      TokenPolicyId (..)

      -- * Token Names
    , TokenName (..)

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
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), ToText (..) )
import Fmt
    ( Buildable (..) )
import GHC.Generics
    ( Generic )
import Quiet
    ( Quiet (..) )

-- | Token policy identifiers, represented by the hash of the monetary policy
-- script.
newtype TokenPolicyId =
    -- | Construct a 'TokenPolicyId' without any validation.
    UnsafeTokenPolicyId { unTokenPolicyId :: Hash "TokenPolicy" }
    deriving stock (Eq, Ord, Generic)
    deriving (Read, Show) via (Quiet TokenPolicyId)

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
    UnsafeTokenName { unTokenName :: Text }
    deriving stock (Eq, Ord, Generic)
    deriving (Read, Show) via (Quiet TokenName)

instance NFData TokenName

instance Buildable TokenName where
    build = build . unTokenName

instance FromJSON TokenName where
    parseJSON = parseJSON >=> either (fail . show) pure . fromText

instance ToJSON TokenName where
    toJSON = toJSON . toText

instance ToText TokenName where
    toText = unTokenName

instance FromText TokenName where
    fromText = pure . UnsafeTokenName

-- | Information about an asset, from a source external to the chain.
newtype AssetMetadata = AssetMetadata
    { name :: Text
    } deriving stock (Eq, Ord, Generic)
    deriving (Read, Show) via (Quiet AssetMetadata)

instance NFData AssetMetadata
