{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.Types.TokenPolicy
    (
      -- * Token Policies
      TokenPolicyId
    , mkTokenPolicyId

      -- * Token Names
    , TokenName
    , mkTokenName

      -- * Constants
    , adaTokenPolicyId
    , adaTokenName

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
import Data.ByteString
    ( ByteString )
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

--------------------------------------------------------------------------------
-- Token policy identifiers
--------------------------------------------------------------------------------

newtype TokenPolicyId = TokenPolicyId
    { unTokenPolicyId :: Hash "TokenPolicy" }
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
    fromText = fmap TokenPolicyId . fromText

mkTokenPolicyId :: ByteString -> TokenPolicyId
mkTokenPolicyId = TokenPolicyId . Hash

--------------------------------------------------------------------------------
-- Token names
--------------------------------------------------------------------------------

newtype TokenName = TokenName
    { unTokenName :: Text }
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
    fromText = pure . TokenName

mkTokenName :: Text -> TokenName
mkTokenName = TokenName

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- TODO: Replace this with the correct token policy ID.
adaTokenPolicyId :: TokenPolicyId
adaTokenPolicyId = mkTokenPolicyId "0"

-- TODO: Verify that this is the correct token name.
adaTokenName :: TokenName
adaTokenName = mkTokenName "ADA"
