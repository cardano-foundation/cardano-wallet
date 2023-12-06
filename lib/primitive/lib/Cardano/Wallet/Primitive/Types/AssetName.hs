{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wallet.Primitive.Types.AssetName
    ( AssetName (..)
    , empty
    , fromByteString
    , maxLength
    ) where

import Prelude

import Control.DeepSeq
    ( NFData
    )
import Control.Monad
    ( (>=>)
    )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    )
import Data.Bifunctor
    ( first
    )
import Data.ByteArray.Encoding
    ( Base (Base16)
    , convertFromBase
    , convertToBase
    )
import Data.ByteString
    ( ByteString
    )
import Data.Hashable
    ( Hashable
    )
import Data.Text.Class
    ( FromText (..)
    , TextDecodingError (..)
    , ToText (..)
    )
import Fmt
    ( Buildable (..)
    )
import GHC.Generics
    ( Generic
    )
import Quiet
    ( Quiet (..)
    )

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

-- | Token names, defined by the monetary policy script.
newtype AssetName =
    -- | Construct a 'AssetName' without any validation.
    UnsafeAssetName { unAssetName :: ByteString }
    deriving stock (Eq, Ord, Generic)
    deriving (Read, Show) via (Quiet AssetName)
    deriving anyclass Hashable

-- | Construct a 'AssetName', validating that the length does not exceed
--   'maxLength'.
--
fromByteString :: ByteString -> Either String AssetName
fromByteString bs
    | BS.length bs <= maxLength = Right $ UnsafeAssetName bs
    | otherwise = Left $ "AssetName length " ++ show (BS.length bs)
        ++ " exceeds maximum of " ++ show maxLength

-- | The empty asset name.
--
-- Asset names may be empty, where a monetary policy script only mints a single
-- asset, or where one asset should be considered as the "default" token for the
-- policy.
--
empty :: AssetName
empty = UnsafeAssetName ""

-- | The maximum length of a valid token name.
--
maxLength :: Int
maxLength = 32

instance NFData AssetName

instance Buildable AssetName where
    build = build . toText

instance FromJSON AssetName where
    parseJSON = parseJSON >=> either (fail . show) pure . fromText

instance ToJSON AssetName where
    toJSON = toJSON . toText

instance ToText AssetName where
    toText = T.decodeLatin1 . convertToBase Base16 . unAssetName

instance FromText AssetName where
    fromText = first TextDecodingError
        . either (Left . ("AssetName is not hex-encoded: " ++)) fromByteString
        . convertFromBase Base16
        . T.encodeUtf8
