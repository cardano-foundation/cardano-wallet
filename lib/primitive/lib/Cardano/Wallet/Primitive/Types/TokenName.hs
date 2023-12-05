{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wallet.Primitive.Types.TokenName
    ( TokenName (..)
    , fromByteString
    , nullTokenName
    , tokenNameMaxLength
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
newtype TokenName =
    -- | Construct a 'TokenName' without any validation.
    UnsafeTokenName { unTokenName :: ByteString }
    deriving stock (Eq, Ord, Generic)
    deriving (Read, Show) via (Quiet TokenName)
    deriving anyclass Hashable

-- | Construct a 'TokenName', validating that the length does not exceed
--   'tokenNameMaxLength'.
--
fromByteString :: ByteString -> Either String TokenName
fromByteString bs
    | BS.length bs <= tokenNameMaxLength = Right $ UnsafeTokenName bs
    | otherwise = Left $ "TokenName length " ++ show (BS.length bs)
        ++ " exceeds maximum of " ++ show tokenNameMaxLength

-- | The empty asset name.
--
-- Asset names may be empty, where a monetary policy script only mints a single
-- asset, or where one asset should be considered as the "default" token for the
-- policy.
--
nullTokenName :: TokenName
nullTokenName = UnsafeTokenName ""

-- | The maximum length of a valid token name.
--
tokenNameMaxLength :: Int
tokenNameMaxLength = 32

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
        . either (Left . ("TokenName is not hex-encoded: " ++)) fromByteString
        . convertFromBase Base16
        . T.encodeUtf8
