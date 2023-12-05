{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Cardano.Wallet.Primitive.Types.TokenMetadata
    ( AssetMetadata (..)
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

import Control.DeepSeq
    ( NFData
    )
import Control.Monad
    ( (>=>)
    )
import Data.Bifunctor
    ( first
    )
import Data.ByteString
    ( ByteString
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( FromText (..)
    , TextDecodingError (..)
    , ToText (..)
    )
import GHC.Generics
    ( Generic
    )
import Network.URI
    ( URI
    , parseAbsoluteURI
    , uriScheme
    )
import Quiet
    ( Quiet (..)
    )

import qualified Data.ByteString as BS
import qualified Data.Text as T

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
    | otherwise = Left $ mconcat
        [ "Length must be at least "
        , show n
        , " characters, got "
        , show len
        ]
  where
    len = T.length text

validateMaxLength :: Int -> Text -> Either String Text
validateMaxLength n text
    | len <= n = Right text
    | otherwise = Left $ mconcat
        [ "Length must be no more than "
        , show n
        , " characters, got "
        , show len
        ]
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
    | otherwise = Left $ mconcat
        [ "Length must be no more than "
        , show maxLen
        , " bytes, got "
        , show len
        ]
  where
    len = BS.length $ unAssetLogo logo
    maxLen = 65536

validateMetadataDecimals :: AssetDecimals -> Either String AssetDecimals
validateMetadataDecimals (AssetDecimals n)
    | n >= 0 && n <= 255 =
        Right $ AssetDecimals n
    | otherwise =
        Left "Decimal value must be between [0, 255] inclusive."
