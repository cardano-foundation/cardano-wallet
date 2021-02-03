{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Wallet.TokenMetadata
    ( TokenMetadataServer (..)
    , tokenMetadataServerFromFile
    , nullTokenMetadataServer

    -- * Convenience
    , fillMetadata
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( AssetMetadata (..), TokenPolicyId )
import Control.Monad
    ( mapM )
import Data.Aeson
    ( FromJSON (..)
    , Object
    , Value
    , eitherDecodeFileStrict
    , withArray
    , withObject
    , (.:)
    )
import Data.Aeson.Types
    ( Parser )
import Data.Foldable
    ( toList )
import Data.Map
    ( Map )
import Data.Set
    ( Set )
import Data.Text
    ( Text )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

-- | Helper for adding metadata to sets of assets.
fillMetadata
    :: Ord a
    => Monad m
    => TokenMetadataServer m
    -> Set AssetId
    -> (Maybe AssetMetadata -> AssetId -> a)
    -> m (Set a)
fillMetadata server assets f = do
    let policies = Set.map tokenPolicyId assets
    m <- fetchTokenMeta server (toList policies)
    return $ Set.map (\aid -> f (Map.lookup (tokenPolicyId aid) m) aid) assets


-- | Mock metadata server which pulls the response from a file.
--
-- Doesn't care about which policy ids we request metadata for.
tokenMetadataServerFromFile :: FilePath -> TokenMetadataServer IO
tokenMetadataServerFromFile fp = TokenMetadataServer
    { fetchTokenMeta = \_tokens -> do
        either (fail . show) (pure . unTokenMetadataMap)
            =<< eitherDecodeFileStrict fp
    }

nullTokenMetadataServer :: Monad m => TokenMetadataServer m
nullTokenMetadataServer = TokenMetadataServer
    { fetchTokenMeta = \_ -> pure Map.empty
    }

newtype TokenMetadataServer m = TokenMetadataServer
    { fetchTokenMeta :: [TokenPolicyId] -> m (Map TokenPolicyId AssetMetadata)
    }

-- Makes it easier to work with Aeson. TODO: probably remove.
newtype TokenMetadataMap = TokenMetadataMap
    { unTokenMetadataMap :: (Map TokenPolicyId AssetMetadata) }

instance FromJSON TokenMetadataMap where
    parseJSON = fmap TokenMetadataMap . parseTokenMetadataMap

-- NOTE: This parser assumes we will be able to fetch multiple metadata at once,
-- which we hopefully will be:
-- https://github.com/input-output-hk/metadata-server/blob/fc6eb8fda07c259da4e0ef3e6e1c9c62f137a0d0/john-instructions.md
parseTokenMetadataMap :: Value -> Parser (Map TokenPolicyId AssetMetadata)
parseTokenMetadataMap = fmap Map.fromList . withArray "list of subjects"
    (mapM parseTokenMetadataEntry . toList)
  where
    parseTokenMetadataEntry :: Value -> Parser (TokenPolicyId, AssetMetadata)
    parseTokenMetadataEntry = withObject "token metadata" $ \o -> do
        subject <- o .: "subject"
        name' <- AssetMetadata <$> o `getProperty` "name"
        return (subject, name')
      where
        -- | Properties contain both a value an a signature.
        getProperty :: Object -> Text -> Parser Text
        getProperty obj key =
            (obj .: key) >>= withObject (T.unpack key) (\propObj -> do
                (val :: Text)
                    <- propObj .: "value"
                --(sigs :: [Signature])
                --    <- propObj .: "signatures" -- maybe "anSignatures" in server?
                return val
                )
