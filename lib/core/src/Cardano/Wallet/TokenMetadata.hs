{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.TokenMetadata
    ( TokenMetadataServer (..)
    , tokenMetadataServerFromFile
    , nullTokenMetadataServer

    -- * Convenience
    , fillMetadata

    -- * Client
    , metadataClient
    , getTokenMetadata
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( AssetMetadata (..), TokenPolicyId )
import Control.Monad
    ( mapM, (<=<) )
import Data.Aeson
    ( FromJSON (..)
    , Object
    , ToJSON (..)
    , Value
    , eitherDecodeFileStrict
    , eitherDecodeStrict'
    , encode
    , withArray
    , withObject
    , (.:)
    )
import Data.Aeson.Types
    ( Parser )
import Data.Bifunctor
    ( bimap )
import Data.ByteString
    ( ByteString )
import Data.Foldable
    ( toList )
import Data.Map
    ( Map )
import Data.Set
    ( Set )
import Data.Text
    ( Text )
import GHC.Generics
    ( Generic )
import Network.HTTP.Client
import UnliftIO.Exception
    ( Exception, throwIO, tryAny )

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

{-------------------------------------------------------------------------------
   Token Metadata
-------------------------------------------------------------------------------}

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
        md <- AssetMetadata <$> o `getProperty` "name" <*> o `getProperty` "description"
        return (subject, md)
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

{-------------------------------------------------------------------------------
                            Cardano Metadata Server
-------------------------------------------------------------------------------}

-- | Models a request to the @POST /metadata/query@ endpoint of the metadata
-- server -- the only one that we need.
data BatchRequest = BatchRequest
    { subjects :: [Subject]
    , properties :: [PropertyName]
    } deriving (Generic, Show, Eq)

type BatchResponse = [Properties]

data Properties = Properties
    { subject :: Subject
    , owner :: Signature
    , properties :: [(PropertyName, (PropertyValue, [Signature]))]
    } deriving (Generic, Show, Eq)

newtype Subject = Subject { unSubject :: Text }
    deriving (Generic, Show, Eq)

newtype PropertyName = PropertyName { unPropertyName :: Text }
    deriving (Generic, Show, Eq)

-- Properties can actually have any json value.
-- But for now, limit to text.
newtype PropertyValue = PropertyValue { unPropertyValue :: Text }
    deriving (Generic, Show, Eq)

newtype Hex = Hex ByteString
 deriving (Generic, Show, Eq)
data Signature = Signature
    { signature :: Hex
    , publicKey :: Hex
    } deriving (Generic, Show, Eq)

-- todo: use generic
instance ToJSON BatchRequest where
instance ToJSON PropertyName where
instance FromJSON PropertyName where
instance ToJSON Subject where
instance FromJSON Subject where
instance FromJSON Properties where
instance FromJSON Signature where
instance FromJSON Hex where
    parseJSON = error "hex decode bytestring"
instance FromJSON PropertyValue where

newtype JSONParseError = JSONParseError String
    deriving (Show, Eq)
instance Exception JSONParseError

metadataClient :: String -> Manager -> BatchRequest -> IO BatchResponse
metadataClient baseURL manager =
    parseResponse <=< flip httpLbs manager <=< makeHttpReq
  where
    makeHttpReq query = do
        req <- parseRequest (baseURL ++ "metadata/query")
        pure req {
            method = "POST",
            requestBody = RequestBodyLBS $ encode query
            }
    parseResponse = either (throwIO . JSONParseError) pure
        . eitherDecodeStrict'
        . BL.toStrict
        . responseBody

{-------------------------------------------------------------------------------
                           Requesting token metadata
-------------------------------------------------------------------------------}

data TokenMetadataError = TokenMetadataError String -- todo: constructors

getTokenMetadata
    :: (BatchRequest -> IO BatchResponse)
    -> [AssetId]
    -> IO (Either TokenMetadataError [(AssetId, AssetMetadata)])
getTokenMetadata client =
    fmap (bimap (TokenMetadataError . show) fromResponse) . tryAny . client . toRequest
  where
    toRequest :: [AssetId] -> BatchRequest
    toRequest as = BatchRequest (map toSubject as)
        [PropertyName "name", PropertyName "description"]
    toSubject = error "todo: hex-encode policyId + assetName"
    fromResponse :: BatchResponse -> [(AssetId, AssetMetadata)]
    fromResponse = error "todo"

testIt :: IO ()
testIt = do
    client <- metadataClient "http://localhost:8000/api/" <$> newManager defaultManagerSettings
    Right r <- getTokenMetadata client []
    pure ()
