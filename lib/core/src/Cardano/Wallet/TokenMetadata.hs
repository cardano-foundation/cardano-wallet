{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Wallet.TokenMetadata
    (

    -- * Convenience
      fillMetadata

    -- * Client
    , TokenMetadataClient (..)
    , metadataClient
    , nullMetadataClient

    , getTokenMetadata
    , BatchRequest (..)
    , BatchResponse
    , SubjectProperties (..)
    , Property (..)
    , PropertyValue
    , Subject (..)
    , Signature (..)

    -- * Parsing
    , metadataFromProperties
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( AssetMetadata (..), TokenName (..), TokenPolicyId (..) )
import Control.Monad
    ( (<=<) )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value (..)
    , eitherDecodeStrict'
    , encode
    , withObject
    , withText
    , (.:)
    )
import Data.Bifunctor
    ( bimap )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Foldable
    ( toList )
import Data.Hashable
    ( Hashable )
import Data.Maybe
    ( mapMaybe )
import Data.String
    ( IsString (..) )
import Data.Text
    ( Text )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( Symbol )
import Network.HTTP.Client
    ( Manager
    , Request (..)
    , RequestBody (..)
    , Response (..)
    , httpLbs
    , parseRequest
    )
import Numeric.Natural
    ( Natural )
import UnliftIO.Exception
    ( Exception, throwIO, tryAny )

import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as T

{-------------------------------------------------------------------------------
   Token Metadata
-------------------------------------------------------------------------------}

-- | Helper for adding metadata to sets of assets.
fillMetadata
    :: (Foldable t, Functor t)
    => TokenMetadataClient IO
    -> t AssetId
    -> (Maybe AssetMetadata -> AssetId -> a)
    -> IO (t a)
fillMetadata client assets f = do
    res <- getTokenMetadata client (toList assets)
    case res of
        Right l -> do
            let m = Map.fromList l
            return $ fmap (\aid -> f (Map.lookup aid m) aid) assets
        Left _e -> do
            -- TODO: Trace error?
            return $ fmap (f Nothing) assets

{-------------------------------------------------------------------------------
                            Cardano Metadata Server
-------------------------------------------------------------------------------}

-- | Models a request to the @POST /metadata/query@ endpoint of the metadata
-- server -- the only one that we need.
data BatchRequest = BatchRequest
    { subjects :: [Subject]
    , properties :: [PropertyName]
    } deriving (Generic, Show, Eq)

-- | Properties for each subject in 'BatchRequest'
type BatchResponse = [SubjectProperties]

-- | Property values and signatures for a given subject.
data SubjectProperties = SubjectProperties
    { subject :: Subject
    , owner :: Signature
    -- TODO: use Data.SOP.NP and parameterize type by property names
    , properties :: ( Property "name"
                    -- , (PropertyValue "acronym", [Signature])
                    , Property "description"
                    -- , (PropertyValue "url", [Signature])
                    -- , (PropertyValue "logo", [Signature])
                    -- , (PropertyValue "unit", [Signature])
                    )
    } deriving (Generic, Show, Eq)

-- | A property value and its signatures.
data Property name = Property
    { value :: PropertyValue name
    , signatures :: [Signature]
    } deriving (Generic)

deriving instance Show (PropertyValue name) => Show (Property name)
deriving instance Eq (PropertyValue name) => Eq (Property name)

-- | A metadata server subject, which can be any string.
newtype Subject = Subject { unSubject :: Text }
    deriving (Generic, Show, Eq)
    deriving newtype (IsString, Hashable)

-- | Metadata property identifier.
newtype PropertyName = PropertyName { unPropertyName :: Text }
    deriving (Generic, Show, Eq)
    deriving newtype IsString

-- | The type of a given property name.
type family PropertyValue (name :: Symbol) :: *
type instance PropertyValue "name" = Text
type instance PropertyValue "acronym" = Text
type instance PropertyValue "description" = Text
type instance PropertyValue "url" = Text
-- type instance PropertyValue "logo" = AssetLogoBase64
type instance PropertyValue "unit" = AssetUnit

data AssetUnit = AssetUnit
    { name :: Text
    , decimals :: Natural
    } deriving (Generic, Show, Eq)

-- | Used for checking integrity and authenticity of metadata.
data Signature = Signature
    { signature :: ByteString
    , publicKey :: ByteString
    } deriving (Generic, Show, Eq)

{-------------------------------------------------------------------------------
                       Client for Cardano metadata-server
-------------------------------------------------------------------------------}

newtype TokenMetadataClient m = TokenMetadataClient
    { batchRequest :: BatchRequest -> m BatchResponse }

newtype JSONParseError = JSONParseError String
    deriving (Show, Eq)
instance Exception JSONParseError

metadataClient :: String -> Manager -> TokenMetadataClient IO
metadataClient baseURL manager =
    TokenMetadataClient {
        batchRequest = parseResponse <=< flip httpLbs manager <=< makeHttpReq
    }
  where
    makeHttpReq query = do
        req <- parseRequest (baseURL ++ "metadata/query")
        pure req
            { method = "POST"
            , requestBody = RequestBodyLBS $ encode query
            , requestHeaders = [("Content-type", "application/json")]
            }
    -- todo: logging of response status and any errors
    parseResponse = either (throwIO . JSONParseError) pure
        . eitherDecodeStrict'
        . BL.toStrict
        . responseBody

nullMetadataClient :: Monad m => TokenMetadataClient m
nullMetadataClient = TokenMetadataClient
    { batchRequest = \_ -> pure [] }


{-------------------------------------------------------------------------------
                           Requesting token metadata
-------------------------------------------------------------------------------}

getTokenMetadata
    :: TokenMetadataClient IO
    -> [AssetId]
    -> IO (Either TokenMetadataError [(AssetId, AssetMetadata)])
getTokenMetadata client as =
    fmap (bimap handleError fromResponse) . tryAny . batchRequest client $ req
  where
    subjects = map assetIdToSubject as
    req = BatchRequest
        { subjects
        , properties = [PropertyName "name", PropertyName "description"]
        }
    subjectAsset = HM.fromList $ zip subjects as
    fromResponse :: BatchResponse -> [(AssetId, AssetMetadata)]
    fromResponse = mapMaybe (\ps -> (,) <$> HM.lookup (subject ps) subjectAsset <*> pure (metadataFromProperties ps))
    handleError = TokenMetadataFetchError . show

-- | Creates a metadata server subject from an AssetId. The subject is the
-- policy id and asset name hex-encoded.
assetIdToSubject :: AssetId -> Subject
assetIdToSubject (AssetId (UnsafeTokenPolicyId (Hash p)) (UnsafeTokenName n)) =
    Subject $ T.decodeLatin1 $ convertToBase Base16 (p <> n)

-- | Convert metadata server properties response into an 'AssetMetadata' record.
-- Only the values are taken. Signatures are ignored (for now).
metadataFromProperties :: SubjectProperties -> AssetMetadata
metadataFromProperties (SubjectProperties _ _ ((Property n _, Property d _))) =
    AssetMetadata n d

-- | The possible errors which can occur when fetching metadata.
-- TODO: more constructors
newtype TokenMetadataError
    = TokenMetadataFetchError String -- ^ Some IO exception
    deriving (Generic, Show, Eq)

{-------------------------------------------------------------------------------
                      Aeson instances for metadata-server
-------------------------------------------------------------------------------}

instance ToJSON BatchRequest where

instance ToJSON PropertyName where
    toJSON = String . unPropertyName
instance FromJSON PropertyName where
    parseJSON = withText "PropertyName" (pure . PropertyName)

instance ToJSON Subject where
    toJSON = String . unSubject
instance FromJSON Subject where
    parseJSON = withText "Subject" (pure . Subject)

instance FromJSON SubjectProperties where
   parseJSON = withObject "SubjectProperties" $ \o -> SubjectProperties
       <$> o .: "subject"
       <*> o .: "owner"
       <*> ((,) <$> o .: "name" <*> o .: "description")

instance FromJSON (PropertyValue name) => FromJSON (Property name) where
    parseJSON = withObject "Property" $ \o -> Property
        <$> o .: "value"
        <*> o .: "anSignatures"

instance FromJSON Signature where
    parseJSON = withObject "Signature" $ \o -> Signature
        <$> fmap unHex (o .: "signature")
        <*> fmap unHex (o .: "publicKey")

newtype Hex = Hex { unHex :: ByteString } deriving (Generic, Show, Eq)

instance FromJSON Hex where
    parseJSON = withText "hex bytestring" $
        either fail (pure . Hex) . convertFromBase Base16 . T.encodeUtf8

instance FromJSON AssetUnit where
    -- TODO: AssetUnit, when it's provided by the metadata server
