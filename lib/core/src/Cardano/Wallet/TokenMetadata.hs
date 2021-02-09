{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Wallet.TokenMetadata
    (
    -- * Convenience
      fillMetadata

    -- * Token Metadata Client
    , TokenMetadataClient
    , newMetadataClient
    , getTokenMetadata

    -- * Logging
    , TokenMetadataLog (..)

    -- * Generic metadata server client
    , metadataClient
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

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation, HasSeverityAnnotation (..) )
import Cardano.Wallet.Primitive.Types
    ( TokenMetadataServer (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( AssetMetadata (..), TokenName (..), TokenPolicyId (..) )
import Control.Tracer
    ( Tracer, traceWith )
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
import Data.Functor
    ( ($>) )
import Data.Hashable
    ( Hashable )
import Data.Maybe
    ( mapMaybe )
import Data.String
    ( IsString (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
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
    , requestFromURI
    )
import Network.HTTP.Client.TLS
    ( newTlsManager )
import Network.URI
    ( relativeTo )
import Network.URI.Static
    ( relativeReference )
import Numeric.Natural
    ( Natural )
import UnliftIO.Exception
    ( Exception, throwIO, tryAny )

import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
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
        Right (Map.fromList -> m) ->
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

-- | Specification of a larger unit for an asset. For example, the "lovelace"
-- asset has the larger unit "ada" with 6 zeroes.
data AssetUnit = AssetUnit
    { name :: Text -- ^ Name of the larger asset.
    , decimals :: Natural  -- ^ Number of zeroes to add to base unit.
    } deriving (Generic, Show, Eq)

-- | Will be used in future for checking integrity and authenticity of metadata.
data Signature = Signature
    { signature :: ByteString
    , publicKey :: ByteString
    } deriving (Generic, Show, Eq)

{-------------------------------------------------------------------------------
                       Client for Cardano metadata-server
-------------------------------------------------------------------------------}

metadataClient
    :: Tracer IO TokenMetadataLog
    -> TokenMetadataServer
    -> Manager
    -> BatchRequest
    -> IO BatchResponse
metadataClient tr (TokenMetadataServer baseURI) manager req = do
    traceWith tr $ MsgWillFetchMetadata req
    res <- parseResponse =<< flip httpLbs manager =<< makeHttpReq req
    traceWith tr $ MsgDidFetchMetadata res
    return res
  where
    makeHttpReq query = do
        req <- requestFromURI $ endpoint `relativeTo` baseURI
        pure req
            { method = "POST"
            , requestBody = RequestBodyLBS $ encode query
            , requestHeaders = [("Content-type", "application/json")]
            }
    endpoint = [relativeReference|metadata/query|]

    -- todo: logging of response status and any errors
    parseResponse = either (throwIO . JSONParseError) pure
        . eitherDecodeStrict'
        . BL.toStrict
        . responseBody

-----------
-- Errors

-- | The possible errors which can occur when fetching metadata.
data TokenMetadataError
    = TokenMetadataFetchError String -- ^ Some IO exception
    | TokenMetadataJSONParseError String
    deriving (Generic, Show, Eq)

-- fixme: remove
newtype JSONParseError = JSONParseError String
    deriving (Show, Eq)
instance Exception JSONParseError

-----------
-- Logging

data TokenMetadataLog
    = MsgNotConfigured
    | MsgWillFetchMetadata BatchRequest
    | MsgDidFetchMetadata BatchResponse

instance HasSeverityAnnotation TokenMetadataLog where
    getSeverityAnnotation = \case
        MsgNotConfigured -> Info
        MsgWillFetchMetadata _ -> Debug
        MsgDidFetchMetadata _ -> Debug

instance ToText TokenMetadataLog where
    toText = \case
        MsgNotConfigured -> mconcat
            [ "No token metadata server is configured."
            ]
        MsgWillFetchMetadata r -> mconcat
            [ "Will fetch metadata: "
            , T.pack (show r)
            ]
        MsgDidFetchMetadata r -> mconcat
            [ "Will fetch metadata: "
            , T.pack (show r)
            ]

instance HasPrivacyAnnotation TokenMetadataLog

{-------------------------------------------------------------------------------
                           Requesting token metadata
-------------------------------------------------------------------------------}

-- | Represents a client for the metadata server.
newtype TokenMetadataClient m = TokenMetadataClient
    { _batchQuery :: BatchRequest -> m BatchResponse
    }

-- | Not a client for the metadata server.
nullTokenMetadataClient :: Applicative m => TokenMetadataClient m
nullTokenMetadataClient = TokenMetadataClient (\_ -> pure [])

-- | Construct a 'TokenMetadataClient' for use with 'getTokenMetadata'.
newMetadataClient
    :: Tracer IO TokenMetadataLog -- ^ Logging
    -> Maybe TokenMetadataServer -- ^ URL of metadata server, if enabled.
    -> IO (TokenMetadataClient IO)
newMetadataClient tr (Just uri) =
    TokenMetadataClient . metadataClient tr uri <$> newTlsManager
newMetadataClient tr Nothing =
    traceWith tr MsgNotConfigured $> nullTokenMetadataClient

-- | Fetches metadata for a list of assets using the given client.
getTokenMetadata
    :: TokenMetadataClient IO
    -> [AssetId]
    -> IO (Either TokenMetadataError [(AssetId, AssetMetadata)])
getTokenMetadata (TokenMetadataClient client) as =
    fmap (bimap handleError fromResponse) . tryAny . client $ req
  where
    subjects = map assetIdToSubject as
    req = BatchRequest
        { subjects
        , properties = [PropertyName "name", PropertyName "description"]
        }
    subjectAsset = HM.fromList $ zip subjects as
    fromResponse :: BatchResponse -> [(AssetId, AssetMetadata)]
    fromResponse = mapMaybe $ \ps -> (,)
        <$> HM.lookup (subject ps) subjectAsset
        <*> pure (metadataFromProperties ps)
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
