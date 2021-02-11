{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- A client used to query asset metadata from the Cardano metadata-server.
--
-- The OpenAPI specification is here:
-- <https://github.com/input-output-hk/metadata-server/blob/master/specifications/api/openapi.yaml>

module Cardano.Wallet.TokenMetadata
    (
    -- * Associating metadata with assets
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
    , BatchResponse (..)
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
import Cardano.Wallet.Logging
    ( BracketLog (..), LoggedException (..), bracketTracer, produceTimings )
import Cardano.Wallet.Primitive.Types
    ( TokenMetadataServer (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( AssetLogo (..)
    , AssetMetadata (..)
    , AssetUnit (..)
    , TokenName (..)
    , TokenPolicyId (..)
    )
import Control.Applicative
    ( (<|>) )
import Control.Monad
    ( mapM, when )
import Control.Tracer
    ( Tracer, contramap, traceWith )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value (..)
    , eitherDecodeFileStrict
    , eitherDecodeStrict'
    , encode
    , object
    , withObject
    , withText
    , (.!=)
    , (.:)
    , (.:?)
    , (.=)
    )
import Data.Bifunctor
    ( first )
import Data.ByteArray.Encoding
    ( Base (Base16, Base64), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Foldable
    ( toList )
import Data.Functor
    ( ($>) )
import Data.Hashable
    ( Hashable )
import Data.Maybe
    ( catMaybes, fromMaybe, mapMaybe )
import Data.String
    ( IsString (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Time.Clock
    ( DiffTime )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( Symbol )
import Network.HTTP.Client
    ( HttpException
    , Manager
    , Request (..)
    , RequestBody (..)
    , Response (..)
    , brReadSome
    , requestFromURI
    , setRequestCheckStatus
    , withResponse
    )
import Network.HTTP.Client.TLS
    ( newTlsManager )
import Network.URI
    ( URI (..), relativeTo )
import Network.URI.Static
    ( relativeReference )
import UnliftIO.Exception
    ( SomeException, handle, handleAny )

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T

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

-- | Models the response from the @POST /metadata/query@ endpoint of the
-- metadata server. This should contain properties each subject in the
-- 'BatchRequest'.
newtype BatchResponse = BatchResponse
    { getBatchResponse :: [SubjectProperties]
    } deriving (Generic, Show, Eq)

-- | Property values and signatures for a given subject.
data SubjectProperties = SubjectProperties
    { subject :: Subject
    , owner :: Maybe Signature
    -- TODO: use Data.SOP.NP and parameterize type by property names
    -- Name and description are required, both others may be missing the
    -- response.
    , properties ::
        ( Property "name"
        , Property "description"
        , Maybe (Property "acronym")
        , Maybe (Property "url")
        , Maybe (Property "logo")
        , Maybe (Property "unit")
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
    deriving (Generic, Show, Eq, Ord)
    deriving newtype (IsString, Hashable)

-- | Metadata property identifier.
newtype PropertyName = PropertyName { unPropertyName :: Text }
    deriving (Generic, Show, Eq)
    deriving newtype IsString

-- | The type of a given property name.
type family PropertyValue (name :: Symbol) :: *
type instance PropertyValue "name" = Text
type instance PropertyValue "description" = Text
type instance PropertyValue "acronym" = Text
type instance PropertyValue "url" = Text
type instance PropertyValue "logo" = AssetLogo
type instance PropertyValue "unit" = AssetUnit

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
    -> IO (Either TokenMetadataError BatchResponse)
metadataClient tr (TokenMetadataServer baseURI) manager batch = do
    res <- handleExc $ fmap parseResponse . doRequest =<< makeHttpReq batch
    traceWith tr $ MsgFetchResult batch res
    return res
  where
    -- Construct a Request from the batch.
    makeHttpReq query = do
        let json = encode query
            uri = endpoint `relativeTo` baseURI
        traceWith tr $ MsgFetchRequestBody uri json
        req <- requestFromURI uri
        pure $ setRequestCheckStatus req
            { method = "POST"
            , requestBody = RequestBodyLBS json
            , requestHeaders = [("Content-type", "application/json")]
            }
    endpoint = [relativeReference|metadata/query|]

    -- Read the request body. Status code has already been checked via
    -- 'setRequestStatus'.
    doRequest req = bracketTracer (contramap (MsgFetchRequest batch) tr) $ do
        withResponse req manager $ \res -> do
            bs <- brReadSome (responseBody res) maxResponseSize
            when (BL.length bs >= fromIntegral maxResponseSize) $
                traceWith tr (MsgFetchMetadataMaxSize maxResponseSize)
            pure $ BL.toStrict bs

    -- decode and parse json
    parseResponse bs =
        first (TokenMetadataJSONParseError bs) (eitherDecodeStrict' bs)

    -- Convert http-client exceptions to Left, handle any other synchronous
    -- exceptions that may occur.
    handleExc = handle (loggedErr TokenMetadataFetchError)
        . handleAny (loggedErr TokenMetadataClientError)
    loggedErr c = pure . Left . c . LoggedException

    -- Don't let a metadata server consume all our memory - limit to 10MiB
    maxResponseSize = 10*1024*1024

-----------
-- Errors

-- | The possible errors which can occur when fetching metadata.
data TokenMetadataError
    = TokenMetadataClientError (LoggedException SomeException)
        -- ^ Unhandled exception
    | TokenMetadataFetchError (LoggedException HttpException)
        -- ^ Error with HTTP request
    | TokenMetadataJSONParseError ByteString String
        -- ^ Error from aeson decoding of JSON
    deriving (Generic, Show, Eq)

instance ToText TokenMetadataError where
    toText = \case
        TokenMetadataClientError e ->
             "Unhandled exception: " <> toText e
        TokenMetadataFetchError e ->
             "Error querying metadata server: " <> toText e
        TokenMetadataJSONParseError json e -> mconcat
            [ "Error parsing metadata server response JSON: "
            , T.pack e
            , "\nThe first 250 characters of the response are:\n"
            , T.decodeUtf8With T.lenientDecode $ B8.take 250 json
            ]

-----------
-- Logging

data TokenMetadataLog
    = MsgNotConfigured
    | MsgFetchRequest BatchRequest BracketLog
    | MsgFetchRequestBody URI BL.ByteString
    | MsgFetchMetadataMaxSize Int
    | MsgFetchResult BatchRequest (Either TokenMetadataError BatchResponse)
    | MsgFetchMetadataTime BatchRequest DiffTime
    deriving (Show, Eq)

instance HasSeverityAnnotation TokenMetadataLog where
    getSeverityAnnotation = \case
        MsgNotConfigured -> Notice
        MsgFetchRequest _ b -> getSeverityAnnotation b
        MsgFetchRequestBody _ _ -> Debug
        MsgFetchMetadataMaxSize _ -> Warning
        MsgFetchResult _ (Right _) -> Info
        MsgFetchResult _ (Left _) -> Error
        MsgFetchMetadataTime _ _ -> Debug

instance ToText TokenMetadataLog where
    toText = \case
        MsgNotConfigured -> mconcat
            [ "No token metadata server is configured."
            ]
        MsgFetchRequest r BracketStart -> mconcat
            [ "Will fetch metadata: "
            , T.pack (show r)
            ]
        MsgFetchRequest _ b -> mconcat
            [ "Metadata fetch: "
            , toText b
            ]
        MsgFetchRequestBody uri bs -> mconcat
            [ "POST ", T.pack (show uri), "\n"
            , T.decodeUtf8With T.lenientDecode (BL.toStrict bs) ]
        MsgFetchMetadataMaxSize maxSize -> mconcat
            [ "Metadata server returned more data than the permitted maximum of"
            , toText maxSize
            , " bytes."
            ]
        MsgFetchResult req res -> case res of
            Right (BatchResponse batch) -> mconcat
                [ "Successfully queried metadata-server for "
                , toText (length $ subjects req)
                , " assets, and received "
                , toText (length batch)
                , " in response."
                ]
            Left e -> mconcat
                [ "An error occurred while fetching metadata: "
                , toText e
                ]
        MsgFetchMetadataTime _ dt -> mconcat
            [ "Metadata request took: "
            , T.pack (show dt)
            ]

instance HasPrivacyAnnotation TokenMetadataLog

traceRequestTimings :: Tracer IO TokenMetadataLog -> IO (Tracer IO TokenMetadataLog)
traceRequestTimings tr = produceTimings msgQuery trDiffTime
  where
    trDiffTime = contramap (uncurry MsgFetchMetadataTime) tr
    msgQuery = \case
        MsgFetchRequest req b -> Just (req, b)
        _ -> Nothing

{-------------------------------------------------------------------------------
                           Requesting token metadata
-------------------------------------------------------------------------------}

-- | Represents a client for the metadata server.
newtype TokenMetadataClient m = TokenMetadataClient
    { _batchQuery :: BatchRequest -> m (Either TokenMetadataError BatchResponse)
    }

-- | Not a client for the metadata server.
nullTokenMetadataClient :: Applicative m => TokenMetadataClient m
nullTokenMetadataClient = TokenMetadataClient $ \_ ->
    pure . Right $ BatchResponse []

-- | Mock metadata client using a hard-coded server response from a file for
-- data.
mockFileTokenMetadataClient :: URI -> TokenMetadataClient IO
mockFileTokenMetadataClient uri = TokenMetadataClient $ \(BatchRequest subjects _)-> do
    let filePath = T.unpack
            . fromMaybe (error "mockFileTokenMetadataClient: bad file path")
            . T.stripPrefix "file:/" . T.pack $ show uri
    BatchResponse mds <- either (error . show) id
        <$> eitherDecodeFileStrict filePath
    let m = HM.fromList [(subject md, md) | md <- mds]
    pure $ Right $ BatchResponse $ mapMaybe (`HM.lookup` m) subjects

-- | Construct a 'TokenMetadataClient' for use with 'getTokenMetadata'.
newMetadataClient
    :: Tracer IO TokenMetadataLog -- ^ Logging
    -> Maybe TokenMetadataServer -- ^ URL of metadata server, if enabled.
    -> IO (TokenMetadataClient IO)
newMetadataClient tr (Just uri'@(TokenMetadataServer uri))
    | uriScheme uri == "file:" = do
        pure $ mockFileTokenMetadataClient uri
    | otherwise = do
    trTimings <- traceRequestTimings tr
    TokenMetadataClient . metadataClient (tr <> trTimings) uri' <$> newTlsManager
newMetadataClient tr Nothing =
    traceWith tr MsgNotConfigured $> nullTokenMetadataClient

-- | Fetches metadata for a list of assets using the given client.
getTokenMetadata
    :: TokenMetadataClient IO
    -> [AssetId]
    -> IO (Either TokenMetadataError [(AssetId, AssetMetadata)])
getTokenMetadata (TokenMetadataClient client) as =
    fmap fromResponse <$> client req
  where
    subjects = map assetIdToSubject as
    req = BatchRequest
        { subjects
        , properties = PropertyName <$>
             [ "name", "description", "acronym"
             , "url", "logo", "unit" ]
        }
    subjectAsset = HM.fromList $ zip subjects as
    fromResponse :: BatchResponse -> [(AssetId, AssetMetadata)]
    fromResponse = mapMaybe (\ps -> (,)
        <$> HM.lookup (subject ps) subjectAsset
        <*> pure (metadataFromProperties ps))
        . getBatchResponse

-- | Creates a metadata server subject from an AssetId. The subject is the
-- policy id and asset name hex-encoded.
assetIdToSubject :: AssetId -> Subject
assetIdToSubject (AssetId (UnsafeTokenPolicyId (Hash p)) (UnsafeTokenName n)) =
    Subject $ T.decodeLatin1 $ convertToBase Base16 (p <> n)

-- | Convert metadata server properties response into an 'AssetMetadata' record.
-- Only the values are taken. Signatures are ignored (for now).
metadataFromProperties :: SubjectProperties -> AssetMetadata
metadataFromProperties (SubjectProperties _ _ properties) =
    AssetMetadata { name, description, acronym, url, logo, unit }
  where
    ( pName, pDescription, pAcronym, pUrl, pLogo, pUnit ) = properties
    name = value pName
    description = value pDescription
    acronym = value <$> pAcronym
    url = value <$> pUrl
    logo = value <$> pLogo
    unit = value <$> pUnit

{-------------------------------------------------------------------------------
                      Aeson instances for metadata-server
-------------------------------------------------------------------------------}

instance ToJSON BatchRequest where

instance ToJSON PropertyName where
    toJSON = String . unPropertyName
instance FromJSON PropertyName where
    parseJSON = withText "PropertyName" (pure . PropertyName)

instance FromJSON BatchResponse where
    parseJSON =  withObject "BatchResponse" $ \o -> do
        (xs :: [Value]) <- o .: "subjects"
        let maybeParseItem v = fmap Just (parseJSON v) <|> pure Nothing
        BatchResponse <$> (catMaybes <$> mapM maybeParseItem xs)

instance ToJSON Subject where
    toJSON = String . unSubject
instance FromJSON Subject where
    parseJSON = withText "Subject" (pure . Subject)

instance FromJSON SubjectProperties where
    parseJSON = withObject "SubjectProperties" $ \o -> SubjectProperties
        <$> o .: "subject"
        <*> o .:? "owner"
        <*> parseProperties o
      where
        parseProperties o = (,,,,,)
            <$> o .: "name"
            <*> o .: "description"
            <*> o .:? "acronym"
            <*> o .:? "url"
            <*> o .:? "logo"
            <*> o .:? "unit"

instance FromJSON (PropertyValue name) => FromJSON (Property name) where
    parseJSON = withObject "Property" $ \o -> Property
        <$> o .: "value"
        <*> o .:? "anSignatures" .!= []

instance FromJSON Signature where
    parseJSON = withObject "Signature" $ \o -> Signature
        <$> fmap (raw @'Base16) (o .: "signature")
        <*> fmap (raw @'Base16) (o .: "publicKey")

instance FromJSON AssetLogo where
    parseJSON =
        fmap (AssetLogo . raw @'Base64) . parseJSON

instance ToJSON AssetLogo where
    toJSON =
        toJSON . B8.unpack . convertToBase Base64 . unAssetLogo

instance FromJSON AssetUnit where
    parseJSON = withObject "AssetUnit" $ \o -> AssetUnit
        <$> o .: "name"
        <*> o .: "decimals"

instance ToJSON AssetUnit where
    toJSON AssetUnit{name,decimals} = object
        [ "name" .= name
        , "decimals" .= decimals
        ]

--
-- Helpers
--

newtype Encoded (base :: Base) = Encoded
    { raw :: ByteString }
    deriving (Generic, Show, Eq)

instance FromJSON (Encoded 'Base16) where
    parseJSON = withText "base16 bytestring" $
        either fail (pure . Encoded) . convertFromBase Base16 . T.encodeUtf8

instance FromJSON (Encoded 'Base64) where
    parseJSON = withText "base64 bytestring" $
        either fail (pure . Encoded) . convertFromBase Base64 . T.encodeUtf8

instance FromJSON BatchRequest where

instance ToJSON SubjectProperties where
   toJSON (SubjectProperties s o (n,d,a,u,l,t)) = object
       [ "subject" .= s
       , "owner" .= o
       , "name" .= n
       , "description" .= d
       , "acronym" .= a
       , "url" .= u
       , "logo" .= l
       , "unit" .= t
       ]

instance ToJSON (PropertyValue name) => ToJSON (Property name) where
    toJSON (Property v s) = object [ "value" .= v, "anSignatures" .= s ]

instance ToJSON Signature where
    toJSON (Signature s k) = object
        [ "signature" .= hex s
        , "publicKey" .= hex k
        ]
      where
        hex = T.decodeLatin1 . convertToBase Base16

instance ToJSON BatchResponse where
    toJSON (BatchResponse subs) = object
        [ "subjects" .= subs
        ]
