{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2026 Cardano Foundation
-- License: Apache-2.0
--
-- CIP-0119 DRep off-chain metadata: HTTP fetch and hash verification.
module Cardano.Wallet.DRep.Metadata
    ( fetchDRepMetadata
    , FetchError (..)
    , parseCip0119
    , resolveUrl
    , defaultIpfsGatewayUrl
    ) where

import Cardano.Wallet.Primitive.Types.DRep
    ( DRepMetaReference (..)
    , DRepMetadata (..)
    )
import Control.Exception
    ( SomeException
    , try
    )
import Control.Monad.Trans.Except
    ( ExceptT (..)
    , throwE
    )
import Cryptography.Hash.Blake
    ( blake2b256
    )
import Control.Applicative
    ( (<|>)
    )
import Data.Aeson
    ( Object
    , Value
    , (.:)
    , (.:?)
    )
import Data.Aeson.Types
    ( Parser
    , parseEither
    )
import Data.ByteString
    ( ByteString
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Text
    ( Text
    )
import Network.HTTP.Client
    ( BodyReader
    , Manager
    , requestFromURI
    , responseBody
    , responseStatus
    , withResponse
    )
import Network.HTTP.Types.Status
    ( status200
    , statusCode
    )
import Network.URI
    ( URI
    , parseURI
    , uriScheme
    )
import Prelude

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Text as T

data FetchError
    = FetchInvalidUri !Text
    | FetchHttpError !String
    | FetchNotOk !Int
    | FetchParseError !String
    | FetchHashMismatch
    deriving (Eq, Show)

-- | Default IPFS gateway used when no override is supplied.
defaultIpfsGatewayUrl :: String
defaultIpfsGatewayUrl = "https://ipfs.blockfrost.dev/ipfs/"

-- | Maximum number of bytes we will read from a metadata response body.
-- Responses larger than this are rejected with 'FetchHttpError'.
maxMetadataBytes :: Int
maxMetadataBytes = 1024 * 1024

-- | Read a response body up to 'maxMetadataBytes'.  Returns
-- 'Left (FetchHttpError ...)' if the body exceeds the limit.
readLimited :: BodyReader -> IO (Either FetchError ByteString)
readLimited br = go 0 []
  where
    go !total !acc = do
        chunk <- br
        if BS.null chunk
            then pure $ Right (BS.concat (reverse acc))
            else do
                let total' = total + BS.length chunk
                if total' > maxMetadataBytes
                    then pure $ Left (FetchHttpError "response body exceeds 1 MiB limit")
                    else go total' (chunk : acc)

-- | Resolve a URL to an HTTPS URI, rewriting ipfs:// to the given gateway.
-- Rejects any URL whose final scheme is not https to prevent SSRF via
-- on-chain anchor URLs.
resolveUrl :: String -> Text -> ExceptT FetchError IO URI
resolveUrl ipfsGateway url =
    let urlStr = case T.stripPrefix "ipfs://" url of
            Just cid -> ipfsGateway <> T.unpack cid
            Nothing -> T.unpack url
    in  case parseURI urlStr of
            Nothing -> throwE $ FetchInvalidUri url
            Just uri
                | uriScheme uri /= "https:" -> throwE $ FetchInvalidUri url
                | otherwise -> pure uri

-- | Fetch a CIP-0119 metadata document from 'url', verify its Blake2b-256
-- hash matches 'expectedHash' (raw bytes), and parse it into 'DRepMetadata'.
-- Returns 'Left FetchError' on any failure. Supports ipfs:// URLs via the
-- given IPFS gateway URL.
fetchDRepMetadata
    :: String
    -- ^ IPFS gateway base URL (e.g. \"https://ipfs.blockfrost.dev/ipfs/\")
    -> Manager
    -> Text
    -> ByteString
    -> ExceptT FetchError IO DRepMetadata
fetchDRepMetadata ipfsGateway manager url expectedHash = do
    uri <- resolveUrl ipfsGateway url
    raw <- ExceptT $ do
        eitherResult <- try @SomeException $ do
            req <- requestFromURI uri
            withResponse req manager $ \resp ->
                if responseStatus resp == status200
                    then readLimited (responseBody resp)
                    else
                        pure
                            $ Left
                            $ FetchNotOk
                                (statusCode (responseStatus resp))
        pure $ case eitherResult of
            Left exc -> Left (FetchHttpError (show exc))
            Right (Left fe) -> Left fe
            Right (Right bs) -> Right bs
    if blake2b256 raw /= expectedHash
        then throwE FetchHashMismatch
        else case Aeson.eitherDecodeStrict raw of
            Left err -> throwE $ FetchParseError err
            Right val -> case parseEither parseCip0119 val of
                Left err -> throwE $ FetchParseError err
                Right meta -> pure meta

-- | Parse a CIP-0119 JSON document.
--
-- Supports two layouts:
--   * Nested: top-level "body" object containing the fields (canonical CIP-0119)
--   * Flat: fields at the top level (common in practice)
--
-- In the nested layout, CIP-0119 places @references@ and @doNotList@ at the
-- top level (alongside @body@/@authors@), not inside @body@.  We read them
-- from @top@ first and fall back to @body@ for non-conforming documents.
parseCip0119 :: Value -> Parser DRepMetadata
parseCip0119 = Aeson.withObject "CIP-0119" $ \top -> do
    mBody <- top .:? "body"
    let fields = fromMaybe top mBody
    rawRefs <- fromMaybe [] <$> (top .:? "references" <|> fields .:? "references")
    drepMetaDoNotList <- fromMaybe False <$> (top .:? "doNotList" <|> fields .:? "doNotList")
    drepMetaReferences <- mapM parseReference rawRefs
    meta <- parseBodyFields fields
    pure meta
        { drepMetaReferences = drepMetaReferences
        , drepMetaDoNotList = drepMetaDoNotList
        }

-- | Parse the scalar body fields from either a @body@ object or a flat
-- top-level object.  @drepMetaReferences@ and @drepMetaDoNotList@ are
-- placeholder values that will be overridden by the caller.
parseBodyFields :: Object -> Parser DRepMetadata
parseBodyFields body = do
    drepMetaName <- body .: "givenName"
    drepMetaObjectives <- body .:? "objectives"
    drepMetaMotivations <- body .:? "motivations"
    drepMetaQualifications <- body .:? "qualifications"
    drepMetaPaymentAddress <- body .:? "paymentAddress"
    let drepMetaDoNotList = False
        drepMetaReferences = []
    pure DRepMetadata{..}

parseReference :: Value -> Parser DRepMetaReference
parseReference = Aeson.withObject "reference" $ \o -> do
    drepMetaRefLabel <- o .: "label"
    drepMetaRefUri <- o .: "uri"
    pure DRepMetaReference{..}
