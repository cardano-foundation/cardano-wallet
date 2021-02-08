{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.TokenMetadata.MockServer
    ( withMetadataServer
    , queryServer
    , queryServerStatic
    , assetIdFromSubject
    ) where

import Prelude

import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Cardano.Wallet.Primitive.Types.Hash (Hash (..))
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( AssetMetadata (..), TokenPolicyId (..), TokenName (..) )
import Cardano.Wallet.TokenMetadata
    ( BatchRequest(..),
      BatchResponse,
      Signature(..),
      Subject (..),
      PropertyValue,
      Subject,
      Property(..),
      SubjectProperties(..) )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), object, (.=), eitherDecodeFileStrict )
import Data.ByteArray.Encoding
    ( Base (Base16), convertToBase )
import Data.Proxy
    ( Proxy (..) )
import Network.Wai.Handler.Warp
    ( withApplication )
import Network.URI (URI, parseURI)
import Servant.API ( JSON, ReqBody, type (:>), Post )
import Servant.Server
    ( Server, serve, Handler (..) )
import Control.Monad.Trans.Except (ExceptT(..))
import Data.Maybe (fromJust, fromMaybe)

import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS

{-------------------------------------------------------------------------------
                              Mock metadata-server
-------------------------------------------------------------------------------}

type MetadataQueryApi = "metadata" :> "query"
    :> ReqBody '[JSON] BatchRequest :> Post '[JSON] BatchResponse

-- | Serve a list of metadata.
queryServer :: [(AssetId, AssetMetadata)] -> Server MetadataQueryApi
queryServer md = pure . map respond . subjects
  where
    respond subj = case lookup (assetIdFromSubject subj) md of
        Nothing -> error "doh"
        Just _md -> error "todo"

assetIdFromSubject :: Subject -> AssetId
assetIdFromSubject = mk . BS.splitAt 32 . unsafeFromHex . T.encodeUtf8 . unSubject
  where
    mk (p, n) = AssetId (UnsafeTokenPolicyId (Hash p)) (UnsafeTokenName n)

-- | Serve a json file.
queryServerStatic :: FilePath -> IO (Server MetadataQueryApi)
queryServerStatic golden = do
    Right mds <- eitherDecodeFileStrict golden
    pure $ queryHandlerBase mds

queryHandlerBase :: [SubjectProperties] -> BatchRequest -> Handler BatchResponse
queryHandlerBase ps = Handler . ExceptT . pure . Right . map respond . subjects
  where
    subs = zip (map subject ps) ps
    respond subj = fromJust $ lookup subj subs

queryApi :: Proxy MetadataQueryApi
queryApi = Proxy

withMetadataServer :: IO (Server MetadataQueryApi) -> (URI -> IO a) -> IO a
withMetadataServer srv action = withApplication app (action . mkUrl)
  where
    app = serve queryApi <$> srv
    mkUrl port = fromMaybe (error "withMetadataServer: bad uri")
        $ parseURI
        $ "http://localhost:" ++ show port ++ "/"

{-------------------------------------------------------------------------------
                              JSON orphans
-------------------------------------------------------------------------------}

instance FromJSON BatchRequest where

instance ToJSON SubjectProperties where
   toJSON (SubjectProperties s o (n, d)) = object
       [ "subject" .= s
       , "owner" .= o
       , "name" .= n
       , "description" .= d
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
