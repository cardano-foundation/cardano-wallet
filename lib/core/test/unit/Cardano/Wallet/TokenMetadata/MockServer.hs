{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.TokenMetadata.MockServer
    ( withMetadataServer
    , queryServerStatic
    , assetIdFromSubject
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( TokenMetadataServer (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..), TokenPolicyId (..) )
import Cardano.Wallet.TokenMetadata
    ( BatchRequest (..)
    , BatchResponse (..)
    , Property (..)
    , PropertyValue
    , Signature (..)
    , Subject (..)
    , Subject
    , SubjectProperties (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), eitherDecodeFileStrict, object, (.=) )
import Data.ByteArray.Encoding
    ( Base (Base16), convertToBase )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Maybe
    ( fromJust, fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Network.URI
    ( parseURI )
import Network.Wai.Handler.Warp
    ( withApplication )
import Servant.API
    ( (:>), JSON, Post, ReqBody )
import Servant.Server
    ( Handler (..), Server, serve )

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

{-------------------------------------------------------------------------------
                              Mock metadata-server
-------------------------------------------------------------------------------}

type MetadataQueryApi = "metadata" :> "query"
    :> ReqBody '[JSON] BatchRequest :> Post '[JSON] BatchResponse

assetIdFromSubject :: Subject -> AssetId
assetIdFromSubject = mk . BS.splitAt 32 . unsafeFromHex . T.encodeUtf8 . unSubject
  where
    mk (p, n) = AssetId (UnsafeTokenPolicyId (Hash p)) (UnsafeTokenName n)

-- | Serve a json file.
queryServerStatic :: FilePath -> IO (Server MetadataQueryApi)
queryServerStatic golden = do
    Right (BatchResponse mds) <- eitherDecodeFileStrict golden
    pure $ queryHandlerBase mds

queryHandlerBase :: [SubjectProperties] -> BatchRequest -> Handler BatchResponse
queryHandlerBase ps = Handler . ExceptT . pure . Right . BatchResponse . map respond . view #subjects
  where
    subs = zip (map subject ps) ps
    respond subj = fromJust $ lookup subj subs

queryApi :: Proxy MetadataQueryApi
queryApi = Proxy

withMetadataServer
    :: IO (Server MetadataQueryApi)
    -> (TokenMetadataServer -> IO a)
    -> IO a
withMetadataServer mkServer action = withApplication app (action . mkUrl)
  where
    app = serve queryApi <$> mkServer
    mkUrl port = TokenMetadataServer
        $ fromMaybe (error "withMetadataServer: bad uri")
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

instance ToJSON BatchResponse where
    toJSON (BatchResponse subs)= object
        [ "subjects" .= subs
        ]
