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
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( TokenMetadataServer (..) )
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
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), eitherDecodeFileStrict, object, (.=) )
import Data.ByteArray.Encoding
    ( Base (Base16), convertToBase )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Map
    ( Map )
import Data.Maybe
    ( fromMaybe, mapMaybe )
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

import qualified Data.Map as Map
import qualified Data.Text.Encoding as T

{-------------------------------------------------------------------------------
                              Mock metadata-server
-------------------------------------------------------------------------------}

-- | Start a metadata server.
--
-- To be used with @queryServerStatic@.
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

-- | Serve a json file.
--
-- Will filter the json and only serve metadata for the requested subjects.
queryServerStatic :: FilePath -> IO (Server MetadataQueryApi)
queryServerStatic golden = do
    BatchResponse mds <- either (error . show) id
        <$> eitherDecodeFileStrict golden
    let m = Map.fromList $ map (\x -> (view #subject x, x)) mds
    pure $ queryHandlerBase m
  where
    queryHandlerBase :: Map Subject SubjectProperties -> BatchRequest -> Handler BatchResponse
    queryHandlerBase store = do
        Handler . ExceptT . pure . Right . BatchResponse . mapMaybe respond . view #subjects
      where
        respond = (`Map.lookup` store)

type MetadataQueryApi = "metadata" :> "query"
    :> ReqBody '[JSON] BatchRequest :> Post '[JSON] BatchResponse

queryApi :: Proxy MetadataQueryApi
queryApi = Proxy

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
