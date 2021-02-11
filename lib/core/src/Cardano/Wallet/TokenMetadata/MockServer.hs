{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- A mock metadata-server for testing, metadata requests. Created using the
-- metadata-server Haskell source code as a reference.

module Cardano.Wallet.TokenMetadata.MockServer
    ( withMetadataServer
    , queryServerStatic

    -- * Helpers
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
    , Subject (..)
    , Subject
    , SubjectProperties (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Data.Aeson
    ( eitherDecodeFileStrict )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.HashMap.Strict
    ( HashMap )
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

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as T

{-------------------------------------------------------------------------------
                              Mock metadata-server
-------------------------------------------------------------------------------}

-- | The batch query API, excerpted from
-- @metadata-server/metadata-lib/src/Cardano/Metadata/Server/API.hs@.
type MetadataQueryApi = "metadata" :> "query"
    :> ReqBody '[JSON] BatchRequest :> Post '[JSON] BatchResponse

-- | Start a metadata server.
--
-- To be used with @queryServerStatic@.
withMetadataServer
    :: IO (Server MetadataQueryApi)
    -> (TokenMetadataServer -> IO a)
    -> IO a
withMetadataServer mkServer action = withApplication app (action . mkUrl)
  where
    app = serve (Proxy @MetadataQueryApi) <$> mkServer
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
    let m = HM.fromList [(view #subject md, md) | md <- mds]
    pure $ queryHandlerBase m

queryHandlerBase
    :: HashMap Subject SubjectProperties
    -> BatchRequest
    -> Handler BatchResponse
queryHandlerBase index = Handler . ExceptT . pure . Right .
    BatchResponse . mapMaybe (`HM.lookup` index) . subjects

-- | The reverse of subjectToAssetId
assetIdFromSubject :: Subject -> AssetId
assetIdFromSubject =
    mk . BS.splitAt 32 . unsafeFromHex . T.encodeUtf8 . unSubject
  where
    mk (p, n) = AssetId (UnsafeTokenPolicyId (Hash p)) (UnsafeTokenName n)
