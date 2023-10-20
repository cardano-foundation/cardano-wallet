{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
    , withMetadataServerOptions
    , queryServerStatic
    , queryServerReloading

    -- * Helpers
    , assetIdFromSubject
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( TokenMetadataServer (..)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..)
    )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( AssetDecimals (..)
    , AssetLogo (..)
    , AssetURL (..)
    , TokenName (..)
    , TokenPolicyId (..)
    )
import Cardano.Wallet.TokenMetadata
    ( BatchRequest (..)
    , BatchResponse (..)
    , Property (..)
    , PropertyName
    , PropertyValue
    , Signature (..)
    , Subject (..)
    , SubjectProperties (..)
    , propertyName
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value (..)
    , eitherDecodeFileStrict
    , object
    , (.=)
    )
import Data.ByteArray.Encoding
    ( Base (Base16, Base64)
    , convertToBase
    )
import Data.Function
    ( (&)
    )
import Data.Generics.Internal.VL.Lens
    ( view
    )
import Data.HashSet
    ( HashSet
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Proxy
    ( Proxy (..)
    )
import GHC.TypeLits
    ( KnownSymbol
    )
import Network.URI
    ( parseURI
    )
import Network.Wai
    ( Middleware
    )
import Network.Wai.Handler.Warp
    ( Port
    , defaultSettings
    , runSettings
    , setBeforeMainLoop
    , setPort
    , withApplication
    )
import Servant.API
    ( (:>)
    , JSON
    , Post
    , ReqBody
    )
import Servant.Server
    ( Handler (..)
    , Server
    , serve
    )
import UnliftIO.Async
    ( race
    )
import UnliftIO.Exception
    ( throwString
    )
import UnliftIO.MVar
    ( newEmptyMVar
    , putMVar
    , takeMVar
    )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashSet as Set
import qualified Data.Text.Encoding as T

{-------------------------------------------------------------------------------
                              Mock metadata-server
-------------------------------------------------------------------------------}

-- | The batch query API, excerpted from
-- @metadata-server/metadata-lib/src/Cardano/Metadata/Server/API.hs@.
type MetadataQueryApi = "metadata" :> "query"
    :> ReqBody '[JSON] BatchRequest :> Post '[JSON] BatchResponse

-- | Start a metadata server on any random port. See 'withMetadataServerOptions'
-- for running a server with middleware and predefined port.
--
-- To be used with @queryServerStatic@.
withMetadataServer
    :: IO (Server MetadataQueryApi)
    -> (TokenMetadataServer -> IO a)
    -> IO a
withMetadataServer = withMetadataServerOptions id Nothing

-- | Start a metadata server.
withMetadataServerOptions
    :: Middleware
    -> Maybe Port
    -> IO (Server MetadataQueryApi)
    -> (TokenMetadataServer -> IO a)
    -> IO a
withMetadataServerOptions middleware mPort mkServer action = case mPort of
    Just port -> do
        app <- mkApp
        started <- newEmptyMVar
        let settings = defaultSettings
                & setBeforeMainLoop (putMVar started port)
                & setPort port
        race (runSettings settings app) (takeMVar started >>= action') >>= \case
            Right a -> pure a
            Left () -> throwString "Unexpected: runSettings exited"

    Nothing -> withApplication mkApp action'
  where
    mkApp = middleware . serve (Proxy @MetadataQueryApi) <$> mkServer
    mkUrl port = TokenMetadataServer
        $ fromMaybe (error "withMetadataServer: bad uri")
        $ parseURI
        $ "http://localhost:" ++ show port ++ "/"
    action' = action . mkUrl

-- | Serve a json file.
--
-- Will filter the json and only serve metadata for the requested subjects.
queryServerStatic :: FilePath -> IO (BatchRequest -> Handler BatchResponse)
queryServerStatic golden = do
    db <- either (error . show) id <$> eitherDecodeFileStrict golden
    pure (pure . handler db)
  where
    handler (BatchResponse db) (BatchRequest subs props) = BatchResponse $
        filterResponse (Set.fromList subs) (Set.fromList props) db

    filterResponse
        :: HashSet Subject
        -> HashSet PropertyName
        -> [SubjectProperties]
        -> [SubjectProperties]
    filterResponse subs props = map filterProps . filter inSubs
      where
        filterProps (SubjectProperties subj own (a, b, c, d, e, f)) =
            SubjectProperties subj own
            (inProps a, inProps b, inProps c, inProps d, inProps e, inProps f)

        inSubs sp = (view #subject sp) `Set.member` subs

        inProps :: KnownSymbol name => Maybe (Property name) -> Maybe (Property name)
        inProps (Just p) = if (propertyName p) `Set.member` props then Just p else Nothing
        inProps Nothing = Nothing

-- | Like 'queryServerStatic', except that the JSON file will be reloaded before
-- every request.
queryServerReloading :: FilePath -> BatchRequest -> Handler BatchResponse
queryServerReloading golden req = do
    handler <- liftIO $ queryServerStatic golden
    handler req

-- | The reverse of subjectToAssetId
assetIdFromSubject :: Subject -> AssetId
assetIdFromSubject =
    mk . BS.splitAt 32 . unsafeFromHex . T.encodeUtf8 . unSubject
  where
    mk (p, n) = AssetId (UnsafeTokenPolicyId (Hash p)) (UnsafeTokenName n)

{-------------------------------------------------------------------------------
                              JSON orphans
-------------------------------------------------------------------------------}

instance FromJSON BatchRequest where

instance ToJSON SubjectProperties where
   toJSON (SubjectProperties s o (n,d,a,u,l,dec)) = object $
       [ "subject" .= s
       , "owner" .= o
       ] ++ optionals
       [ "name" .= n
       , "description" .= d
       , "ticker" .= a
       , "url" .= u
       , "logo" .= l
       , "decimals" .= dec
       ]
     where
       optionals = filter ((/= Null) . snd)

instance ToJSON (PropertyValue name) => ToJSON (Property name) where
    toJSON (Property v s c) = object
        [ "value" .= either snd toJSON v
        , "signatures" .= s
        , "sequenceNumber" .= c
        ]

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

instance ToJSON AssetLogo where
    toJSON = toJSON . B8.unpack . convertToBase Base64 . unAssetLogo

instance ToJSON AssetURL where
    toJSON = toJSON . show . unAssetURL

instance ToJSON AssetDecimals where
  toJSON = toJSON . unAssetDecimals
