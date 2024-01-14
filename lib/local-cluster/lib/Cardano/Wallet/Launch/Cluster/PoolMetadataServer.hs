{-# LANGUAGE RecordWildCards #-}
module Cardano.Wallet.Launch.Cluster.PoolMetadataServer
    ( PoolMetadataServer (..)
    , withPoolMetadataServer
    )
where

import Prelude

import Cardano.BM.Tracing
    ( traceWith
    )
import Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM
    , runClusterM
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    )
import Cardano.Wallet.Launch.Cluster.Logging
    ( ClusterLog (MsgRegisteringPoolMetadata)
    )
import Control.Monad.Reader
    ( MonadIO (..)
    , MonadReader (..)
    )
import Crypto.Hash.Extra
    ( blake2b256
    )
import Data.ByteArray.Encoding
    ( Base (Base16)
    , convertToBase
    )
import Data.Tagged
    ( untag
    )
import System.Directory
    ( createDirectoryIfMissing
    )
import System.FilePath
    ( (</>)
    )
import Test.Utils.StaticServer
    ( withStaticServer
    )

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8

data PoolMetadataServer = PoolMetadataServer
    { registerMetadataForPoolIndex :: Int -> Aeson.Value -> IO ()
    , urlFromPoolIndex :: Int -> String
    }

withPoolMetadataServer
    :: (PoolMetadataServer -> ClusterM a)
    -> ClusterM a
withPoolMetadataServer action = do
    config@Config{..} <- ask
    let metadir = untag cfgClusterDir </> "pool-metadata"
    liftIO $ do
        createDirectoryIfMissing False metadir
        withStaticServer metadir $ \baseURL -> do
            let _urlFromPoolIndex i = baseURL </> metadataFileName i
            runClusterM config $ action PoolMetadataServer
                { registerMetadataForPoolIndex = \i metadata -> do
                    let metadataBytes = Aeson.encode metadata
                    BL8.writeFile (metadir </> (metadataFileName i)) metadataBytes
                    let hash = blake2b256 (BL.toStrict metadataBytes)
                    traceWith cfgTracer
                        $ MsgRegisteringPoolMetadata
                            (_urlFromPoolIndex i)
                            (B8.unpack $ convertToBase Base16 hash)
                , urlFromPoolIndex = _urlFromPoolIndex
                }
  where
    metadataFileName :: Int -> FilePath
    metadataFileName i = show i <> ".json"
