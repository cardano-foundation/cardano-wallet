{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.Node.GenTopology
    ( genTopology
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.ClusterEra
    ( ClusterEra (..)
    )
import Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM
    , askNodeDir
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( DirOf (..)
    , FileOf (..)
    , toFilePath
    )
import Control.Monad.Reader
    ( MonadIO (..)
    , asks
    )
import Data.Aeson
    ( ToJSON (..)
    , (.=)
    )
import System.Path
    ( RelDir
    , relFile
    , (<.>)
    , (</>)
    )

import qualified Data.Aeson as Aeson

-- | Generate a topology file from a list of peers.
genTopology
    :: RelDir
    -> [Int]
    -> ClusterM (FileOf "topology")
genTopology nodeSegment peers = do
    DirOf nodeDir <- askNodeDir nodeSegment
    clusterEra <- asks cfgLastHardFork
    let file = nodeDir </> relFile "config" <.> "json"
    liftIO
        $ Aeson.encodeFile (toFilePath file)
        $ case clusterEra of
            BabbageHardFork -> encodeLegacyTopology peers
            ConwayHardFork -> encodeP2PTopology peers
    pure $ FileOf @"topology" file

encodeP2PTopology :: [Int] -> Aeson.Value
encodeP2PTopology peers = encodeTopology
  where
    encodePeer :: Int -> Aeson.Value
    encodePeer port =
        Aeson.object
            [ "address" .= ("127.0.0.1" :: String)
            , "port" .= port
            ]
    encodePeers :: Aeson.Value
    encodePeers = Aeson.object
        [ "accessPoints" .= toJSON (encodePeer <$> peers)
        , "valency" .= length peers
        , "advertise" .= False
        ]
    encodeTopology ::  Aeson.Value
    encodeTopology = Aeson.object
        [ "localRoots" .= toJSON [encodePeers]
        , "publicRoots" .= toJSON @[()] []
        , "useLedgerAfterSlot" .= toJSON @Int (-1)
        ]

encodeLegacyTopology :: [Int] -> Aeson.Value
encodeLegacyTopology peers = encodeTopology
  where
    encodeTopology = Aeson.object
        ["Producers" .= map encodePeer peers]
    encodePeer :: Int -> Aeson.Value
    encodePeer port =
        Aeson.object
            [ "addr" .= ("127.0.0.1" :: String)
            , "port" .= port
            , "valency" .= (1 :: Int)
            ]
