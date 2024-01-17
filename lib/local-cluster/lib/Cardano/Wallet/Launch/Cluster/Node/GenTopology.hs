{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.Node.GenTopology
    ( genTopology
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM
    , askNodeDir
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( NodePathSegment
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( FileOf (..)
    )
import Control.Monad.Reader
    ( MonadIO (..)
    )
import Data.Aeson
    ( (.=)
    )
import System.FilePath
    ( (</>)
    )

import qualified Data.Aeson as Aeson

-- | Generate a topology file from a list of peers.
genTopology
    :: NodePathSegment
    -> [Int]
    -> ClusterM (FileOf "topology")
genTopology nodeSegment peers = do
    nodeDir <- askNodeDir nodeSegment
    let file = nodeDir </> "node.topology"
    liftIO
        $ Aeson.encodeFile file
        $ Aeson.object ["Producers" .= map encodePeer peers]
    pure $ FileOf @"topology" file
  where
    encodePeer :: Int -> Aeson.Value
    encodePeer port =
        Aeson.object
            [ "addr" .= ("127.0.0.1" :: String)
            , "port" .= port
            , "valency" .= (1 :: Int)
            ]
