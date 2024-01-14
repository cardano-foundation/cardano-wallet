{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
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
    ( NodeSegment
    )
import Control.Monad.Reader
    ( MonadIO (..)
    )
import Data.Aeson
    ( (.=)
    )
import Data.Tagged
    ( Tagged (..)
    )
import System.FilePath
    ( (</>)
    )

import qualified Data.Aeson as Aeson

-- | Generate a topology file from a list of peers.
genTopology
    :: NodeSegment
    -> [Int]
    -> ClusterM (Tagged "topology" FilePath)
genTopology nodeSegment peers = do
    nodeDir <- askNodeDir nodeSegment
    let file = nodeDir </> "node.topology"
    liftIO
        $ Aeson.encodeFile file
        $ Aeson.object ["Producers" .= map encodePeer peers]
    pure $ Tagged @"topology" file
  where
    encodePeer :: Int -> Aeson.Value
    encodePeer port =
        Aeson.object
            [ "addr" .= ("127.0.0.1" :: String)
            , "port" .= port
            , "valency" .= (1 :: Int)
            ]
