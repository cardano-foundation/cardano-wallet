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
import Cardano.Wallet.Launch.Cluster.FileOf
    ( DirOf (..)
    , FileOf (..)
    , toFilePath
    )
import Control.Monad.Reader
    ( MonadIO (..)
    )
import Data.Aeson
    ( (.=)
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
    let file = nodeDir </> relFile "node" <.> "topology"
    liftIO
        $ Aeson.encodeFile (toFilePath file)
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
