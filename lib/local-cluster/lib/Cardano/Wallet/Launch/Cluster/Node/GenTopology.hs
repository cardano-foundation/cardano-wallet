{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.Node.GenTopology
    ( genTopology
    )
where

import Prelude

import Data.Aeson
    ( (.=)
    )
import Data.Tagged
    ( Tagged (..)
    , untag
    )
import System.FilePath
    ( (</>)
    )

import qualified Data.Aeson as Aeson

-- | Generate a topology file from a list of peers.
genTopology
    :: Tagged "output" FilePath -> [Int] -> IO (Tagged "topology" FilePath)
genTopology outputDir peers = do
    let file = untag outputDir </> "node.topology"
    Aeson.encodeFile file $ Aeson.object ["Producers" .= map encodePeer peers]
    pure $ Tagged @"topology" file
  where
    encodePeer :: Int -> Aeson.Value
    encodePeer port =
        Aeson.object
            [ "addr" .= ("127.0.0.1" :: String)
            , "port" .= port
            , "valency" .= (1 :: Int)
            ]
