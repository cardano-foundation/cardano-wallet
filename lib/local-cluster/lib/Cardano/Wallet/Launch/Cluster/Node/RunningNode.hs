{-# LANGUAGE DerivingStrategies #-}
module Cardano.Wallet.Launch.Cluster.Node.RunningNode
    ( RunningNode (..)
    )
where

import Prelude

import Cardano.Launcher.Node
    ( CardanoNodeConn
    )
import Cardano.Ledger.Shelley.API
    ( ShelleyGenesis
    )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData
    )

-- | Information about a launched node.
data RunningNode = RunningNode
    { runningNodeSocketPath :: CardanoNodeConn
    , runningNodeShelleyGenesis :: ShelleyGenesis
    , runningNodeVersionData :: NodeToClientVersionData
    } deriving stock (Show, Eq)
