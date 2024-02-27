{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.Node.Relay
    ( withRelayNode
    )
where

import Prelude

import Cardano.Launcher.Node
    ( CardanoNodeConfig (..)
    , NodePort (..)
    )
import Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM
    , askNodeDir
    , bracketTracer'
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( NodePathSegment (..)
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( FileOf (..)
    )
import Cardano.Wallet.Launch.Cluster.Logging
    ( setLoggingName
    )
import Cardano.Wallet.Launch.Cluster.Node.GenNodeConfig
    ( genNodeConfig
    )
import Cardano.Wallet.Launch.Cluster.Node.GenTopology
    ( genTopology
    )
import Cardano.Wallet.Launch.Cluster.Node.NodeParams
    ( NodeParams (..)
    )
import Cardano.Wallet.Launch.Cluster.Node.Process
    ( withCardanoNodeProcess
    )
import Cardano.Wallet.Launch.Cluster.Node.RunningNode
    ( RunningNode (..)
    )
import Control.Monad.Reader
    ( MonadIO (..)
    )
import Data.Tagged
    ( Tagged (..)
    )
import System.Directory
    ( createDirectory
    )

-- | Launches a @cardano-node@ with the given configuration which will not forge
-- blocks, but has every other cluster node as its peer. Any transactions
-- submitted to this node will be broadcast to every node in the cluster.
--
-- Connecting wallet to a block-producing (pool) node could cause problems
-- with the block production: wallet sends resource-heavy queries and that
-- causes timeout and breaks connection with other nodes;
--
-- Connectiong wallet to a non-block producing (relay) node allows to avoid
-- such problems.
withRelayNode
    :: NodeParams
    -- ^ Parameters used to generate config files.
    -> (RunningNode -> IO a)
    -- ^ Callback function with socket path
    -> ClusterM a
withRelayNode params onClusterStart = do
    let name = "node"
        nodeSegment = NodePathSegment name
    nodeDir' <- askNodeDir nodeSegment
    let NodeParams genesisFiles hardForks (port, peers) logCfg _ = params
    bracketTracer' "withRelayNode" $ do
        liftIO $ createDirectory nodeDir'

        let logCfg' = setLoggingName name logCfg
        (config, genesisData, vd) <-
            genNodeConfig
                nodeSegment
                (Tagged @"node-name" "-relay")
                genesisFiles
                hardForks
                logCfg'
        topology <- genTopology nodeSegment peers

        let cfg =
                CardanoNodeConfig
                    { nodeDir = nodeDir'
                    , nodeConfigFile = pathOf config
                    , nodeTopologyFile = pathOf topology
                    , nodeDatabaseDir = "db"
                    , nodeDlgCertFile = Nothing
                    , nodeSignKeyFile = Nothing
                    , nodeOpCertFile = Nothing
                    , nodeKesKeyFile = Nothing
                    , nodeVrfKeyFile = Nothing
                    , nodePort = Just (NodePort port)
                    , nodeLoggingHostname = Just name
                    , nodeExecutable = Nothing
                    , nodeOutputFile = nodeParamsOutputFile params
                    }

        let onClusterStart' socket = onClusterStart (RunningNode socket genesisData vd)
        withCardanoNodeProcess name cfg onClusterStart'
