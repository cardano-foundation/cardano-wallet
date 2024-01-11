{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.Node.Relay
    ( withRelayNode
    )
where

import Prelude

import Cardano.BM.Tracing
    ( Tracer
    )
import Cardano.Launcher.Node
    ( CardanoNodeConfig (..)
    , NodePort (..)
    )
import Cardano.Wallet.Launch.Cluster.Logging
    ( ClusterLog
    , bracketTracer'
    , setLoggingName
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
import Data.Tagged
    ( Tagged (Tagged)
    , untag
    )
import System.Directory
    ( createDirectory
    )
import System.FilePath
    ( (</>)
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
    :: Tracer IO ClusterLog
    -- ^ Trace for subprocess control logging
    -> Tagged "cluster" FilePath
    -- ^ Parent state directory.
    -- Node data will be created in a subdirectory of this.
    -> Tagged "cluster-configs" FilePath
    -> NodeParams
    -- ^ Parameters used to generate config files.
    -> (RunningNode -> IO a)
    -- ^ Callback function with socket path
    -> IO a
withRelayNode tr clusterDir setupDir params onClusterStart = do
    let name = "node"
    let nodeDir' = Tagged @"output" $ untag clusterDir </> name
    let NodeParams genesisFiles hardForks (port, peers) logCfg = params
    bracketTracer' tr "withRelayNode" $ do
        createDirectory $ untag nodeDir'

        let logCfg' = setLoggingName name logCfg
        (config, genesisData, vd) <-
            genNodeConfig
                nodeDir'
                setupDir
                (Tagged @"node-name" "-relay")
                genesisFiles hardForks logCfg'
        topology <- genTopology nodeDir' peers

        let cfg =
                CardanoNodeConfig
                    { nodeDir = untag nodeDir'
                    , nodeConfigFile = untag config
                    , nodeTopologyFile = untag topology
                    , nodeDatabaseDir = "db"
                    , nodeDlgCertFile = Nothing
                    , nodeSignKeyFile = Nothing
                    , nodeOpCertFile = Nothing
                    , nodeKesKeyFile = Nothing
                    , nodeVrfKeyFile = Nothing
                    , nodePort = Just (NodePort port)
                    , nodeLoggingHostname = Just name
                    , nodeExecutable = Nothing
                    }

        let onClusterStart' socket = onClusterStart (RunningNode socket genesisData vd)
        withCardanoNodeProcess tr name cfg onClusterStart'
