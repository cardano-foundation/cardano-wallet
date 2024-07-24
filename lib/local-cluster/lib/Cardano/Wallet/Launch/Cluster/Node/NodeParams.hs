{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module Cardano.Wallet.Launch.Cluster.Node.NodeParams
    ( NodeParams (..)
    , singleNodeParams
    )
where

import Prelude

import Cardano.BM.Tracing
    ( Severity
    )
import Cardano.Launcher.Node
    ( MaybeK
    )
import Cardano.Wallet.Launch.Cluster.ClusterEra
    ( ClusterEra (BabbageHardFork)
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( OsNamedPipe
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( DirOf (..)
    , FileOf (..)
    )
import Cardano.Wallet.Launch.Cluster.Logging
    ( LogFileConfig (..)
    )
import Cardano.Wallet.Launch.Cluster.Node.GenesisFiles
    ( GenesisFiles
    )

-- | Configuration parameters which update the @node.config@ test data file.
data NodeParams d = NodeParams
    { nodeGenesisFiles :: GenesisFiles
    -- ^ Genesis block start time
    , nodeHardForks :: ClusterEra
    -- ^ Era to hard fork into.
    , nodePeers :: (Int, [Int])
    -- ^ A list of ports used by peers and this node
    , nodeLogConfig :: LogFileConfig DirOf
    -- ^ The node will always log to "cardano-node.log" relative to the
    -- config. This option can set the minimum severity and add another output
    -- file.
    , nodeParamsOutputFile :: Maybe (FileOf "node-output")
    , nodeSocket :: MaybeK d OsNamedPipe
    }
    deriving stock (Show)

singleNodeParams
    :: GenesisFiles
    -> Severity
    -> Maybe (DirOf "node-logs", Severity)
    -> Maybe (FileOf "node-output")
    -> MaybeK d OsNamedPipe
    -> NodeParams d
singleNodeParams genesisFiles severity extraLogFile =
    NodeParams genesisFiles BabbageHardFork (0, []) LogFileConfig
        { minSeverityTerminal = severity
        , extraLogDir = fmap fst extraLogFile
        , minSeverityFile = maybe severity snd extraLogFile
        }
