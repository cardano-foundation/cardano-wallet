{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-
Control the launching of a cluster of nodes for testing purposes.
The cluster will run on the testnet network.
-}

module Test.Integration.Framework.Cluster.Launch
    ( withLocalCluster
    ) where

import Prelude

import Cardano.Launcher
    ( withBackendCreateProcess
    )
import Cardano.Launcher.Node
    ( CardanoNodeConn
    , cardanoNodeConn
    , nodeSocketPath
    )
import Cardano.Ledger.Shelley.API
    ( ShelleyGenesis (sgNetworkMagic)
    )
import Cardano.Wallet.Faucet.Yaml
    ( saveFunds
    )
import Cardano.Wallet.Launch.Cluster
    ( ClusterEra
    , ClusterLog (MsgLauncher)
    , FaucetFunds
    , FileOf (..)
    , RunningNode
    , clusterEraToString
    )
import Cardano.Wallet.Launch.Cluster.CommandLine
    ( CommandLineOptions (..)
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( DirOf (..)
    , absFilePathOf
    , toFilePath
    )
import Cardano.Wallet.Launch.Cluster.Node.RunningNode
    ( RunningNode (..)
    )
import Control.Concurrent
    ( threadDelay
    )
import Control.Monad
    ( void
    )
import Control.Monad.Cont
    ( ContT (..)
    )
import Control.Monad.Trans
    ( lift
    )
import Control.Monitoring
    ( MonitorState
    )
import Data.Aeson
    ( FromJSON
    )
import Data.Functor.Contravariant
    ( (>$<)
    )
import GHC.Stack
    ( HasCallStack
    )
import Ouroboros.Network.Magic
    ( NetworkMagic (..)
    )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData (..)
    )
import System.Environment
    ( getEnvironment
    )
import System.IO.Extra
    ( withTempFile
    )
import System.Path
    ( absFile
    , relFile
    , (</>)
    )
import System.Process.Extra
    ( CreateProcess (..)
    , StdStream (..)
    , proc
    )

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS

localClusterProcess
    :: CommandLineOptions
    -> ClusterEra
    -> IO CreateProcess
localClusterProcess CommandLineOptions{..} era = do
    myEnv <- getEnvironment
    let envs =
            [ ("LOCAL_CLUSTER_ERA", clusterEraToString era)
            ]
    pure
        $ (proc "local-cluster" args)
            { env = Just $ myEnv ++ envs
            , -- , cwd = Just $ nodeDir cfg
              std_out = CreatePipe
            , std_err = CreatePipe
            }
  where
    args =
        [ "--cluster-configs"
        , toFilePath $ absDirOf clusterConfigsDir
        , "--faucet-funds"
        , toFilePath $ absFileOf faucetFundsFile
        ]
            <> case clusterDir of
                Nothing -> []
                Just clusterDir' ->
                    [ "--cluster"
                    , toFilePath $ absDirOf clusterDir'
                    ]

withFaucetFunds
    :: HasCallStack
    => FaucetFunds
    -> ContT r IO (FileOf s)
withFaucetFunds faucetFunds = ContT $ \action ->
    withTempFile $ \faucetFundsFile -> do
        let faucetFundsPath = FileOf $ absFile faucetFundsFile
        saveFunds faucetFundsPath faucetFunds
        action faucetFundsPath

withSocketPath
    :: HasCallStack
    => DirOf s
    -> ContT r m CardanoNodeConn
withSocketPath cfgClusterDir = ContT $ \f ->
    case cardanoNodeConn
        $ nodeSocketPath
        $ toFilePath
        $ absDirOf cfgClusterDir of
        Left err -> error $ "Failed to get socket path: " ++ err
        Right socketPath -> f socketPath

withGenesisData :: FromJSON a => FileOf "genesis-shelley" -> ContT r IO a
withGenesisData shelleyGenesis = ContT $ \f -> do
    genesisData <-
        BS.readFile (absFilePathOf shelleyGenesis)
            >>= Aeson.throwDecodeStrict
    f genesisData

withLocalCluster
    :: HasCallStack
    => Int
    -- ^ Port for monitoring the local cluster.
    -> MonitorState
    -- ^ In which state the monitoring should start.
    -> Config
    -- ^ Configuration for the cluster.
    -> FaucetFunds
    -- ^ Initial faucet funds.
    -> (RunningNode -> IO a)
    -- ^ Action to run once when all pools have started.
    -> IO a
withLocalCluster
    monitoringPort
    initialPullingState
    Config{..}
    faucetFunds
    action = do
        let
            clusterConfigsDir = cfgClusterConfigs
            shelleyGenesis =
                FileOf
                    $ absDirOf cfgClusterDir
                        </> relFile "shelley-genesis.json"
            clusterDir = Just cfgClusterDir
            pullingMode = initialPullingState
        flip runContT pure $ do
            faucetFundsFile <- withFaucetFunds faucetFunds
            socketPath <- withSocketPath cfgClusterDir
            cp <-
                lift
                    $ localClusterProcess
                        CommandLineOptions{..}
                        cfgLastHardFork
            void
                $ ContT
                $ withBackendCreateProcess
                    (MsgLauncher "local-cluster" >$< cfgTracer)
                    cp
            lift $ threadDelay 10_000_000 -- when the cluster is ready ?
            genesisData <- withGenesisData shelleyGenesis
            lift
                $ action
                $ RunningNode
                    { runningNodeSocketPath = socketPath
                    , runningNodeShelleyGenesis = genesisData
                    , runningNodeVersionData =
                        NodeToClientVersionData
                            { networkMagic =
                                NetworkMagic
                                    $ sgNetworkMagic genesisData
                            , query = False
                            }
                    }
