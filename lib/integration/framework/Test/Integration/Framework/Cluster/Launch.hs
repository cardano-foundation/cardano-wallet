{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

{-
Control the launching of a cluster of nodes for testing purposes.
The cluster will run on the testnet network.
-}

module Test.Integration.Framework.Cluster.Launch
    ( withLocalCluster
    ) where

import Prelude

import Cardano.Launcher
    ( ProcessRun (..)
    , withBackendCreateProcess
    )
import Cardano.Launcher.Node
    ( cardanoNodeConn
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
    , toFilePath
    )
import Cardano.Wallet.Launch.Cluster.Node.RunningNode
    ( RunningNode (..)
    )
import Control.Concurrent
    ( threadDelay
    )
import Control.Exception
    ( throwIO
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
        , toFilePath $ absDirOf  clusterConfigsDir
        , "--faucet-funds"
        , toFilePath $ absFileOf faucetFundsFile
        ]
            <> case clusterDir of
                Nothing -> []
                Just clusterDir' ->
                    [ "--cluster"
                    , toFilePath $ absDirOf clusterDir'
                    ]

withLocalCluster
    :: HasCallStack
    => Config
    -> FaucetFunds
    -> (RunningNode -> IO a)
    -- ^ Action to run once when all pools have started.
    -> IO a
withLocalCluster Config{..} faucetFunds run = do
    r <- withTempFile $ \faucetFundsPath -> do
        let faucetFundsFile = FileOf $ absFile faucetFundsPath
            clusterConfigsDir = cfgClusterConfigs
            shelleyGenesis = absDirOf cfgClusterDir
                </> relFile "shelley-genesis.json"
            clusterDir = Just cfgClusterDir
        saveFunds (faucetFundsFile) faucetFunds
        case cardanoNodeConn $ nodeSocketPath $ toFilePath $ absDirOf cfgClusterDir of
            Left err -> error $ "Failed to get socket path: " ++ err
            Right socketPath -> do
                cp <- localClusterProcess CommandLineOptions{..} cfgLastHardFork
                withBackendCreateProcess
                    (MsgLauncher "local-cluster" >$< cfgTracer)
                    cp
                    $ ProcessRun
                    $ \_ _mout _merr _ -> do
                        threadDelay 10_000_000 -- when the cluster is ready ?
                        genesisData <-
                            BS.readFile (toFilePath shelleyGenesis)
                                >>= Aeson.throwDecodeStrict
                        let networkMagic =
                                NetworkMagic
                                    $ sgNetworkMagic genesisData
                            runningNode =
                                RunningNode
                                    { runningNodeSocketPath = socketPath
                                    , runningNodeShelleyGenesis = genesisData
                                    , runningNodeVersionData =
                                        NodeToClientVersionData
                                            { networkMagic
                                            , query = False
                                            }
                                    }
                        run runningNode
    either throwIO pure r
