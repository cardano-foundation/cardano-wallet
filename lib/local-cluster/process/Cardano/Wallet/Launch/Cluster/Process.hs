{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Evaluate" #-}

module Cardano.Wallet.Launch.Cluster.Process
    ( withLocalCluster
    , EnvVars (..)
    , WalletPresence (..)
    , defaultEnvVars

      -- * Re-exports
    , RunMonitorQ (..)
    , RunFaucetQ (..)
    , FaucetQ (..)
    , SendFaucetAssets (..)
    , waitForRunningNode
    ) where

import Prelude

import Cardano.BM.ToTextTracer
    ( ToTextTracer (..)
    , logHandleFromFilePath
    , withToTextTracer
    )
import Cardano.Launcher
    ( Command (..)
    , IfToSendSigINT (..)
    , StdStream (..)
    , TimeoutInSecs (..)
    , withBackendProcess
    )
import Cardano.Wallet.Launch.Cluster
    ( FaucetFunds
    , FileOf (..)
    )
import Cardano.Wallet.Launch.Cluster.CommandLine
    ( WalletPresence (..)
    )
import Cardano.Wallet.Launch.Cluster.Faucet.Serialize
    ( saveFunds
    )
import Cardano.Wallet.Launch.Cluster.Http.Faucet.Client
    ( FaucetQ (..)
    , RunFaucetQ (..)
    )
import Cardano.Wallet.Launch.Cluster.Http.Faucet.SendFaucetAssets
    ( SendFaucetAssets (..)
    )
import Cardano.Wallet.Launch.Cluster.Http.Monitor.Client
    ( RunMonitorQ (..)
    , waitForRunningNode
    )
import Cardano.Wallet.Launch.Cluster.Http.Service
    ( withServiceClient
    )
import Cardano.Wallet.Network.Ports
    ( PortNumber
    , getRandomPort
    )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkId (..)
    , withSNetworkId
    )
import Control.Monad
    ( forM_
    )
import Control.Monad.Cont
    ( ContT (..)
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Tracer
    ( nullTracer
    )
import System.Directory
    ( createDirectoryIfMissing
    )
import System.Environment
    ( lookupEnv
    )
import System.FilePath
    ( (<.>)
    , (</>)
    )
import System.IO.Extra
    ( withTempFile
    )
import System.Path
    ( absFile
    )

data EnvVars = EnvVars
    { clusterConfigsPath :: String
    , clusterLogsFilePath :: String
    , clusterLogsMinSeverity :: String
    }

defaultEnvVars :: EnvVars
defaultEnvVars =
    EnvVars
        { clusterConfigsPath = "LOCAL_CLUSTER_CONFIGS"
        , clusterLogsFilePath = "CLUSTER_LOGS_DIR_PATH"
        , clusterLogsMinSeverity = "CLUSTER_LOGS_MIN_SEVERITY"
        }

getClusterConfigsPathFromEnv :: EnvVars -> IO FilePath
getClusterConfigsPathFromEnv environmentVars = do
    mp <- lookupEnv $ clusterConfigsPath environmentVars
    case mp of
        Just path -> pure path
        Nothing -> error "LOCAL_CLUSTER_CONFIGS not set"

getClusterLogsFilePathFromEnv :: EnvVars -> IO (Maybe FilePath)
getClusterLogsFilePathFromEnv environmentVars = do
    mp <- lookupEnv $ clusterLogsFilePath environmentVars
    forM_ mp $ \dir ->
        createDirectoryIfMissing True dir
    pure mp

getClusterLogsMinSeverity :: EnvVars -> IO (Maybe String)
getClusterLogsMinSeverity environmentVars =
    lookupEnv $ clusterLogsMinSeverity environmentVars

-- | Start a local cluster with the given name and initial faucet funds.
-- The cluster will be started in the background and the function will return
-- a pair of functions to query the cluster and a tracer to log as the process
withLocalCluster
    :: FilePath
    -- ^ name of the cluster
    -> WalletPresence
    -- ^ whether to start the wallet
    -> EnvVars
    -- ^ environment variables that have to be used
    -> FaucetFunds
    -- ^ initial faucet funds
    -> ContT () IO ((RunMonitorQ IO, RunFaucetQ IO), ToTextTracer)
withLocalCluster name walletOption envs faucetFundsValue = do
    port <- liftIO getRandomPort
    faucetFundsPath <- ContT withTempFile
    liftIO $ saveFunds (FileOf $ absFile faucetFundsPath) faucetFundsValue
    (logsPathName, command) <-
        localClusterCommand name walletOption envs port faucetFundsPath
    ToTextTracer processLogs <- case logsPathName of
        Nothing -> pure $ ToTextTracer nullTracer
        Just path ->
            withToTextTracer (Right $ path <> "-process" <.> "log") Nothing
    _ <-
        ContT
            $ withBackendProcess
                processLogs
                command
                NoTimeout
                DoNotSendSigINT
    queries <- withSNetworkId (NTestnet 42)
        $ \network -> withServiceClient network port nullTracer
    pure (queries, ToTextTracer processLogs)

-- | Generate a command to start a local cluster with the given the local cluster
-- name, monitoring port, and faucet funds path.
localClusterCommand
    :: FilePath
    -- ^ filename to append to the logs dir
    -> WalletPresence
    -- ^ whether to start the wallet
    -> EnvVars
    -- ^ environment variables that have to be used
    -> PortNumber
    -- ^ monitoring port
    -> FilePath
    -- ^ faucet funds path
    -> ContT r IO (Maybe FilePath, Command)
localClusterCommand name walletOptions envs port faucetFundsPath = do
    configsPath <- liftIO $ getClusterConfigsPathFromEnv envs
    mLogsPath <- liftIO $ getClusterLogsFilePathFromEnv envs
    mMinSeverity <- liftIO $ getClusterLogsMinSeverity envs
    (clusterStdout, logsPathName) <- case mLogsPath of
        Nothing -> pure (Inherit, Nothing)
        Just logsPath -> do
            let logsPathName = logsPath </> name
            fmap (\h -> (UseHandle h, Just logsPathName))
                $ logHandleFromFilePath
                $ logsPath </> name <> "-stdout" <.> "log"

    pure
        $ (logsPathName,)
        $ Command
            { cmdName = "local-cluster"
            , cmdArgs =
                [ "--faucet-funds"
                , faucetFundsPath
                , "--monitoring-port"
                , show port
                , "--cluster-configs"
                , configsPath
                ]
                    <> case mLogsPath of
                        Nothing -> []
                        Just logsPath ->
                            [ "--cluster-logs"
                            , logsPath </> name <.> "log"
                            ]
                    <> case mMinSeverity of
                        Nothing -> []
                        Just minSeverity ->
                            [ "--min-severity"
                            , show minSeverity
                            ]
                    <> case walletOptions of
                        NoWallet -> []
                        WalletPresence mPortNumber ->
                            ["--wallet-present"]
                                <> case mPortNumber of
                                    Nothing -> []
                                    Just portNumber ->
                                        [ "--wallet-port"
                                        , show portNumber
                                        ]
            , cmdSetup = pure ()
            , cmdInput = NoStream
            , cmdOutput = clusterStdout
            }
