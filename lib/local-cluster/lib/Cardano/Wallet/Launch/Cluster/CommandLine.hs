{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Launch.Cluster.CommandLine
    ( CommandLineOptions (..)
    , WalletPresence (..)
    , parseCommandLineOptions
    , clusterConfigsDirParser
    )
where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( Absolutizer (..)
    , DirOf (..)
    , FileOf (..)
    , newAbsolutizer
    )
import Cardano.Wallet.Launch.Cluster.Http.Service
    ( ServiceConfiguration (..)
    )
import Cardano.Wallet.Network.Ports
    ( PortNumber
    )
import Control.Monad
    ( unless
    )
import Control.Monitoring.Tracing
    ( MonitorState (..)
    )
import Data.Maybe
    ( fromMaybe
    )
import Options.Applicative
    ( Parser
    , auto
    , execParser
    , help
    , helper
    , info
    , long
    , metavar
    , option
    , optional
    , progDesc
    , strOption
    , switch
    , (<**>)
    )
import System.Path
    ( absRel
    )

import qualified Cardano.BM.Data.Severity as Severity

data WalletPresence = NoWallet | WalletPresence (Maybe PortNumber)
    deriving stock (Show)

data CommandLineOptions = CommandLineOptions
    { clusterConfigsDir :: DirOf "cluster-configs"
    , clusterDir :: Maybe (DirOf "cluster")
    , clusterLogs :: Maybe (FileOf "cluster-logs")
    , minSeverity :: Maybe Severity
    , nodeToClientSocket :: Maybe (FileOf "node-to-client-socket")
    , httpService :: ServiceConfiguration
    , faucetFunds :: FileOf "faucet-funds"
    , walletPresent :: WalletPresence
    }
    deriving stock (Show)

parseCommandLineOptions :: IO CommandLineOptions
parseCommandLineOptions = do
    absolutizer <- newAbsolutizer
    execParser
        $ info
            ( CommandLineOptions
                <$> clusterConfigsDirParser absolutizer
                <*> clusterDirParser absolutizer
                <*> clusterLogsParser absolutizer
                <*> minSeverityParser
                <*> nodeToClientSocketParser absolutizer
                <*> monitoringParser
                <*> faucetFundsParser absolutizer
                <*> withWalletParser
                <**> helper
            )
            (progDesc "Local Cluster for testing")

withWalletParser :: Parser WalletPresence
withWalletParser = do
    p <- walletPresentParser
    port <- walletPortParser
    pure $ if p then WalletPresence port else NoWallet

walletPresentParser :: Parser Bool
walletPresentParser = do
    switch
        ( long "wallet-present"
            <> help "If to start the wallet or not"
        )

walletPortParser :: Parser (Maybe PortNumber)
walletPortParser = do
    optional
        $ option
            auto
            ( long "wallet-port"
                <> metavar "WALLET_PORT"
                <> help "Port for the wallet HTTP server"
            )

faucetFundsParser :: Absolutizer -> Parser (FileOf "faucet-funds")
faucetFundsParser (Absolutizer absOf) =
    FileOf . absOf . absRel
        <$> strOption
            ( long "faucet-funds"
                <> metavar "FAUCET_FUNDS"
                <> help "Path to the faucet funds file"
            )

minSeverityParser :: Parser (Maybe Severity)
minSeverityParser =
    optional
        $ option
            parse
            ( long "min-severity"
                <> metavar "MIN_SEVERITY"
                <> help "Minimum severity level for logging"
            )
  where
    parse = do
        s :: String <- auto
        case s of
            "Debug" -> pure Severity.Debug
            "Info" -> pure Severity.Info
            "Notice" -> pure Severity.Notice
            "Warning" -> pure Severity.Warning
            "Error" -> pure Severity.Error
            "Critical" -> pure Severity.Critical
            "Alert" -> pure Severity.Alert
            "Emergency" -> pure Severity.Emergency
            _ -> fail "Invalid severity level"

monitoringParser :: Parser ServiceConfiguration
monitoringParser =
    mkServiceConfiguration
        <$> httpApiPortParser
        <*> controlInitalStateParser
  where
    mkServiceConfiguration port mstate =
        ServiceConfiguration port
            $ fromMaybe Run mstate

controlInitalStateParser :: Parser (Maybe MonitorState)
controlInitalStateParser =
    optional
        $ option
            parse
            ( long "control-initial-state"
                <> metavar "CONTROL_INITIAL_STATE"
                <> help "Initial state of the control, wait, step or run"
            )
  where
    parse = do
        s :: String <- auto
        case s of
            "wait" -> pure Wait
            "step" -> pure Step
            "run" -> pure Run
            _ -> fail "Invalid control initial state"

httpApiPortParser :: Parser (Maybe PortNumber)
httpApiPortParser = do
    optional
        $ option
            parse
            ( long "monitoring-port"
                <> metavar "MONITORING_PORT"
                <> help "Port for the monitoring HTTP server"
            )
  where
    parse = do
        p <- auto
        unless (p `elem` validPorts)
            $ fail
            $ "Invalid port number. Must be inside: "
                ++ show minPort
                ++ ".."
                ++ show maxPort
        pure p

minPort :: PortNumber
minPort = 1024

maxPort :: PortNumber
maxPort = 65535

validPorts :: [PortNumber]
validPorts = [minPort .. maxPort]

nodeToClientSocketParser
    :: Absolutizer
    -> Parser (Maybe (FileOf "node-to-client-socket"))
nodeToClientSocketParser (Absolutizer absOf) =
    optional
        $ FileOf . absOf . absRel
            <$> strOption
                ( long "socket-path"
                    <> metavar "NODE_TO_CLIENT_SOCKET"
                    <> help "Path to the node-to-client socket"
                )

clusterConfigsDirParser :: Absolutizer -> Parser (DirOf "cluster-configs")
clusterConfigsDirParser (Absolutizer absOf) =
    DirOf . absOf . absRel
        <$> strOption
            ( long "cluster-configs"
                <> metavar "LOCAL_CLUSTER_CONFIGS"
                <> help "Path to the local cluster configuration directory"
            )

clusterDirParser :: Absolutizer -> Parser (Maybe (DirOf "cluster"))
clusterDirParser (Absolutizer absOf) =
    optional
        $ DirOf . absOf . absRel
            <$> strOption
                ( long "cluster"
                    <> metavar "LOCAL_CLUSTER"
                    <> help "Path to the local cluster directory"
                )

clusterLogsParser :: Absolutizer -> Parser (Maybe (FileOf "cluster-logs"))
clusterLogsParser (Absolutizer absOf) =
    optional
        $ FileOf . absOf . absRel
            <$> strOption
                ( long "cluster-logs"
                    <> metavar "LOCAL_CLUSTER_LOGS"
                    <> help "Path to the local cluster logs file"
                )
