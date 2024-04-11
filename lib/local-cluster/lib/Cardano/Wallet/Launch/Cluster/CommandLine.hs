{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module Cardano.Wallet.Launch.Cluster.CommandLine
    ( CommandLineOptions (..)
    , parseCommandLineOptions
    , clusterConfigsDirParser
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.FileOf
    ( Absolutizer (..)
    , DirOf (..)
    , FileOf (..)
    , newAbsolutizer
    )
import Options.Applicative
    ( Parser
    , execParser
    , help
    , helper
    , info
    , long
    , metavar
    , optional
    , progDesc
    , strOption
    , (<**>)
    )
import System.Path
    ( absRel
    )

data CommandLineOptions = CommandLineOptions
    { clusterConfigsDir :: DirOf "cluster-configs"
    , clusterDir :: Maybe (DirOf "cluster")
    , clusterLogs :: Maybe (FileOf "cluster-logs")
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
                <**> helper
            )
            (progDesc "Local Cluster for testing")

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
