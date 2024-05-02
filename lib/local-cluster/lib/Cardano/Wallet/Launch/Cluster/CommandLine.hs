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
    , progDesc
    , strOption
    , (<**>)
    )
import System.Path
    ( absRel
    )

newtype CommandLineOptions = CommandLineOptions
    {clusterConfigsDir :: DirOf "cluster-configs"}
    deriving stock (Show)

parseCommandLineOptions :: IO CommandLineOptions
parseCommandLineOptions = do
    absolutizer <- newAbsolutizer
    execParser
        $ info
            ( fmap CommandLineOptions (clusterConfigsDirParser absolutizer)
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
