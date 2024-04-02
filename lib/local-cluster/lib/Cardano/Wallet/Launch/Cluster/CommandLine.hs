{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module Cardano.Wallet.Launch.Cluster.CommandLine
    ( CommandLineOptions (..)
    , parseCommandLineOptions
    , clusterConfigsDirParser
    ) where

import Prelude

import Cardano.Wallet.Launch.Cluster.FileOf
    ( Absolutizer (..)
    , DirOf (..)
    , newAbsolutizer
    )
import Options.Applicative
    ( (<**>)
    )
import System.Path
    ( absRel
    )

import qualified Options.Applicative as O

newtype CommandLineOptions = CommandLineOptions
    {clusterConfigsDir :: DirOf "cluster-configs"}
    deriving stock (Show)

parseCommandLineOptions :: IO CommandLineOptions
parseCommandLineOptions = do
    absolutizer <- newAbsolutizer
    O.execParser
        $ O.info
            ( fmap CommandLineOptions (clusterConfigsDirParser absolutizer)
                <**> O.helper
            )
            (O.progDesc "Local Cluster for testing")

clusterConfigsDirParser :: Absolutizer -> O.Parser (DirOf "cluster-configs")
clusterConfigsDirParser (Absolutizer absOf) =
    DirOf . absOf . absRel
        <$> O.strOption
            ( O.long "cluster-configs"
                <> O.metavar "LOCAL_CLUSTER_CONFIGS"
                <> O.help "Path to the local cluster configuration directory"
            )
