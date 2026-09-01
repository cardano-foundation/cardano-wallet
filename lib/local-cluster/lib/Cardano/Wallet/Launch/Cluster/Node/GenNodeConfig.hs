{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.Node.GenNodeConfig
    ( genNodeConfig
    )
where

import Cardano.Ledger.Shelley.API
    ( ShelleyGenesis (..)
    )
import Cardano.Network.NodeToClient.Version
    ( NodeToClientVersionData (..)
    )
import Cardano.Wallet.Launch.Cluster.Aeson
    ( ChangeValue
    )
import Cardano.Wallet.Launch.Cluster.ClusterEra
    ( ClusterEra (..)
    )
import Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM
    , askNodeDir
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( DirOf (..)
    , FileOf (..)
    , absFilePathOf
    , toFilePath
    )
import Cardano.Wallet.Launch.Cluster.Logging
    ( LogFileConfig (..)
    )
import Cardano.Wallet.Launch.Cluster.Node.GenesisFiles
    ( GenesisFiles
    , GenesisRecord (..)
    )
import Cardano.Wallet.Tracing.Data.Severity
    ( Severity (..)
    )
import Control.Lens
    ( (&)
    , (.~)
    , (?~)
    )
import Control.Monad.Reader
    ( MonadIO (..)
    , MonadReader (..)
    )
import Data.Aeson
    ( Key
    , Value (..)
    , toJSON
    )
import Data.Aeson.Key
    ( fromText
    )
import Data.Aeson.Lens
    ( atKey
    , key
    )
import Data.Generics.Labels
    (
    )
import Data.Tagged
    ( Tagged
    , untag
    )
import Ouroboros.Network.Magic
    ( NetworkMagic (..)
    )
import System.Path
    ( RelDir
    , relFile
    , (<.>)
    , (</>)
    )
import Prelude

import qualified Data.Text as T
import qualified Data.Yaml as Yaml

genNodeConfig
    :: RelDir
    -- ^ A top-level directory where to put the configuration.
    -> Tagged "node-name" String
    -- ^ Node name
    -> GenesisFiles
    -- ^ Genesis block start time
    -> ClusterEra
    -- ^ Last era to hard fork into.
    -> LogFileConfig FileOf
    -- ^ Minimum severity level for logging and optional /extra/ logging output
    -> ClusterM
        ( FileOf "node-config"
        , ShelleyGenesis
        , NodeToClientVersionData
        )
genNodeConfig nodeSegment name genesisFiles clusterEra logCfg = do
    Config{..} <- ask

    DirOf poolDir <- askNodeDir nodeSegment

    let LogFileConfig{minSeverityTerminal = severity} = logCfg

        GenesisRecord byronFile shelleyFile alonzoFile conwayFile dijkstraFile =
            genesisFiles

        patchConfig value =
            value
                & setFilePath "ByronGenesisFile" byronFile
                & setFilePath "ShelleyGenesisFile" shelleyFile
                & setFilePath "AlonzoGenesisFile" alonzoFile
                & setFilePath "ConwayGenesisFile" conwayFile
                & setFilePath "DijkstraGenesisFile" dijkstraFile
                & removeGenesisHashes
                & setHardFork "ShelleyHardFork"
                & setHardFork "AllegraHardFork"
                & setHardFork "MaryHardFork"
                & setHardFork "AlonzoHardFork"
                & setHardForksForLatestEras clusterEra
                & key "TestMinSeverity" .~ toJSON Debug
                & setMinSeverity severity
                & controlExperimental clusterEra

        poolNodeConfig =
            poolDir </> relFile ("node" <> untag name <> "-config") <.> "yaml"

        nodeConfigPath =
            absDirOf cfgClusterConfigs </> relFile "node-config.json"

    liftIO
        $ Yaml.decodeFileThrow (toFilePath nodeConfigPath)
            >>= Yaml.encodeFile (toFilePath poolNodeConfig) . patchConfig

    genesisData <- Yaml.decodeFileThrow $ absFilePathOf shelleyFile

    pure
        ( FileOf @"node-config" poolNodeConfig
        , genesisData
        , NodeToClientVersionData
            { networkMagic = NetworkMagic $ sgNetworkMagic genesisData
            , query = False
            }
        )

controlExperimental :: ClusterEra -> ChangeValue
controlExperimental = \case
    _ -> setExperimental True

setExperimental :: Bool -> ChangeValue
setExperimental enabled value =
    value
        & atKey "ExperimentalProtocolsEnabled" ?~ Bool enabled
        & atKey "ExperimentalHardForksEnabled" ?~ Bool enabled

setHardForksForLatestEras :: ClusterEra -> ChangeValue
setHardForksForLatestEras clusterEra =
    case clusterEra of
        ConwayHardFork ->
            setHardFork (T.pack $ show ConwayHardFork)

-- . setHardFork (T.pack $ show BabbageHardFork)

setFilePath :: Key -> FileOf x -> ChangeValue
setFilePath keyName path =
    atKey keyName ?~ toJSON (absFilePathOf path)

setHardFork :: T.Text -> ChangeValue
setHardFork hardFork =
    atKey ("Test" <> fromText hardFork <> "AtEpoch") ?~ Number 0

setMinSeverity :: Severity -> ChangeValue
setMinSeverity severity =
    key "TraceOptions" . key "" . atKey "severity"
        ?~ toJSON (show severity)

removeGenesisHashes :: ChangeValue
removeGenesisHashes value =
    value
        & atKey "ByronGenesisHash" .~ Nothing
        & atKey "ShelleyGenesisHash" .~ Nothing
        & atKey "AlonzoGenesisHash" .~ Nothing
        & atKey "ConwayGenesisHash" .~ Nothing
        & atKey "DijkstraGenesisHash" .~ Nothing
