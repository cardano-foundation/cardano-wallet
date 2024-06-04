{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.Node.GenNodeConfig
    ( genNodeConfig
    )
where

import Prelude

import Cardano.BM.Data.Output
    ( ScribeDefinition (..)
    , ScribeFormat (..)
    , ScribeKind (..)
    , ScribePrivacy (..)
    )
import Cardano.BM.Data.Severity
    ( Severity (..)
    )
import Cardano.Ledger.Api
    ( StandardCrypto
    )
import Cardano.Ledger.Shelley.API
    ( ShelleyGenesis (..)
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
import Control.Lens
    ( (%~)
    , (&)
    , (.~)
    , (<&>)
    , (?~)
    , (^?)
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
    ( _Array
    , _String
    , atKey
    , key
    )
import Data.Generics.Labels
    ()
import Data.Maybe
    ( catMaybes
    )
import Data.Tagged
    ( Tagged
    , untag
    )
import Ouroboros.Network.Magic
    ( NetworkMagic (..)
    )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData (..)
    )
import System.Path
    ( RelDir
    , relFile
    , (<.>)
    , (</>)
    )

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
        , ShelleyGenesis StandardCrypto
        , NodeToClientVersionData
        )
genNodeConfig nodeSegment name genesisFiles clusterEra logCfg = do
    Config{..} <- ask

    DirOf poolDir <- askNodeDir nodeSegment

    let LogFileConfig severity mExtraLogFile extraSev = logCfg

        GenesisRecord byronFile shelleyFile alonzoFile conwayFile = genesisFiles

        scribes =
            let
                fileScribe (path, sev) =
                    ScribeDefinition
                        { scName = path
                        , scFormat = ScText
                        , scKind = FileSK
                        , scMinSev = sev
                        , scMaxSev = Critical
                        , scPrivacy = ScPublic
                        , scRotation = Nothing
                        }
            in
                fileScribe
                    <$> catMaybes
                        [ Just ("cardano-node.log", severity)
                        , mExtraLogFile <&> \(FileOf file) ->
                            (T.pack $ toFilePath file, extraSev)
                        ]

        patchConfig value =
            value
                & setFilePath "ByronGenesisFile" byronFile
                & setFilePath "ShelleyGenesisFile" shelleyFile
                & setFilePath "AlonzoGenesisFile" alonzoFile
                & setFilePath "ConwayGenesisFile" conwayFile
                & removeGenesisHashes
                & setHardFork "ShelleyHardFork"
                & setHardFork "AllegraHardFork"
                & setHardFork "MaryHardFork"
                & setHardFork "AlonzoHardFork"
                & setHardForksForLatestEras clusterEra
                & key "TestMinSeverity" .~ toJSON Debug
                & key "setupScribes" .~ toJSON scribes
                & key "defaultScribes" .~ toJSON (scribeToJSON <$> scribes)
                & addMinSeverityStdout severity
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
    BabbageHardFork -> setExperimental False
    ConwayHardFork -> setExperimental True

setExperimental :: Bool -> ChangeValue
setExperimental enabled value = value
    & atKey "ExperimentalProtocolsEnabled" ?~ Bool enabled
    & atKey "ExperimentalHardForksEnabled" ?~ Bool enabled

setHardForksForLatestEras :: ClusterEra -> ChangeValue
setHardForksForLatestEras clusterEra =
    case clusterEra of
        BabbageHardFork -> setHardFork (T.pack $ show BabbageHardFork)
        ConwayHardFork ->
            setHardFork (T.pack $ show ConwayHardFork)
                . setHardFork (T.pack $ show BabbageHardFork)

scribeToJSON :: ScribeDefinition -> [Value]
scribeToJSON ScribeDefinition{..} =
    [ toJSON scKind
    , toJSON scName
    ]

setFilePath :: Key -> FileOf x -> ChangeValue
setFilePath keyName path =
    atKey keyName ?~ toJSON (absFilePathOf path)

setHardFork :: T.Text -> ChangeValue
setHardFork hardFork =
    atKey ("Test" <> fromText hardFork <> "AtEpoch") ?~ Number 0

addMinSeverityStdout :: Severity -> ChangeValue
addMinSeverityStdout severity =
    key "setupScribes" . _Array . traverse %~ setMinSev
  where
    setMinSev :: ChangeValue
    setMinSev scribe = case scribe ^? key "scKind" . _String of
        Just "StdoutSK" -> scribe & atKey "scMinSev" ?~ toJSON (show severity)
        _ -> scribe

removeGenesisHashes :: ChangeValue
removeGenesisHashes value = value
    & atKey "ByronGenesisHash" .~ Nothing
    & atKey "ShelleyGenesisHash" .~ Nothing
    & atKey "AlonzoGenesisHash" .~ Nothing
    & atKey "ConwayGenesisHash" .~ Nothing
