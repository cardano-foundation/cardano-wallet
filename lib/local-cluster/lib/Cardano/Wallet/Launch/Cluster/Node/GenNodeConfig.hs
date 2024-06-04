{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.Node.GenNodeConfig
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
    ( withAddedKey
    , withObject
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
import Cardano.Wallet.Launch.Cluster.Node.GenesisFiles
    ( GenesisFiles
    , GenesisRecord (..)
    )
import Cardano.Wallet.Launch.Cluster.Logging
    ( LogFileConfig (..)
    )
import Control.Monad
    ( (>=>)
    )
import Control.Monad.Reader
    ( MonadIO (..)
    , MonadReader (..)
    )
import Data.Aeson
    ( toJSON
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
    ( AbsFile
    , RelDir
    , relFile
    , (<.>)
    , (</>)
    )

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Text as T
import qualified Data.Yaml as Yaml

genNodeConfig
    :: RelDir
    -- ^ A top-level directory where to put the configuration.
    -> Tagged "node-name" String -- Node name
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
    let GenesisRecord byronFile shelleyFile alonzoFile conwayFile = genesisFiles
    let fileScribe (path, sev) =
            ScribeDefinition
                { scName = path
                , scFormat = ScText
                , scKind = FileSK
                , scMinSev = sev
                , scMaxSev = Critical
                , scPrivacy = ScPublic
                , scRotation = Nothing
                }

    let scribes =
            map fileScribe
                $ catMaybes
                    [ Just ("cardano-node.log", severity)
                    , case mExtraLogFile of
                        Just (FileOf file) ->
                            Just
                                (T.pack $ toFilePath file, extraSev)
                        Nothing -> Nothing
                    ]

    let poolNodeConfig =
            poolDir </> relFile ("node" <> untag name <> "-config") <.> "yaml"
        nodeConfigPath :: AbsFile
        nodeConfigPath = absDirOf cfgClusterConfigs
            </> relFile "node-config.json"
    liftIO
        $ Yaml.decodeFileThrow (toFilePath nodeConfigPath)
            >>= withAddedKey "ShelleyGenesisFile" (absFilePathOf shelleyFile)
            >>= withAddedKey "ByronGenesisFile" (absFilePathOf byronFile)
            >>= withAddedKey "AlonzoGenesisFile" (absFilePathOf alonzoFile)
            >>= withAddedKey "ConwayGenesisFile" (absFilePathOf conwayFile)
            >>= withHardForks clusterEra
            >>= withAddedKey "minSeverity" Debug
            >>= withScribes scribes
            >>= withObject (addMinSeverityStdout severity)
            >>= Yaml.encodeFile (toFilePath poolNodeConfig)

    -- Parameters
    genesisData <- Yaml.decodeFileThrow $ absFilePathOf shelleyFile
    let networkMagic = NetworkMagic $ sgNetworkMagic genesisData
    pure
        ( FileOf @"node-config" poolNodeConfig
        , genesisData
        , NodeToClientVersionData{networkMagic, query = False}
        )
  where
    withScribes :: [ScribeDefinition] -> Yaml.Value -> IO Yaml.Value
    withScribes scribes =
        withAddedKey "setupScribes" scribes
            >=> withAddedKey
                "defaultScribes"
                (map (\s -> [toJSON $ scKind s, toJSON $ scName s]) scribes)

    withHardForks :: ClusterEra -> Yaml.Value -> IO Yaml.Value
    withHardForks era =
        withObject (pure . Aeson.union (Aeson.fromList hardForks))
      where
        hardForks =
            [ ( Aeson.fromText $ "Test" <> hardFork <> "AtEpoch"
              , Yaml.Number 0
              )
            | hardFork <-
                [ "ShelleyHardFork"
                , "AllegraHardFork"
                , "MaryHardFork"
                , "AlonzoHardFork"
                ]
                    <> (T.pack . show <$> [minBound .. era])
            ]

-- | Add a @setupScribes[1].scMinSev@ field in a given config object.
-- The full lens library would be quite helpful here.
addMinSeverityStdout
    :: MonadFail m
    => Severity
    -> Aeson.Object
    -> m Aeson.Object
addMinSeverityStdout severity ob = case Aeson.lookup "setupScribes" ob of
    Just (Aeson.Array scribes) -> do
        let scribes' = Aeson.Array $ fmap setMinSev scribes
        pure $ Aeson.insert "setupScribes" scribes' ob
    _ -> fail "setupScribes logging config is missing or the wrong type"
  where
    sev = toJSON $ show severity
    setMinSev (Aeson.Object scribe)
        | Aeson.lookup "scKind" scribe == Just (Aeson.String "StdoutSK") =
            Aeson.Object (Aeson.insert "scMinSev" sev scribe)
        | otherwise = Aeson.Object scribe
    setMinSev a = a
