{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
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
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    )
import Cardano.Wallet.Launch.Cluster.GenesisFiles
    ( GenesisFiles (..)
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
    ( Tagged (..)
    , untag
    )
import Ouroboros.Network.Magic
    ( NetworkMagic (..)
    )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData (..)
    )
import System.FilePath
    ( (</>)
    )

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Text as T
import qualified Data.Yaml as Yaml

genNodeConfig
    :: String
    -- ^ A top-level directory where to put the configuration.
    -> Tagged "node-name" String -- Node name
    -> GenesisFiles
    -- ^ Genesis block start time
    -> ClusterEra
    -- ^ Last era to hard fork into.
    -> LogFileConfig
    -- ^ Minimum severity level for logging and optional /extra/ logging output
    -> ClusterM
        ( Tagged "node-config" FilePath
        , ShelleyGenesis StandardCrypto
        , NodeToClientVersionData
        )
genNodeConfig poolName name genesisFiles clusterEra logCfg = do
    Config{..} <- ask
    let poolDir = untag cfgClusterDir </> poolName
    let LogFileConfig severity mExtraLogFile extraSev = logCfg
    let GenesisFiles
            { byronGenesis
            , shelleyGenesis
            , alonzoGenesis
            , conwayGenesis
            } = genesisFiles

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
                    , (,extraSev) . T.pack <$> mExtraLogFile
                    ]

    let poolNodeConfig =
            poolDir </> ("node" <> untag name <> "-config.yaml")

    liftIO $ Yaml.decodeFileThrow (untag cfgClusterConfigs </> "node-config.json")
        >>= withAddedKey "ShelleyGenesisFile" shelleyGenesis
        >>= withAddedKey "ByronGenesisFile" byronGenesis
        >>= withAddedKey "AlonzoGenesisFile" alonzoGenesis
        >>= withAddedKey "ConwayGenesisFile" conwayGenesis
        >>= withHardForks clusterEra
        >>= withAddedKey "minSeverity" Debug
        >>= withScribes scribes
        >>= withObject (addMinSeverityStdout severity)
        >>= Yaml.encodeFile poolNodeConfig

    -- Parameters
    genesisData <- Yaml.decodeFileThrow shelleyGenesis
    let networkMagic = NetworkMagic $ sgNetworkMagic genesisData
    pure
        ( Tagged @"node-config" poolNodeConfig
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
            [ ( Aeson.fromText $ "Test" <> T.pack (show hardFork) <> "AtEpoch"
              , Yaml.Number 0
              )
            | hardFork <- [ShelleyHardFork .. era]
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
