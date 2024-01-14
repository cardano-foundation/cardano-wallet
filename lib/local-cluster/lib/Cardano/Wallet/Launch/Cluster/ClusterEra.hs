{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.ClusterEra
    ( ClusterEra (..)
    , clusterEraFromEnv
    , localClusterConfigsFromEnv
    , clusterEraToString
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.FileOf
    ( FileOf (..)
    )
import Data.Char
    ( toLower
    )
import Data.Functor
    ( (<&>)
    )
import Data.Maybe
    ( fromMaybe
    )
import System.Environment.Extended
    ( lookupEnvNonEmpty
    )
import System.Exit
    ( die
    )
import System.FilePath
    ( (</>)
    )

data ClusterEra
    = ByronNoHardFork
    | ShelleyHardFork
    | AllegraHardFork
    | MaryHardFork
    | AlonzoHardFork
    | BabbageHardFork
    | ConwayHardFork
    deriving stock (Show, Read, Eq, Ord, Enum)

-- | Defaults to the latest era.
clusterEraFromEnv :: IO ClusterEra
clusterEraFromEnv = do
    mera <- lookupEnvNonEmpty var
    case mera of
        Nothing -> pure BabbageHardFork
        Just era -> getEra era
  where
    var = "LOCAL_CLUSTER_ERA"
    err :: [Char] -> IO a
    err era = die $ var ++ ": " ++ era ++ " era is not supported"
    getEra env = case map toLower env of
        "byron" -> err "byron"
        "shelley" -> err "shelley"
        "allegra" -> err "allegra"
        "mary" -> err "mary"
        "alonzo" -> err "alonzo"
        "babbage" -> pure BabbageHardFork
        "conway" -> pure ConwayHardFork
        _ -> die $ var ++ ": unknown era"

localClusterConfigsFromEnv :: IO (FileOf "cluster-configs")
localClusterConfigsFromEnv =
    lookupEnvNonEmpty "LOCAL_CLUSTER_CONFIGS"
        <&> FileOf @"cluster-configs"
            . fromMaybe
                (".." </> "local-cluster" </> "test" </> "data" </> "cluster-configs")

clusterEraToString :: ClusterEra -> String
clusterEraToString = \case
    ByronNoHardFork -> "byron"
    ShelleyHardFork -> "shelley"
    AllegraHardFork -> "allegra"
    MaryHardFork -> "mary"
    AlonzoHardFork -> "alonzo"
    BabbageHardFork -> "babbage"
    ConwayHardFork -> "conway"
