{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.ClusterEra
   ( ClusterEra(..)
    , clusterEraFromEnv
    , localClusterConfigsFromEnv
    , clusterEraToString
    )
where

import Prelude

import Data.Char
    ( toLower
    )
import Data.Functor
    ( (<&>)
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Tagged
    ( Tagged (Tagged)
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
    deriving stock (Show, Read, Eq, Ord, Bounded, Enum)

-- | Defaults to the latest era.
clusterEraFromEnv :: IO ClusterEra
clusterEraFromEnv =
    fmap withDefault . traverse getEra =<< lookupEnvNonEmpty var
  where
    var = "LOCAL_CLUSTER_ERA"
    getEra env = case map toLower env of
        "byron" -> pure ByronNoHardFork
        "shelley" -> pure ShelleyHardFork
        "allegra" -> pure AllegraHardFork
        "mary" -> pure MaryHardFork
        "alonzo" -> pure AlonzoHardFork
        "babbage" -> pure BabbageHardFork
        _ -> die $ var ++ ": unknown era"
    withDefault = fromMaybe maxBound

localClusterConfigsFromEnv :: IO (Tagged "cluster-configs" FilePath)
localClusterConfigsFromEnv =
    lookupEnvNonEmpty "LOCAL_CLUSTER_CONFIGS"
        <&> Tagged @"cluster-configs"
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
