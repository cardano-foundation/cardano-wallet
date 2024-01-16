{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.ClusterEra
    ( ClusterEra (..)
    , clusterEraFromEnv
    , localClusterConfigsFromEnv
    , clusterEraToString
    , ignoreInConway
    , ignoreInBabbage
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
    = BabbageHardFork
    | ConwayHardFork
    deriving stock (Show, Read, Eq, Ord, Enum, Bounded)

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
    err era =
        die
            $ var
                ++ ": "
                ++ era
                ++ " era is not supported in the local cluster"
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
    BabbageHardFork -> "babbage"
    ConwayHardFork -> "conway"

ignoreInConway :: Applicative f => ClusterEra -> f () -> f ()
ignoreInConway era f = case era of
    ConwayHardFork -> pure ()
    _ -> f

ignoreInBabbage :: Applicative f => ClusterEra -> f () -> f ()
ignoreInBabbage era f = case era of
    BabbageHardFork -> pure ()
    _ -> f
