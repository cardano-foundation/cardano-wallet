{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module Cardano.Wallet.Launch.Cluster.Env where

import Prelude

import Cardano.Wallet.Launch.Cluster.ClusterEra
    ( ClusterEra (..)
    )
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

nodeOutputFileFromEnv :: IO (Maybe (FileOf "node-output"))
nodeOutputFileFromEnv = fmap FileOf
    <$> lookupEnvNonEmpty "LOCAL_CLUSTER_NODE_OUTPUT_FILE"

testnetMagicFromEnv :: IO Int
testnetMagicFromEnv = lookupEnvNonEmpty "LOCAL_CLUSTER_MAGIC" <&> \case
    Nothing -> 42
    Just magic -> case reads magic
        of
            [(m, "")] -> m
            _ -> error $ "Invalid testnet magic: " ++ magic
