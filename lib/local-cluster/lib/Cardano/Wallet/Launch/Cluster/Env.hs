{-# LANGUAGE DataKinds #-}

module Cardano.Wallet.Launch.Cluster.Env where

import Prelude

import Cardano.Wallet.Launch.Cluster.ClusterEra
    ( ClusterEra (..)
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( DirOf (..)
    , FileOf (..)
    , absolutize
    )
import Data.Char
    ( toLower
    )
import System.Environment.Extended
    ( lookupEnvNonEmpty
    )
import System.Exit
    ( die
    )
import System.Path
    ( absRel
    , relDir
    , (</>)
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

localClusterConfigsFromEnv :: IO (DirOf "cluster-configs")
localClusterConfigsFromEnv = do
    mConfigsPath <- lookupEnvNonEmpty "LOCAL_CLUSTER_CONFIGS"
    let configPath = case mConfigsPath of
            Just path -> absRel path
            Nothing ->
                absRel ".."
                    </> relDir "local-cluster"
                    </> relDir "test"
                    </> relDir "data"
                    </> relDir "cluster-configs"
    DirOf <$> absolutize configPath

nodeOutputFileFromEnv :: IO (Maybe (FileOf "node-output"))
nodeOutputFileFromEnv = do
    mNodeOutput <-
        fmap absRel
            <$> lookupEnvNonEmpty "LOCAL_CLUSTER_NODE_OUTPUT_FILE"
    fmap FileOf <$> absolutize `traverse` mNodeOutput
