{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Launch.Cluster.ClusterEra
    ( ClusterEra (..)
    , clusterEraToString
    )
where

import Prelude

data ClusterEra
    = ConwayHardFork
    deriving stock (Show, Read, Eq, Ord, Enum, Bounded)

clusterEraToString :: ClusterEra -> String
clusterEraToString = \case
    ConwayHardFork -> "conway"
