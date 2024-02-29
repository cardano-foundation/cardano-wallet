{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Launch.Cluster.ClusterEra
    ( ClusterEra (..)
    , clusterEraToString
    , ignoreInConway
    , ignoreInBabbage
    )
where

import Prelude

data ClusterEra
    = BabbageHardFork
    | ConwayHardFork
    deriving stock (Show, Read, Eq, Ord, Enum, Bounded)

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
