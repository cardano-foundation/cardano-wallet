{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Cardano.Wallet.Launch.Cluster.Monitoring.Phase
    ( Phase (..)
    , History (..)
    , matchCluster
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.Node.RunningNode
    ( RunningNode
    )
import Data.Time
    ( UTCTime
    )
import GHC.Generics
    ( Generic
    )

-- | The different phases the cluster can be in. We use the convention to report
-- the start of a phase.
data Phase
    = RetrievingFunds
    | Metadata
    | Genesis
    | Pool0
    | Funding
    | Pools
    | Relay
    | Cluster (Maybe RunningNode)
    deriving stock (Eq, Show, Generic)
    -- deriving anyclass (ToJSON, FromJSON)

-- | The history of the cluster phases
newtype History = History
    { history :: [(UTCTime, Phase)]
    }
    deriving stock (Eq, Show, Generic)

-- | Try to extract the 'RunningNode' from a phase.
matchCluster :: Phase -> Maybe RunningNode
matchCluster (Cluster m) = m
matchCluster _ = Nothing
