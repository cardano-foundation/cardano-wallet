{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Cardano.Wallet.Launch.Cluster.Monitoring.Phase
    ( Phase (..)
    , RelayNode (..)
    , History (..)
    )
where

import Prelude

import Cardano.Launcher.Node
    ( CardanoNodeConn
    , cardanoNodeConn
    , nodeSocketFile
    )
import Data.Aeson
    ( FromJSON
    , ToJSON
    )
import Data.Aeson.Types
    ( FromJSON (..)
    , ToJSON (..)
    )
import Data.Time
    ( UTCTime
    )
import GHC.Generics
    ( Generic
    )

-- | A relay node as a reference to its socket file or pipe
newtype RelayNode = RelayNode CardanoNodeConn
    deriving stock (Eq, Show, Generic)

instance ToJSON RelayNode where
    toJSON (RelayNode f) = toJSON $ nodeSocketFile f

instance FromJSON RelayNode where
    parseJSON x = do
        f <- parseJSON x
        case cardanoNodeConn f of
            Right conn -> pure $ RelayNode conn
            Left e -> fail e

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
    | Cluster (Maybe RelayNode)
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | The history of the cluster phases
newtype History = History
    { history :: [(UTCTime, Phase)]
    }
    deriving stock (Eq, Show)
