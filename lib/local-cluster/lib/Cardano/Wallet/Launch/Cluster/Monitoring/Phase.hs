{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Cardano.Wallet.Launch.Cluster.Monitoring.Phase
    ( Phase (..)
    , RelayNode (..)
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
import GHC.Generics
    ( Generic
    )

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

data Phase
    = Metadata
    | Genesis
    | Pool0
    | Funding
    | Pools
    | Relay
    | Cluster (Maybe RelayNode)
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
