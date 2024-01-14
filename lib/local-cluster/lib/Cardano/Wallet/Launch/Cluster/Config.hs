{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    , ShelleyGenesisModifier
    , TestnetMagic (..)
    , NodeSegment (..)
    )
where

import Prelude

import Cardano.BM.Tracer
    ( Tracer
    )
import Cardano.Ledger.Api
    ( StandardCrypto
    )
import Cardano.Ledger.Shelley.API
    ( ShelleyGenesis
    )
import Cardano.Wallet.Launch.Cluster.ClusterEra
    ( ClusterEra
    )
import Cardano.Wallet.Launch.Cluster.Logging
    ( ClusterLog (..)
    , LogFileConfig
    )
import Cardano.Wallet.Launch.Cluster.PoolRecipe
    ( PoolRecipe
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Tagged
    ( Tagged
    )
import Numeric.Natural
    ( Natural
    )

newtype TestnetMagic = TestnetMagic { testnetMagicToNatural :: Natural }
    deriving stock (Show)

type ShelleyGenesisModifier =
    ShelleyGenesis StandardCrypto -> ShelleyGenesis StandardCrypto

data Config = Config
    { cfgStakePools :: NonEmpty PoolRecipe
    -- ^ Stake pools to register.
    , cfgLastHardFork :: ClusterEra
    -- ^ Which era to use.
    , cfgNodeLogging :: LogFileConfig
    -- ^ Log severity for node.
    , cfgClusterDir :: Tagged "cluster" FilePath
    -- ^ Root directory for cluster data.
    , cfgClusterConfigs :: Tagged "cluster-configs" FilePath
    -- ^ Directory containing data for cluster setup.
    , cfgTestnetMagic :: TestnetMagic
    -- ^ Testnet magic to use.
    , cfgShelleyGenesisMods :: [ShelleyGenesisModifier]
    -- ^ Shelley genesis modifications to apply.
    , cfgTracer :: Tracer IO ClusterLog
    }

newtype NodeSegment = NodeSegment FilePath
    deriving stock (Show)
    deriving newtype (Eq, Ord)
