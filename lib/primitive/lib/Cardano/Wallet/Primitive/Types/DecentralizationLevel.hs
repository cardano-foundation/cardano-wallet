{-# LANGUAGE DeriveGeneric #-}

module Cardano.Wallet.Primitive.Types.DecentralizationLevel
    ( DecentralizationLevel (..)
    , fromDecentralizationLevel
    , fromFederationPercentage
    , getFederationPercentage
    )
where

import Prelude

import Control.DeepSeq
    ( NFData
    )
import Data.Percentage
    ( Percentage
    )
import Fmt
    ( Buildable (..)
    )
import GHC.Generics
    ( Generic
    )

import qualified Data.Percentage as Percentage

-- | Indicates the current level of decentralization in the network.
--
-- According to the Design Specification for Delegation and Incentives in
-- Cardano, the decentralization parameter __/d/__ is a value in the range
-- '[0, 1]', where:
--
--   * __/d/__ = '1' indicates that the network is /completely federalized/.
--   * __/d/__ = '0' indicates that the network is /completely decentralized/.
--
-- However, in Cardano Wallet, we represent the decentralization level as a
-- percentage, where:
--
--   * '  0 %' indicates that the network is /completely federalized/.
--   * '100 %' indicates that the network is /completely decentralized/.
newtype DecentralizationLevel = DecentralizationLevel
    {getDecentralizationLevel :: Percentage}
    deriving (Bounded, Eq, Generic, Show)

fromDecentralizationLevel :: Percentage -> DecentralizationLevel
fromDecentralizationLevel = DecentralizationLevel

-- | Percentage of federated nodes.
-- Equal to the "decentralization parameter" /d/ from the ledger specification.
fromFederationPercentage :: Percentage -> DecentralizationLevel
fromFederationPercentage = fromDecentralizationLevel . Percentage.complement

getFederationPercentage :: DecentralizationLevel -> Percentage
getFederationPercentage = Percentage.complement . getDecentralizationLevel

instance NFData DecentralizationLevel

instance Buildable DecentralizationLevel where
    build = build . getDecentralizationLevel
