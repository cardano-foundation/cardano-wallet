{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
-- | This module can fold over a blockchain to collect metrics about
-- Stake pools.
--
-- It interacts with:
-- - "Cardano.Wallet.Network" which provides the chain
-- - "Cardano.Wallet.DB" - which can persist the metrics
-- - "Cardano.Wallet.Api.Server" - which presents the results in an endpoint
module Cardano.Wallet.StakePool.Metrics where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..), SlotId (..) )
import Data.ByteString
    ( ByteString )
import Data.Map.Strict
    ( Map )
import Data.Word
    ( Word )

import qualified Data.Map.Strict as Map

newtype PoolId = PoolVRFPubKey ByteString -- 32 bytes long
  deriving newtype (Ord, Eq)

data State = State
    { tip :: BlockHeader
    , numberOfBlocksProducedThisEpoch :: Map PoolId Word
    }


applyBlock :: b -> (b -> BlockHeader) -> (b -> PoolId) -> State -> State
applyBlock b getTip getPoolId (State prevTip prevMap) =
    let
        map' = Map.alter  (\case
            Nothing -> Just 1
            Just n -> Just $ n + 1)
            (getPoolId b)
            prevMap
    in
        -- Clear the map if the epoch changes
        -- TODO: The plan to deal with rollbacks might be really unsound.
        if epoch prevTip == epoch (getTip b)
        then (State tip' map')
        else State tip' Map.empty
  where
    epoch = epochNumber . slotId
    tip' = getTip b
