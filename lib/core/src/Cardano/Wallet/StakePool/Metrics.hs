{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
-- | This module can fold over a blockchain to collect metrics about
-- Stake pools.
--
-- It interacts with:
-- - "Cardano.Wallet.Network" which provides the chain
-- - "Cardano.Wallet.DB" - which can persist the metrics
-- - "Cardano.Wallet.Api.Server" - which presents the results in an endpoint
module Cardano.Wallet.StakePool.Metrics
    ( activityForEpoch

    , State (..)
    , applyBlock
    )
    where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..), EpochNo (..), PoolId (..), SlotId (..) )
import Data.Map.Strict
    ( Map )
import Fmt
    ( Buildable (..), blockListF', fmt, (+|), (|+) )
import GHC.Generics
    ( Generic )

import qualified Data.Map.Strict as Map

-- | For a given epoch, and state, this function returns /how many/ blocks
-- each pool produced.
activityForEpoch :: EpochNo -> State -> Map PoolId Int
activityForEpoch epoch s =
    Map.filter (> 0)
    $ Map.map (length . filter slotInCurrentEpoch)
    (activity s)
  where
    slotInCurrentEpoch = ((epoch ==) . epochNumber)

--
-- Internals
--

-- | In-memory state keeping track of which pool produced blocks at which slots.
data State = State
    { tip :: BlockHeader
      -- ^ The blockHeader of the most recently applied block. Used to resume
      -- restoration from @NetworkLayer@.
    , activity :: Map PoolId [SlotId]
    -- ^ Mapping from pools to the slots where pool produced blocks.
    --
    -- This is needed internally to support rollback, but publicly, only the
    -- /length of/ the SlotId-list is likely needed.
    } deriving (Eq, Show, Generic)

instance Buildable State where
    build (State t m) =
        fmt ("Stakepool metrics at tip: "+|t|+"\n") <>
        blockListF'
            mempty
            (\(k,v) -> fmt (""+|k|+": "+|length v|+"") )
            (Map.toList m)

applyBlock :: (BlockHeader, PoolId) -> State -> State
applyBlock (newTip, poolId) (State _prevTip prevMap) =
        State newTip (Map.alter alter poolId prevMap)
  where
    slot = slotId newTip
    alter = \case
        Nothing -> Just [slot]
        Just slots -> Just (slot:slots)
