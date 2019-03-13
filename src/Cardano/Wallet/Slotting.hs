{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT

module Cardano.Wallet.Slotting
    ( SlotId (..)
    , EpochIndex (..)
    , LocalSlotIndex (..)
    , slotsPerEpoch
    , addSlots
    , slotDiff
    , slotNext
    , slotPrev
    , isValidSlotId
    ) where

import Prelude

import Control.DeepSeq
    ( NFData (..) )
import Data.Word
    ( Word16, Word64 )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )

-- | Denotes a chain epoch, which contains a certain number of slots.
newtype EpochIndex = EpochIndex
    { getEpochIndex :: Word64 }
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (Num, Enum, NFData)

-- | Denotes the slot number within an epoch.
newtype LocalSlotIndex = LocalSlotIndex
    { getLocalSlotIndex :: Word16 }
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (Num, Enum, NFData)

-- | A slot identifier is the combination of an epoch and slot.
data SlotId = SlotId
  { epochIndex :: !EpochIndex
  , slotNumber :: !LocalSlotIndex
  } deriving stock (Show, Eq, Ord, Generic)

-- | Hard-coded for the time being
slotsPerEpoch :: Natural
slotsPerEpoch = 21600

instance Bounded LocalSlotIndex where
    minBound = LocalSlotIndex 0
    maxBound = LocalSlotIndex (fromIntegral slotsPerEpoch - 1)

-- | Add a number of slots to an (Epoch, LocalSlotIndex) pair, where the number
-- of slots can be greater than one epoch.
addSlots :: Natural -> SlotId -> SlotId
addSlots n (SlotId (EpochIndex e) (LocalSlotIndex sl))
    = SlotId (EpochIndex (e + fromIntegral e'))
        (LocalSlotIndex (fromIntegral sl'))
    where
        e' = n' `div` slotsPerEpoch
        sl' = n' `mod` slotsPerEpoch
        n' = fromIntegral sl + n

-- | @slotDiff a b@ is the number of slots by which @a@ is greater than @b@.
slotDiff :: SlotId -> SlotId -> Integer
slotDiff s1 s2 = flatten s1 - flatten s2
    where flatten = fromIntegral . flattenSlotId

-- | Convert SlotId into number of slots since genesis.
flattenSlotId :: SlotId -> Natural
flattenSlotId (SlotId (EpochIndex e) (LocalSlotIndex sl))
    = fromIntegral e * slotsPerEpoch + fromIntegral sl

-- | The slot after.
slotNext :: SlotId -> SlotId
slotNext = addSlots 1

-- | The slot before, if there is one.
slotPrev :: SlotId -> Maybe SlotId
slotPrev (SlotId 0 0) = Nothing
slotPrev (SlotId ep 0) = Just $ SlotId (ep - 1) (fromIntegral slotsPerEpoch - 1)
slotPrev (SlotId ep sl) = Just $ SlotId ep (sl - 1)

-- | Whether the epoch index and slot number are in range.
isValidSlotId :: SlotId -> Bool
isValidSlotId (SlotId ep (LocalSlotIndex sl)) = ep >= 0 && sl >= 0
    && sl < fromIntegral slotsPerEpoch
