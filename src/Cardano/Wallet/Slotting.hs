{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT

module Cardano.Wallet.Slotting
    ( SlotId (..)
    , slotsPerEpoch
    , addSlots
    , slotDiff
    , slotNext
    , slotPrev
    , isValidSlotId
    ) where

import Prelude

import Control.DeepSeq
    ( NFData )
import Data.Word
    ( Word16, Word64 )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )

-- | A slot identifier is the combination of an epoch and slot.
data SlotId = SlotId
  { epochIndex :: !Word64
  , slotNumber :: !Word16
  } deriving stock (Show, Eq, Ord, Generic)

instance NFData SlotId

-- | Hard-coded for the time being
slotsPerEpoch :: Natural
slotsPerEpoch = 21600

-- | Add a number of slots to an (Epoch, LocalSlotIndex) pair, where the number
-- of slots can be greater than one epoch.
addSlots :: Natural -> SlotId -> SlotId
addSlots n (SlotId e s) =
    SlotId (e + e') s'
  where
    e' = fromIntegral (n' `div` slotsPerEpoch)
    s' = fromIntegral (n' `mod` slotsPerEpoch)
    n' = fromIntegral s + n

-- | @slotDiff a b@ is the number of slots by which @a@ is greater than @b@.
slotDiff :: SlotId -> SlotId -> Integer
slotDiff s1 s2 = flatten s1 - flatten s2
    where flatten = fromIntegral . flattenSlotId

-- | Convert SlotId into number of slots since genesis.
flattenSlotId :: SlotId -> Natural
flattenSlotId (SlotId e s) =
    fromIntegral e * slotsPerEpoch + fromIntegral s

-- | The slot after.
slotNext :: SlotId -> SlotId
slotNext = addSlots 1

-- | The slot before, if there is one.
slotPrev :: SlotId -> Maybe SlotId
slotPrev (SlotId 0 0) = Nothing
slotPrev (SlotId e 0) = Just $ SlotId (e - 1) (fromIntegral slotsPerEpoch - 1)
slotPrev (SlotId e s) = Just $ SlotId e (s - 1)

-- | Whether the epoch index and slot number are in range.
isValidSlotId :: SlotId -> Bool
isValidSlotId (SlotId e s) =
    e >= 0 && s >= 0 && s < fromIntegral slotsPerEpoch
