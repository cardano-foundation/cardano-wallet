{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Legacy slotting API functions.
-- Used as a reference in tests.
module Cardano.Wallet.Primitive.Slotting.Legacy
    ( SlotParameters (..)
    , slotParams
    , epochStartTime
    , flatSlot
    , fromFlatSlot
    , slotStartTime
    , slotCeiling
    , slotFloor
    , slotAt'
    , slotDifference
    , slotPred
    , slotSucc
    , slotMinBound
    , slotRangeFromTimeRange'
    ) where

import Prelude

import Cardano.Wallet.Primitive.Slotting
    ( StartTime (StartTime)
    )
import Cardano.Wallet.Primitive.Types.EpochNo
    ( EpochNo (..)
    )
import Cardano.Wallet.Primitive.Types.Range
    ( Range (Range)
    )
import Cardano.Wallet.Primitive.Types.SlotId
    ( SlotId (..)
    , SlotInEpoch (SlotInEpoch)
    )
import Cardano.Wallet.Primitive.Types.SlottingParameters
    ( ActiveSlotCoefficient
    , EpochLength (EpochLength)
    , SlotLength (SlotLength)
    , SlottingParameters
    )
import Data.Generics.Internal.VL.Lens
    ( (^.)
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Quantity
    ( Quantity (..)
    )
import Data.Time
    ( UTCTime
    )
import Data.Time.Clock
    ( NominalDiffTime
    , addUTCTime
    , diffUTCTime
    )
import Data.Word
    ( Word64
    )
import GHC.Generics
    ( Generic
    )
import Numeric.Natural
    ( Natural
    )

{-------------------------------------------------------------------------------
                           Legacy slotting functions:
                       These only work for a single era.
-------------------------------------------------------------------------------}

-- | The essential parameters necessary for performing slot arithmetic.
data SlotParameters = SlotParameters
    { getEpochLength
        :: EpochLength
    , getSlotLength
        :: SlotLength
    , getGenesisBlockDate
        :: StartTime
    , getActiveSlotCoefficient
        :: ActiveSlotCoefficient
    }
    deriving (Eq, Generic, Show)

slotParams :: StartTime -> SlottingParameters -> SlotParameters
slotParams t0 sp =
    SlotParameters
        (sp ^. #getEpochLength)
        (sp ^. #getSlotLength)
        t0
        (sp ^. #getActiveSlotCoefficient)

-- | Calculate the time at which an epoch begins.
epochStartTime :: SlotParameters -> EpochNo -> UTCTime
epochStartTime sps e = slotStartTime sps $ SlotId e 0

-- | @slotDifference a b@ is how many slots @a@ is after @b@. The result is
-- non-negative, and if @b > a@ then this function returns zero.
slotDifference :: SlotParameters -> SlotId -> SlotId -> Quantity "slot" Natural
slotDifference (SlotParameters el _ _ _) a b
    | a' > b' = Quantity $ fromIntegral $ a' - b'
    | otherwise = Quantity 0
  where
    a' = flatSlot el a
    b' = flatSlot el b

-- | Return the slot immediately before the given slot.
slotPred :: SlotParameters -> SlotId -> Maybe SlotId
slotPred (SlotParameters (EpochLength el) _ _ _) (SlotId en sn)
    | en == 0 && sn == 0 = Nothing
    | sn > 0 = Just $ SlotId en (sn - 1)
    | otherwise = Just $ SlotId (en - 1) (SlotInEpoch $ el - 1)

-- | Return the slot immediately after the given slot.
slotSucc :: SlotParameters -> SlotId -> SlotId
slotSucc (SlotParameters (EpochLength el) _ _ _) (SlotId en (SlotInEpoch sn))
    | sn < el - 1 = SlotId en (SlotInEpoch $ sn + 1)
    | otherwise = SlotId (en + 1) 0

-- | The time when a slot begins.
slotStartTime :: SlotParameters -> SlotId -> UTCTime
slotStartTime (SlotParameters el (SlotLength sl) (StartTime st) _) slot =
    addUTCTime offset st
  where
    offset = sl * fromIntegral (flatSlot el slot)

-- | For the given time 't', determine the ID of the earliest slot with start
--   time 's' such that 't ≤ s'.
slotCeiling :: SlotParameters -> UTCTime -> SlotId
slotCeiling sp@(SlotParameters _ (SlotLength sl) _ _) t =
    fromMaybe slotMinBound $ slotAt' sp (addUTCTime (pred sl) t)

-- | For the given time 't', determine the ID of the latest slot with start
--   time 's' such that 's ≤ t'.
slotFloor :: SlotParameters -> UTCTime -> Maybe SlotId
slotFloor = slotAt'

-- | Returns the earliest slot.
slotMinBound :: SlotId
slotMinBound = SlotId 0 0

-- | For the given time 't', determine the ID of the unique slot with start
--   time 's' and end time 'e' such that 's ≤ t ≤ e'.
slotAt' :: SlotParameters -> UTCTime -> Maybe SlotId
slotAt' (SlotParameters (EpochLength el) (SlotLength sl) (StartTime st) _) t
    | t < st = Nothing
    | otherwise = Just $ SlotId{epochNumber, slotNumber}
  where
    diff :: NominalDiffTime
    diff = t `diffUTCTime` st

    epochLength :: NominalDiffTime
    epochLength = fromIntegral el * sl

    epochNumber =
        EpochNo
            $ floor (diff / epochLength)

    slotNumber =
        SlotInEpoch
            $ floor ((diff - fromIntegral (unEpochNo epochNumber) * epochLength) / sl)

-- | Transforms the given inclusive time range into an inclusive slot range.
--
-- This function returns a slot range if (and only if) the specified time range
-- intersects with the life of the blockchain.
--
-- If, on the other hand, the specified time range terminates before the start
-- of the blockchain, this function returns 'Nothing'.
slotRangeFromTimeRange'
    :: SlotParameters
    -> Range UTCTime
    -> Maybe (Range SlotId)
slotRangeFromTimeRange' sps (Range mStart mEnd) =
    Range slotStart <$> slotEnd
  where
    slotStart =
        slotCeiling sps <$> mStart
    slotEnd =
        maybe (Just Nothing) (fmap Just . slotFloor sps) mEnd

-- | Convert a 'SlotId' to the number of slots since genesis.
flatSlot :: EpochLength -> SlotId -> Word64
flatSlot (EpochLength epochLength) (SlotId (EpochNo e) (SlotInEpoch s)) =
    fromIntegral epochLength * fromIntegral e + fromIntegral s

-- | Convert a 'flatSlot' index to 'SlotId'.
--
-- This function will fail if applied to a value that is higher than the maximum
-- value of 'flatSlot' for the specified 'EpochLength'.
fromFlatSlot :: EpochLength -> Word64 -> SlotId
fromFlatSlot el@(EpochLength epochLength) n
    | n <= maxFlatSlot =
        SlotId (EpochNo $ fromIntegral e) (fromIntegral s)
    | otherwise =
        error
            $ mconcat
                [ "fromFlatSlot: The specified flat slot number ("
                , show n
                , ") is higher than the maximum flat slot number ("
                , show maxFlatSlot
                , ") for the specified epoch length ("
                , show epochLength
                , ")."
                ]
  where
    e = n `div` fromIntegral epochLength
    s = n `mod` fromIntegral epochLength
    maxFlatSlot =
        flatSlot el (SlotId (EpochNo maxBound) (SlotInEpoch $ epochLength - 1))
