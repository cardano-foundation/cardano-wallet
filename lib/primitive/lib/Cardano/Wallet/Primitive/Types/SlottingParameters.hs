{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}

module Cardano.Wallet.Primitive.Types.SlottingParameters
    ( SlottingParameters (..)
    , ActiveSlotCoefficient (..)
    , SlotLength (..)
    , EpochLength (..)
    , stabilityWindowByron
    , stabilityWindowShelley
    ) where

import Prelude

import Cardano.Slotting.Slot
    ( SlotNo (..)
    )
import Control.DeepSeq
    ( NFData
    )
import Control.Lens
    ( (^.)
    )
import Data.Generics.Labels
    ()
import Data.Quantity
    ( Quantity (getQuantity)
    )
import Data.Time
    ( NominalDiffTime
    )
import Data.Word
    ( Word32
    )
import Fmt
    ( Buildable (..)
    , blockListF'
    )
import GHC.Generics
    ( Generic
    )

newtype SlotLength = SlotLength {unSlotLength :: NominalDiffTime}
    deriving (Show, Eq, Generic)

instance NFData SlotLength

-- | Number of slots in a single epoch
newtype EpochLength = EpochLength {unEpochLength :: Word32}
    deriving (Show, Eq, Generic)

instance NFData EpochLength

data SlottingParameters = SlottingParameters
    { getSlotLength :: SlotLength
    -- ^ Length, in seconds, of a slot.
    , getEpochLength :: EpochLength
    -- ^ Number of slots in a single epoch.
    , getActiveSlotCoefficient :: ActiveSlotCoefficient
    -- ^ a.k.a 'f', in Genesis/Praos, corresponds to the % of active slots
    -- (i.e. slots for which someone can be elected as leader).
    --
    -- Determines the value of 'stabilityWindowShelley'.
    , getSecurityParameter :: Quantity "block" Word32
    -- ^ a.k.a 'k', used to compute the 'stability window' on the chain
    -- (i.e. the longest possible chain fork in slots).
    --
    -- Determines the value of 'stabilityWindowByron' and
    -- 'stabilityWindowShelley'.
    }
    deriving (Generic, Show, Eq)

instance NFData SlottingParameters

-- | In Byron, this stability window is equal to 2k slots, where _k_ is the
--  'getSecurityParameter'
stabilityWindowByron :: SlottingParameters -> SlotNo
stabilityWindowByron sp = SlotNo (2 * k)
  where
    k = fromIntegral $ getQuantity $ getSecurityParameter sp

-- | In Shelley, this stability window is equal to _3k/f_ slots where _k_ is the
-- 'getSecurityParameter' and _f_ is the 'ActiveSlotCoefficient'.
stabilityWindowShelley :: SlottingParameters -> SlotNo
stabilityWindowShelley sp = SlotNo len
  where
    len = ceiling (3 * k / f)
    k = fromIntegral $ getQuantity $ getSecurityParameter sp
    f = unActiveSlotCoefficient $ getActiveSlotCoefficient sp

instance Buildable SlottingParameters where
    build sp =
        blockListF'
            ""
            id
            [ "Slot length:        " <> slotLengthF (getSlotLength sp)
            , "Epoch length:       " <> epochLengthF (getEpochLength sp)
            , "Active slot coeff:  " <> build (sp ^. #getActiveSlotCoefficient)
            , "Security parameter: " <> build (sp ^. #getSecurityParameter)
            ]
      where
        slotLengthF (SlotLength s) = build s
        epochLengthF (EpochLength s) = build s

newtype ActiveSlotCoefficient = ActiveSlotCoefficient {unActiveSlotCoefficient :: Double}
    deriving stock (Generic, Eq, Show)
    deriving newtype (Buildable, Num, Fractional, Real, Ord, RealFrac)

instance NFData ActiveSlotCoefficient
