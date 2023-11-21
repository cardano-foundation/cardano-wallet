{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Wallet.Primitive.Types.SlotId
    ( SlotId (..)
    , SlotInEpoch (..)
    )
where

import Prelude

import Cardano.Wallet.Primitive.Types.EpochNo
    ( EpochNo (EpochNo)
    )
import Control.DeepSeq
    ( NFData
    )
import Data.String
    ( IsString (fromString)
    )
import Data.Word
    ( Word32
    )
import Fmt
    ( Buildable (..)
    )
import GHC.Generics
    ( Generic
    )

-- | A slot identifier is the combination of an epoch and slot.
data SlotId = SlotId
  { epochNumber :: !EpochNo
  , slotNumber :: !SlotInEpoch
  } deriving stock (Show, Read, Eq, Ord, Generic)

newtype SlotInEpoch = SlotInEpoch { unSlotInEpoch :: Word32 }
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving newtype (Num, Buildable, NFData, Enum)

instance NFData SlotId

instance Buildable SlotId where
    build (SlotId (EpochNo e) (SlotInEpoch s)) =
        fromString (show e) <> "." <> fromString (show s)
