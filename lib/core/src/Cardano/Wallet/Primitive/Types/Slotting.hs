{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2020-2021 IOHK
-- License: Apache-2.0
--
-- Basic types for identifying an ouroboros slot.
--
module Cardano.Wallet.Primitive.Types.Slotting
    ( EpochNo (..)
    , SlotNo (..)
    , SlotId (..)
    , SlotLength (..)
    , SlotInEpoch (..)
    , EpochLength (..)
    , StartTime (..)
    , unsafeEpochNo
    , isValidEpochNo
    ) where

import Prelude

import Cardano.Slotting.Slot
    ( SlotNo (..) )
import Cardano.Wallet.Util
    ( HasCallStack, internalError )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( (<=<) )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Data.Time.Clock
    ( NominalDiffTime, UTCTime )
import Data.Word
    ( Word32 )
import Data.Word.Odd
    ( Word31 )
import Fmt
    ( Buildable (..), (+||), (||+) )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )

import qualified Data.Text as T

{-------------------------------------------------------------------------------
                                   Slotting
-------------------------------------------------------------------------------}

-- | A slot identifier is the combination of an epoch and slot.
data SlotId = SlotId
  { epochNumber :: !EpochNo
  , slotNumber :: !SlotInEpoch
  } deriving stock (Show, Read, Eq, Ord, Generic)

newtype SlotInEpoch = SlotInEpoch { unSlotInEpoch :: Word32 }
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving newtype (Num, Buildable, NFData, Enum)

newtype EpochNo = EpochNo { unEpochNo :: Word31 }
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving newtype (Num, Bounded, Enum)

instance ToText EpochNo where
    toText = T.pack . show . unEpochNo

instance FromText EpochNo where
    fromText = validate <=< (fmap (EpochNo . fromIntegral) . fromText @Natural)
      where
        validate x
            | isValidEpochNo x =
                return x
            | otherwise =
                Left $ TextDecodingError "EpochNo value is out of bounds"

isValidEpochNo :: EpochNo -> Bool
isValidEpochNo c = c >= minBound && c <= maxBound

instance Buildable EpochNo where
    build (EpochNo e) = build $ fromIntegral @Word31 @Word32 e

instance NFData EpochNo where
    rnf (EpochNo !_) = ()

-- | Convert the specified value into an 'EpochNo', or fail if the value is
--   too large.
unsafeEpochNo :: HasCallStack => Word32 -> EpochNo
unsafeEpochNo epochNo
    | epochNo <= maxEpochNo = EpochNo $ fromIntegral epochNo
    | otherwise = internalError $
        "unsafeEpochNo: epoch number ("+||epochNo||+") out of bounds" <>
        " ("+||minBound @Word31||+", "+||maxBound @Word31||+")."
  where
    maxEpochNo :: Word32
    maxEpochNo = fromIntegral @Word31 $ unEpochNo maxBound


instance NFData SlotId

instance Buildable SlotId where
    build (SlotId (EpochNo e) (SlotInEpoch s)) = e||+"."+||s||+""

-- | Duration of a single slot.
newtype SlotLength = SlotLength { unSlotLength :: NominalDiffTime }
    deriving (Show, Eq, Generic)

instance NFData SlotLength

-- | Number of slots in a single epoch
newtype EpochLength = EpochLength { unEpochLength :: Word32 }
    deriving (Show, Eq, Generic)

instance NFData EpochLength

-- | Blockchain start time
newtype StartTime = StartTime UTCTime
    deriving (Show, Eq, Ord, Generic)

instance NFData StartTime
