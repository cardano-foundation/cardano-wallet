{-# LANGUAGE NumericUnderscores #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Indirection module that re-exports types
-- related to computations involving Slots and wall-clock times.
--
-- TODO: Absorb this into a definition of 'TimeInterpreter'.
module Cardano.Wallet.Deposit.Time
    ( -- * from Primitive
      TimeInterpreter
    , PastHorizonException
    , mockTimeInterpreter

    , slotToUTCTime

    -- * from Write
    , Write.TimeTranslation
    , toTimeTranslation
    , toTimeTranslationPure

    -- * wishlist
    , LookupTimeFromSlot
    , unsafeUTCTimeOfSlot
    , unsafeSlotOfUTCTime
    , systemStartMainnet
    , originTime

    ) where

import Prelude

import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException
    , StartTime (..)
    , hoistTimeInterpreter
    , interpretQuery
    , mkSingleEraInterpreter
    )
import Cardano.Wallet.Primitive.Slotting.TimeTranslation
    ( toTimeTranslation
    , toTimeTranslationPure
    )
import Cardano.Wallet.Primitive.Types.SlottingParameters
    ( ActiveSlotCoefficient (..)
    , EpochLength (..)
    , SlotLength (..)
    , SlottingParameters (..)
    )
import Cardano.Wallet.Read
    ( Slot
    , SlotNo (..)
    , WithOrigin (..)
    )
import Data.Functor.Identity
    ( Identity (..)
    )
import Data.IntCast
    ( intCastMaybe
    )
import Data.Quantity
    ( Quantity (..)
    )
import Data.Time.Clock
    ( UTCTime (..)
    )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime
    , utcTimeToPOSIXSeconds
    )

import qualified Cardano.Wallet.Primitive.Slotting as Primitive
import qualified Cardano.Wallet.Read as Read
import qualified Cardano.Write.Tx as Write

{-----------------------------------------------------------------------------
    TimeInterpreter
------------------------------------------------------------------------------}
type TimeInterpreter = Primitive.TimeInterpreter (Either PastHorizonException)

mockTimeInterpreter :: Primitive.TimeInterpreter Identity
mockTimeInterpreter = hoistTimeInterpreter (pure . runIdentity) $
    mkSingleEraInterpreter
        (StartTime $ UTCTime (toEnum 0) 0)
        mockSlottingParameters

mockSlottingParameters :: SlottingParameters
mockSlottingParameters = SlottingParameters
    { getSlotLength = SlotLength 1
    , getEpochLength = EpochLength 21_600
    , getActiveSlotCoefficient = ActiveSlotCoefficient 1
    , getSecurityParameter = Quantity 2_160
    }

{-----------------------------------------------------------------------------
    TimeInterpreter
------------------------------------------------------------------------------}

type LookupTimeFromSlot = Slot -> Maybe (WithOrigin UTCTime)

-- | Look up the UTCTime corresponding to the start of the provided `Slot`.
--
-- TODO: Check roundtrip properties once we need to implement the corresponding 'utcTimeToSlot'.
slotToUTCTime :: TimeInterpreter -> LookupTimeFromSlot
slotToUTCTime _ti Origin = Just Origin
slotToUTCTime ti (At s) = either (const Nothing) (Just . At) . interpretQuery ti . Primitive.slotToUTCTime =<< convertSlotNo s
  where
    convertSlotNo :: SlotNo -> Maybe Primitive.SlotNo
    convertSlotNo (SlotNo n) = Primitive.SlotNo <$> intCastMaybe n

-- TODO: Rename to mainnetUTCTimeOfSlot
-- TODO: Move to tests?
unsafeUTCTimeOfSlot :: Slot -> Maybe (WithOrigin UTCTime)
unsafeUTCTimeOfSlot Origin = Just Origin
unsafeUTCTimeOfSlot (At (SlotNo n)) =
    Just . At
        $ posixSecondsToUTCTime
        $ fromIntegral pt
  where
    pts = fromIntegral n - byronSlots
    pt =
        if pts >= 0
            then shelleyTime + pts
            else shelleyTime + pts * 20

unsafeSlotOfUTCTime :: UTCTime -> Read.Slot
unsafeSlotOfUTCTime t
    | origin = Origin
    | byron = At $ SlotNo $ fromIntegral $ (pt - originTime) `div` 20
    | otherwise = At $ SlotNo $ fromIntegral $ pt - shelleyTime + byronSlots
  where
    pt = floor $ utcTimeToPOSIXSeconds t
    origin = pt < originTime
    byron = pt < shelleyTime

byronSlots :: Integer
byronSlots = 4_924_800

shelleyTime :: Integer
shelleyTime = 1_596_491_091

originTime :: Integer
originTime = shelleyTime - byronSlots * 20

systemStartMainnet :: UTCTime
systemStartMainnet = posixSecondsToUTCTime $ fromIntegral originTime
