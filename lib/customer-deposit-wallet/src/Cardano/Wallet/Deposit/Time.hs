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

    -- * from Write
    , Write.TimeTranslation
    , toTimeTranslation
    ) where

import Prelude

import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException
    , StartTime (..)
    , mkSingleEraInterpreter
    )
import Cardano.Wallet.Primitive.Slotting.TimeTranslation
    ( toTimeTranslation
    )
import Cardano.Wallet.Primitive.Types.SlottingParameters
    ( ActiveSlotCoefficient (..)
    , EpochLength (..)
    , SlotLength (..)
    , SlottingParameters (..)
    )
import Data.Functor.Identity
    ( Identity (..)
    )
import Data.Quantity
    ( Quantity (..)
    )
import Data.Time.Clock
    ( UTCTime (..)
    )

import qualified Cardano.Wallet.Primitive.Slotting as Primitive
import qualified Cardano.Write.Tx as Write

{-----------------------------------------------------------------------------
    Time
------------------------------------------------------------------------------}
type TimeInterpreter = Primitive.TimeInterpreter Identity

mockTimeInterpreter :: TimeInterpreter
mockTimeInterpreter =
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
