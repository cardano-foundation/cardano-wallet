{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Cardano.Write.Tx.TimeTranslation
    ( TimeTranslation
    , timeTranslationFromEpochInfo
    , systemStartTime
    , epochInfo
    ) where

import Prelude

import Cardano.Slotting.EpochInfo
    ( EpochInfo )
import Cardano.Slotting.Time
    ( SystemStart )
import Ouroboros.Consensus.HardFork.History
    ( PastHorizonException )

data TimeTranslation = TimeTranslation
    (EpochInfo (Either PastHorizonException))
    SystemStart

timeTranslationFromEpochInfo
    :: SystemStart
    -> EpochInfo (Either PastHorizonException)
    -> TimeTranslation
timeTranslationFromEpochInfo systemStart info =
    TimeTranslation info systemStart

systemStartTime :: TimeTranslation -> SystemStart
systemStartTime (TimeTranslation _epochInfo systemStart) = systemStart

epochInfo :: TimeTranslation -> EpochInfo (Either PastHorizonException)
epochInfo (TimeTranslation info _systemStart) = info
