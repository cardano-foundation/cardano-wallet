{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
--
-- Provides protocol parameters.
--
module Cardano.Write.ProtocolParameters
    ( ProtocolParameters (..)
    ) where

import Prelude

import Data.Functor.Identity
    ( Identity )

import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Write.Tx as Write

-- TODO:
--  - Make this data type abstract: don't export the constructor.
--  - Replace this type with a re-exported 'Ledger.PParams era'.
newtype ProtocolParameters era = ProtocolParameters
    { pparamsLedger :: Ledger.PParams (Write.ShelleyLedgerEra era)
    }

deriving newtype instance
    Eq (Ledger.PParamsHKD Identity (Write.ShelleyLedgerEra era)) =>
    Eq (ProtocolParameters era)

deriving newtype instance
    Show (Ledger.PParamsHKD Identity (Write.ShelleyLedgerEra era)) =>
    Show (ProtocolParameters era)
