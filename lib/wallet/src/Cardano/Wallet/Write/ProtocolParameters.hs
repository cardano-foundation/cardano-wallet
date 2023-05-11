{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
--
-- Provides protocol parameters.
--
module Cardano.Wallet.Write.ProtocolParameters
    ( ProtocolParameters (..)
    ) where

import Prelude

import qualified Cardano.Wallet.Write.Tx as Write

-- TODO:
--  - Make this data type abstract: don't export the constructor.
--  - Replace this type with a re-exported 'Ledger.PParams era'.
newtype ProtocolParameters era = ProtocolParameters
    { pparamsLedger
        :: Write.PParams (Write.ShelleyLedgerEra era)
    }

deriving instance Eq (Write.PParams (Write.ShelleyLedgerEra era)) => Eq (ProtocolParameters era)
deriving instance Show (Write.PParams (Write.ShelleyLedgerEra era)) => Show (ProtocolParameters era)

