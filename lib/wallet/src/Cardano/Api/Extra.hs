{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Module containing extra 'Cardano.Api' functionality needed by the wallet.
module Cardano.Api.Extra
    ( inAnyCardanoEra
    , unbundleLedgerShelleyBasedProtocolParams
    ) where

import Cardano.Api
    ( BundledProtocolParameters (..), InAnyCardanoEra (..),
    IsCardanoEra (cardanoEra), ShelleyBasedEra (..), Tx )
import Cardano.Api.Shelley
    ( ShelleyLedgerEra )

import qualified Cardano.Ledger.Core as Ledger

-- | Helper function for more easily creating an existential
-- @InAnyCardanoEra Tx@.
inAnyCardanoEra :: IsCardanoEra era => Tx era -> InAnyCardanoEra Tx
inAnyCardanoEra = InAnyCardanoEra cardanoEra

-- Not exposed by cardano-api
unbundleLedgerShelleyBasedProtocolParams
  :: ShelleyBasedEra era
  -> BundledProtocolParameters era
  -> Ledger.PParams (ShelleyLedgerEra era)
unbundleLedgerShelleyBasedProtocolParams = \case
  ShelleyBasedEraShelley -> \(BundleAsShelleyBasedProtocolParameters _ _ lpp) -> lpp
  ShelleyBasedEraAllegra -> \(BundleAsShelleyBasedProtocolParameters _ _ lpp) -> lpp
  ShelleyBasedEraMary -> \(BundleAsShelleyBasedProtocolParameters _ _ lpp) -> lpp
  ShelleyBasedEraAlonzo -> \(BundleAsShelleyBasedProtocolParameters _ _ lpp) -> lpp
  ShelleyBasedEraBabbage -> \(BundleAsShelleyBasedProtocolParameters _ _ lpp) -> lpp
  ShelleyBasedEraConway -> \(BundleAsShelleyBasedProtocolParameters _ _ lpp) -> lpp
