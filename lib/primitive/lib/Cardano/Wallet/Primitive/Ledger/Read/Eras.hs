{-# LANGUAGE GADTs #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Convert from Cardano.Api to Cardano.Wallet.Read.
module Cardano.Wallet.Primitive.Ledger.Read.Eras
    ( fromAnyCardanoEra
    )
where

import Cardano.Api
    ( AnyCardanoEra (..)
    , CardanoEra (..)
    )

import qualified Cardano.Wallet.Read as Read

-- | Convert an era from 'Cardano.Api' to 'Read'.
fromAnyCardanoEra :: AnyCardanoEra -> Read.EraValue Read.Era
fromAnyCardanoEra (AnyCardanoEra era) =
    case era of
        ByronEra -> Read.EraValue Read.Byron
        ShelleyEra -> Read.EraValue Read.Shelley
        AllegraEra -> Read.EraValue Read.Allegra
        MaryEra -> Read.EraValue Read.Mary
        AlonzoEra -> Read.EraValue Read.Alonzo
        BabbageEra -> Read.EraValue Read.Babbage
        ConwayEra -> Read.EraValue Read.Conway
