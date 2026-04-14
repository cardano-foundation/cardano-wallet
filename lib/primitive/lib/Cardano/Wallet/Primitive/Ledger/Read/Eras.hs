{-# LANGUAGE GADTs #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Convert from Cardano.Api to Cardano.Wallet.Read.
module Cardano.Wallet.Primitive.Ledger.Read.Eras
    ( fromAnyCardanoEra
    , toAnyCardanoEra
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
        DijkstraEra -> Read.EraValue Read.Dijkstra

-- | Convert an era from 'Read' to 'Cardano.Api'.
toAnyCardanoEra :: Read.EraValue Read.Era -> AnyCardanoEra
toAnyCardanoEra (Read.EraValue era) =
    case era of
        Read.Byron -> AnyCardanoEra ByronEra
        Read.Shelley -> AnyCardanoEra ShelleyEra
        Read.Allegra -> AnyCardanoEra AllegraEra
        Read.Mary -> AnyCardanoEra MaryEra
        Read.Alonzo -> AnyCardanoEra AlonzoEra
        Read.Babbage -> AnyCardanoEra BabbageEra
        Read.Conway -> AnyCardanoEra ConwayEra
        Read.Dijkstra -> AnyCardanoEra DijkstraEra
