{- |
Copyright: © 2020-2022 IOHK, 2024 Cardano Foundation
License: Apache-2.0

A type list of known eras, useful for indexing types by era.
-}
module Cardano.Read.Ledger.Eras.KnownEras
    ( -- * Era singleton
      Era (..)
    , IsEra (..)

      -- * Known eras
    , KnownEras
    , indexOfEra

      -- * Era type aliases
    , Byron
    , Shelley
    , Allegra
    , Mary
    , Alonzo
    , Babbage
    , Conway
    ) where

import Prelude

import Cardano.Ledger.Api
    ( AllegraEra
    , AlonzoEra
    , BabbageEra
    , ByronEra
    , ConwayEra
    , MaryEra
    , ShelleyEra
    )

-- | The Byron era (pre-Shelley).
type Byron = ByronEra

-- | The Shelley era (first proof-of-stake era).
type Shelley = ShelleyEra

-- | The Allegra era (token locking).
type Allegra = AllegraEra

-- | The Mary era (multi-asset support).
type Mary = MaryEra

-- | The Alonzo era (Plutus smart contracts).
type Alonzo = AlonzoEra

-- | The Babbage era (Plutus V2, reference inputs).
type Babbage = BabbageEra

-- | The Conway era (governance).
type Conway = ConwayEra

{- | Singleton type for eras.

This GADT provides a value-level representation of eras.
-}
data Era era where
    Byron :: Era ByronEra
    Shelley :: Era ShelleyEra
    Allegra :: Era AllegraEra
    Mary :: Era MaryEra
    Alonzo :: Era AlonzoEra
    Babbage :: Era BabbageEra
    Conway :: Era ConwayEra

deriving instance Eq (Era era)
deriving instance Show (Era era)

-- |
-- Singleton class for eras.
--
-- This class provides a way to obtain the 'Era' value for a given era type.
-- All known eras have instances of this class.
class IsEra era where
    -- | Get the singleton 'Era' value for this era type.
    theEra :: Era era

instance IsEra ByronEra where theEra = Byron
instance IsEra ShelleyEra where theEra = Shelley
instance IsEra AllegraEra where theEra = Allegra
instance IsEra MaryEra where theEra = Mary
instance IsEra AlonzoEra where theEra = Alonzo
instance IsEra BabbageEra where theEra = Babbage
instance IsEra ConwayEra where theEra = Conway

-- | Type-level list of known eras, in chronological order.
type KnownEras =
    '[ ByronEra
     , ShelleyEra
     , AllegraEra
     , MaryEra
     , AlonzoEra
     , BabbageEra
     , ConwayEra
     ]

-- | Official numbering of the members of 'KnownEras'.
indexOfEra :: Era era -> Int
indexOfEra e = case e of
    Byron -> 0
    Shelley -> 1
    Allegra -> 2
    Mary -> 3
    Alonzo -> 4
    Babbage -> 5
    Conway -> 6
