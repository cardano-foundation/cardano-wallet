{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK, 2024 Cardano Foundation
-- License: Apache-2.0
--
-- A type list of known eras, useful for indexing types by era.
module Cardano.Read.Ledger.Eras.KnownEras
    ( Era (..)
    , IsEra (..)
    , KnownEras
    , indexOfEra
    , Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Mary
    , Shelley
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

type Byron = ByronEra
type Shelley = ShelleyEra
type Allegra = AllegraEra
type Mary = MaryEra
type Alonzo = AlonzoEra
type Babbage = BabbageEra
type Conway = ConwayEra

-- | Singleton type for eras.
--
-- This GADT provides a value-level representation of eras.
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

-- | Singleton class for eras.
class IsEra era where
    theEra :: Era era

instance IsEra Byron where theEra = Byron
instance IsEra ShelleyEra where theEra = Shelley
instance IsEra AllegraEra where theEra = Allegra
instance IsEra MaryEra where theEra = Mary
instance IsEra AlonzoEra where theEra = Alonzo
instance IsEra BabbageEra where theEra = Babbage
instance IsEra ConwayEra where theEra = Conway

-- | Type-level list of known eras, in chronological order.
type KnownEras =
    '[ Byron
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
