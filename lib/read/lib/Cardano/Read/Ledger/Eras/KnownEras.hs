{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Copyright: © 2020-2022 IOHK, 2024 Cardano Foundation
License: Apache-2.0

A type list of known eras, useful for indexing types by era.
-}
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
    ( Allegra
    , Alonzo
    , Babbage
    , ByronEra
    , Conway
    , Mary
    , Shelley
    , StandardCrypto
    )

type Byron = ByronEra StandardCrypto

-- | Singleton type for eras.
--
-- This GADT provides a value-level representation of eras.
data Era era where
    Byron :: Era Byron
    Shelley :: Era Shelley
    Allegra :: Era Allegra
    Mary :: Era Mary
    Alonzo :: Era Alonzo
    Babbage :: Era Babbage
    Conway :: Era Conway

deriving instance Eq (Era era)
deriving instance Show (Era era)

-- | Singleton class for eras.
class IsEra era where
    theEra :: Era era

instance IsEra Byron where theEra = Byron
instance IsEra Shelley where theEra = Shelley
instance IsEra Allegra where theEra = Allegra
instance IsEra Mary where theEra = Mary
instance IsEra Alonzo where theEra = Alonzo
instance IsEra Babbage where theEra = Babbage
instance IsEra Conway where theEra = Conway

-- | Type-level list of known eras, in chronological order.
type KnownEras =
    '[Byron, Shelley, Allegra, Mary, Alonzo, Babbage, Conway]

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
