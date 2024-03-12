{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- A type list of known eras, useful for indexed-by-era operations.

module Cardano.Wallet.Read.Eras.KnownEras
    ( KnownEras
    , knownEraIndices

    , Era (..)
    , AnyEra (..)
    , IsEra (..)
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
import Generics.SOP
    ( Proxy (..)
    , lengthSList
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

deriving instance Show (Era era)

-- | Existentially quantified era.
data AnyEra where
    AnyEra :: forall era. Era era -> AnyEra

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

-- | Type-level list of known eras.
type KnownEras =
    '[Byron, Shelley, Allegra, Mary, Alonzo, Babbage, Conway]

-- | Official numbering of the KnownEras.
knownEraIndices :: [Int]
knownEraIndices = [0 .. lengthSList (Proxy :: Proxy KnownEras) - 1]
