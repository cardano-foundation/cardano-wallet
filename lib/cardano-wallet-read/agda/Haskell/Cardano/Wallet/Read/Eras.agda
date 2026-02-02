{-# OPTIONS --erasure #-}

-- Synchronized manually with the corresponding Haskell module.
module Haskell.Cardano.Wallet.Read.Eras where

open import Haskell.Prelude

{-----------------------------------------------------------------------------
    Eras
------------------------------------------------------------------------------}
data Byron   : Set where ByronEraC   : Byron
data Shelley : Set where ShelleyEraC : Shelley
data Allegra : Set where AllegraEraC : Allegra
data Mary    : Set where MaryEraC    : Mary
data Alonzo  : Set where AlonzoEraC  : Alonzo
data Babbage : Set where BabbageEraC : Babbage
data Conway  : Set where ConwayEraC  : Conway

data Era : Set → Set where
    ByronC   : Era Byron
    ShelleyC : Era Shelley
    AllegraC : Era Allegra
    MaryC    : Era Mary
    AlonzoC  : Era Alonzo
    BabbageC : Era Babbage
    ConwayC  : Era Conway

record IsEra (era : Set) : Set where
  field
    theEra : Era era

{-# COMPILE AGDA2HS IsEra existing-class #-}

instance
  iIsEraByron : IsEra Byron
  iIsEraByron = record { theEra = ByronC }

  iIsEraShelley : IsEra Shelley
  iIsEraShelley = record { theEra = ShelleyC }

  iIsEraAllegra : IsEra Allegra
  iIsEraAllegra = record { theEra = AllegraC }

  iIsEraMary : IsEra Mary
  iIsEraMary = record { theEra = MaryC }

  iIsEraAlonzo : IsEra Alonzo
  iIsEraAlonzo = record { theEra = AlonzoC }

  iIsEraBabbage : IsEra Babbage
  iIsEraBabbage = record { theEra = BabbageC }

  iIsEraConway : IsEra Conway
  iIsEraConway = record { theEra = ConwayC }
