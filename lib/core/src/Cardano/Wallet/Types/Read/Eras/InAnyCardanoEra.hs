{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Types.Read.Eras.InAnyCardanoEra (isoInAnyCardanoEra)
  where

import Prelude

import Cardano.Api
    ( CardanoEra (..), InAnyCardanoEra (InAnyCardanoEra), IsCardanoEra )
import Cardano.Wallet.Types.Read.Eras.EraFun
    ( EraFun (..), applyEraFun )
import Cardano.Wallet.Types.Read.Eras.EraValue
    ( EraValue
    , MkEraValue (..)
    , allegra
    , alonzo
    , babbage
    , byron
    , eraValueSerialize
    , mary
    , shelley
    )
import Cardano.Wallet.Types.Read.Eras.KnownEras
    ( KnownEras )
import Data.Generics.Internal.VL
    ( Iso', build, iso )
import Generics.SOP
    ( K (..), NP (..), Proxy (Proxy) )
import Generics.SOP.Classes
import Generics.SOP.NP
    ( cmap_NP )

toInAnyCardanoEra :: EraValue f -> InAnyCardanoEra f
toInAnyCardanoEra = fst . build eraValueSerialize . applyEraFun existentials
    where
    cardanoEras :: NP CardanoEra KnownEras
    cardanoEras =
        ByronEra
            :* ShelleyEra
            :* AllegraEra
            :* MaryEra
            :* AlonzoEra
            :* BabbageEra
            :* Nil

    mkExistential
        :: IsCardanoEra era
        => CardanoEra era
        -> (f -.-> K (InAnyCardanoEra f)) era
    mkExistential e = Fn (K . InAnyCardanoEra e)

    existentials :: EraFun src (K (InAnyCardanoEra src))
    existentials = EraFun $ cmap_NP (Proxy @IsCardanoEra) mkExistential cardanoEras

fromInAnyCardanoEra :: InAnyCardanoEra f -> EraValue f
fromInAnyCardanoEra (InAnyCardanoEra era x) = case era of
  ByronEra -> inject byron x
  ShelleyEra -> inject shelley x
  AllegraEra -> inject allegra x
  MaryEra -> inject mary x
  AlonzoEra -> inject alonzo x
  BabbageEra -> inject babbage x
  where
    inject :: MkEraValue f era -> f era -> EraValue f
    inject (MkEraValue p) = build p

-- | an isomorphism between InAnyCardanoEra and EraValue
isoInAnyCardanoEra :: Iso' (EraValue f) (InAnyCardanoEra f)
isoInAnyCardanoEra = iso toInAnyCardanoEra fromInAnyCardanoEra
