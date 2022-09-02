{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Types.Read.Eras.KnownEras
  (KnownEras, knownEraIndices) where

import Prelude

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, MaryEra, ShelleyEra )
import Generics.SOP
    ( Proxy (..), lengthSList )

-- | Known eras, for simplicity we reuse the types  from Cardano.API
type KnownEras =
    '[ByronEra, ShelleyEra, AllegraEra, MaryEra, AlonzoEra, BabbageEra]

-- | Official numbering of the KnownEras
knownEraIndices :: [Int]
knownEraIndices = [0 .. lengthSList (Proxy @KnownEras) - 1]
