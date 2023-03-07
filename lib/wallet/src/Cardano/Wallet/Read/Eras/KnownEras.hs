{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- A type list of known eras, useful for indexed-by-era operations.

module Cardano.Wallet.Read.Eras.KnownEras
    ( KnownEras
    , knownEraIndices
    ) where

import Prelude

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, ConwayEra, MaryEra, ShelleyEra )
import Generics.SOP
    ( Proxy (..), lengthSList )

-- | Known eras, for simplicity we reuse the types from 'Cardano.API'.
type KnownEras =
    '[ByronEra, ShelleyEra, AllegraEra, MaryEra, AlonzoEra, BabbageEra, ConwayEra]

-- | Official numbering of the KnownEras.
knownEraIndices :: [Int]
knownEraIndices = [0 .. lengthSList (Proxy @KnownEras) - 1]
