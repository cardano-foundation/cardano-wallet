-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
module Cardano.Write.Eras
    (
    -- * Recent Eras
    -- ** Names
      Babbage
    , Conway
    , LatestLedgerEra

    -- * 'RecentEra' type
    , RecentEra (..)
    , IsRecentEra (..)
    , MaybeInRecentEra (..)
    , RecentEraConstraints

    -- ** Existential wrapper
    , AnyRecentEra (..)
    , allRecentEras

    -- ** Compatibility with "Cardano.Api"
    , CardanoApiEra
    , cardanoEraFromRecentEra
    , shelleyBasedEraFromRecentEra
    ) where

import Internal.Cardano.Write.Eras
    ( AnyRecentEra (..)
    , Babbage
    , CardanoApiEra
    , Conway
    , IsRecentEra (..)
    , LatestLedgerEra
    , MaybeInRecentEra (..)
    , RecentEra (..)
    , RecentEraConstraints
    , allRecentEras
    , cardanoEraFromRecentEra
    , shelleyBasedEraFromRecentEra
    )
