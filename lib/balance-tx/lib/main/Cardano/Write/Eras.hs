-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
module Cardano.Write.Eras
    ( -- * Recent Eras

      -- ** Names
      Babbage
    , Conway
    , LatestLedgerEra

      -- * 'RecentEra' type
    , RecentEra (..)
    , IsRecentEra (..)
    , RecentEraConstraints

      -- ** Wrappers
    , AnyRecentEra (..)
    , allRecentEras
    , InAnyRecentEra (..)
    , toInAnyRecentEra
    , MaybeInRecentEra (..)

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
    , InAnyRecentEra (..)
    , IsRecentEra (..)
    , LatestLedgerEra
    , MaybeInRecentEra (..)
    , RecentEra (..)
    , RecentEraConstraints
    , allRecentEras
    , cardanoEraFromRecentEra
    , shelleyBasedEraFromRecentEra
    , toInAnyRecentEra
    )
