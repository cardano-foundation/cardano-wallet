-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Re-export `EraValue` library.
module Cardano.Wallet.Read.Eras
    ( -- * Eras
      Era (..)
    , IsEra (..)
    , Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Dijkstra
    , Mary
    , Shelley
    , KnownEras
    , knownEras
    , indexOfEra
    , parseEraIndex

      -- * Era-existential values
    , EraValue (..)
    , getEra
    , eraValueSerialize

      -- * Specials
    , sequenceEraValue

      -- * Era polymorphic functions.

      -- * Applying era functions.
    , applyEraFun
    , applyEraFunValue

      -- * Re-export structure combinators
    , (:.:) (..)
    , K (..)
    , unK
    , (:*:) (..)
    , unComp
    )
where

import Cardano.Read.Ledger.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Dijkstra
    , Era (..)
    , IsEra (..)
    , KnownEras
    , Mary
    , Shelley
    , indexOfEra
    )
import Cardano.Wallet.Read.Eras.EraValue
    ( EraValue (..)
    , applyEraFun
    , applyEraFunValue
    , eraValueSerialize
    , getEra
    , knownEras
    , parseEraIndex
    , sequenceEraValue
    )
import GHC.Generics
    ( (:*:) (..)
    )
import Generics.SOP
    ( K (..)
    , unComp
    , unK
    , (:.:) (..)
    )
