-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Re-export `EraValue` library.
--

module Cardano.Wallet.Read.Eras
  ( -- * Eras
    Era (..)
  , IsEra (..)
  , Allegra
  , Alonzo
  , Babbage
  , Byron
  , Conway
  , Mary
  , Shelley

  , KnownEras
  , knownEraIndices

    -- * Era-existential values
  , EraValue (..)
  , eraValueSerialize

  -- * Specials
  , sequenceEraValue

  -- * Era polymorphic functions.
  -- * Applying era functions.
  , applyEraFun
  , applyEraFunValue

  -- * Re-export structure combinators
  , (:.:)(..)
  , K (..)
  , unK
  , (:*:)(..)
  , unComp
  )
  where

import Cardano.Read.Ledger.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Era (..)
    , IsEra (..)
    , KnownEras
    , Mary
    , Shelley
    , knownEraIndices
    )
import Cardano.Wallet.Read.Eras.EraFun
    ( applyEraFun
    , applyEraFunValue
    )
import Cardano.Wallet.Read.Eras.EraValue
    ( EraValue (..)
    , eraValueSerialize
    , sequenceEraValue
    )
import Generics.SOP
    ( K (..)
    , unComp
    , unK
    , (:.:) (..)
    )
import GHC.Generics
    ( (:*:) (..)
    )
