-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- A datatype that represents values that can be different in any known eras.
--
-- This module re-exports 'Cardano.Read.Ledger.Eras.EraValue' for compatibility.
module Cardano.Wallet.Read.Eras.EraValue
    ( -- * Era-existential values
      EraValue (..)
    , getEra
    , knownEras

    , applyEraFun
    , applyEraFunValue
    , extractEraValue
    , sequenceEraValue

    , parseEraIndex
    , eraValueSerialize
    )
where

import Cardano.Read.Ledger.Eras.EraValue
    ( EraValue (..)
    , applyEraFun
    , applyEraFunValue
    , eraValueSerialize
    , extractEraValue
    , getEra
    , knownEras
    , parseEraIndex
    , sequenceEraValue
    )
