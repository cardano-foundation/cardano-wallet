-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Re-export `EraValue` library.
--

module Cardano.Wallet.Read.Eras
  ( -- * Eras.
    KnownEras
  , knownEraIndices
  , Era (..)
  , IsEra (..)
  , AnyEra (..)
  , Allegra
  , Alonzo
  , Babbage
  , Byron
  , Conway
  , Mary
  , Shelley

    -- * Era bounded values.
  , EraValue
  , eraValueSerialize
  , extractEraValue
  -- * Era specific prisms.
  , MkEraValue (..)
  , byron
  , shelley
  , allegra
  , mary
  , alonzo
  , babbage
  , conway
  -- * Era specific prism shortcuts.
  , inject
  , project
  -- * Specials.
  , sequenceEraValue
  -- * Era polymorphic functions.
  -- * Applying era functions.
  , applyEraFun
  , applyEraFunValue
  -- * Reexports from elsewhere.
  , (:.:)(..)
  , K (..)
  ,  unK
  , (:*:)(..)
  , unComp
  )
  where

import Cardano.Wallet.Read.Eras.EraFun
    ( applyEraFun
    , applyEraFunValue
    )
import Cardano.Wallet.Read.Eras.EraValue
    ( EraValue
    , MkEraValue (..)
    , allegra
    , alonzo
    , babbage
    , byron
    , conway
    , eraValueSerialize
    , extractEraValue
    , inject
    , mary
    , project
    , sequenceEraValue
    , shelley
    )
import Cardano.Wallet.Read.Eras.KnownEras
    ( Allegra
    , Alonzo
    , AnyEra (..)
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
import Generics.SOP
    ( K (..)
    , unComp
    , unK
    , (:.:) (..)
    )
import GHC.Generics
    ( (:*:) (..)
    )
