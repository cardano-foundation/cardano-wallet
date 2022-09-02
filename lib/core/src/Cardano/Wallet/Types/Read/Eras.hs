
module Cardano.Wallet.Types.Read.Eras
  ( -- * eras
    KnownEras
  , knownEraIndices
    -- * era bounded values
  , EraValue
  , eraValueSerialize
  , eraValueS
  , fromEraValueS
  , toEraValueS
  , extractEraValue
  -- * era specific prisms
  , MkEraValue (..)
  , byron
  , shelley
  , allegra
  , mary
  , alonzo
  , babbage
  -- * era specific prism shortcuts
  , inject
  , eject
  -- * sum type encoding
  , EraValueS (..)
  -- * specials
  , sequenceEraValue
  -- * era bounded functions
  , EraFun
  , eraFunR
  , EraFunR(..)
  , fromEraFunR
  , toEraFunR
  -- * composing era functions
  , (*.*)
  , (*.**)
  , (*-*)
  -- * applying era functions
  , applyEraFun
  -- * reexports
  , (:.:)(..)
  , K (..)
  ,  unK
  , (:*:)(..)
  -- * conversion
  , isoInAnyCardanoEra
  )
  where

import Cardano.Wallet.Types.Read.Eras.EraFun
import Cardano.Wallet.Types.Read.Eras.EraValue

import Cardano.Wallet.Types.Read.Eras.InAnyCardanoEra
import Cardano.Wallet.Types.Read.Eras.KnownEras
import Generics.SOP
    ( (:.:) (..), K (..), unK )
import GHC.Generics
    ( (:*:) (..) )
