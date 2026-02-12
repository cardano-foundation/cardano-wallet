{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Prototocol parameters.
module Cardano.Read.Ledger.PParams
    ( -- * Protocol parameters type
      PParamsType
    , PParams (..)
    )
where

import Cardano.Read.Ledger.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Mary
    , Shelley
    )
import Prelude

import Cardano.Chain.Update qualified as BY
import Cardano.Ledger.Core qualified as SH

-- | Protocol parameters of different eras.
--
-- See https://cips.cardano.org/cip/CIP-9
-- for more information on the meaning of these parameters.
--
-- In the Shelley-based eras, this only includes protocol
-- parameters which can be changed by an update proposal.
-- This does not include some parameters such as the
-- \"length of a slot in seconds\"; the Shelley specification
-- calls such parameters \"Global Constants\".
type family PParamsType era where
    PParamsType Byron = BY.ProtocolParameters
    PParamsType Shelley = SH.PParams Shelley
    PParamsType Allegra = SH.PParams Allegra
    PParamsType Mary = SH.PParams Mary
    PParamsType Alonzo = SH.PParams Alonzo
    PParamsType Babbage = SH.PParams Babbage
    PParamsType Conway = SH.PParams Conway

-- | Era-indexed protocol parameters wrapper.
newtype PParams era = PParams (PParamsType era)

deriving instance Show (PParamsType era) => Show (PParams era)
deriving instance Eq (PParamsType era) => Eq (PParams era)
