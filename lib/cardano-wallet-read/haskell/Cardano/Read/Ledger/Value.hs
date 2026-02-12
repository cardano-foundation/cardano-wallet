{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Era-indexed value.
module Cardano.Read.Ledger.Value
    ( -- * Value type
      ValueType
    , Value (..)

      -- * Conversions
    , maryValueFromByronValue
    , maryValueFromShelleyValue
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

import Cardano.Chain.Common qualified as BY
import Cardano.Ledger.BaseTypes qualified as SH
import Cardano.Ledger.Coin qualified as SH
import Cardano.Ledger.Mary.Value qualified as MA

{-----------------------------------------------------------------------------
    Value
------------------------------------------------------------------------------}

-- |
-- Era-specific value type.
--
-- * Byron: 'Lovelace' (ADA only)
-- * Shelley\/Allegra: 'Coin' (ADA only)
-- * Mary and later: 'MaryValue' (ADA + native tokens)
type family ValueType era where
    ValueType Byron = BY.Lovelace
    ValueType Shelley = SH.Coin
    ValueType Allegra = SH.Coin
    ValueType Mary = MA.MaryValue
    ValueType Alonzo = MA.MaryValue
    ValueType Babbage = MA.MaryValue
    ValueType Conway = MA.MaryValue

-- | Era-indexed value wrapper.
newtype Value era = Value (ValueType era)

deriving instance Show (ValueType era) => Show (Value era)
deriving instance Eq (ValueType era) => Eq (Value era)

-- | Convert a Byron value (lovelace) to a Mary multi-asset value.
maryValueFromByronValue :: ValueType Byron -> ValueType Mary
maryValueFromByronValue = SH.inject . SH.Coin . BY.lovelaceToInteger

-- | Convert a Shelley value (coin) to a Mary multi-asset value.
maryValueFromShelleyValue :: ValueType Shelley -> ValueType Mary
maryValueFromShelleyValue = SH.inject
