{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

Era-indexed value.
-}
module Cardano.Read.Ledger.Value
    ( ValueType
    , Value (..)
    , maryValueFromByronValue
    , maryValueFromShelleyValue
    )
    where

import Prelude

import Cardano.Ledger.Crypto
    ( StandardCrypto
    )
import Cardano.Wallet.Read.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Mary
    , Shelley
    )

import qualified Cardano.Chain.Common as BY
import qualified Cardano.Ledger.BaseTypes as SH
import qualified Cardano.Ledger.Coin as SH
import qualified Cardano.Ledger.Mary.Value as MA

{-----------------------------------------------------------------------------
    Value
------------------------------------------------------------------------------}

type family ValueType era where
    ValueType Byron = BY.Lovelace
    ValueType Shelley = SH.Coin
    ValueType Allegra = SH.Coin
    ValueType Mary = MA.MaryValue StandardCrypto
    ValueType Alonzo = MA.MaryValue StandardCrypto
    ValueType Babbage = MA.MaryValue StandardCrypto
    ValueType Conway = MA.MaryValue StandardCrypto

newtype Value era = Value (ValueType era)

deriving instance Show (ValueType era) => Show (Value era)
deriving instance Eq (ValueType era) => Eq (Value era)

maryValueFromByronValue :: ValueType Byron -> ValueType Mary
maryValueFromByronValue = SH.inject . SH.Coin . BY.lovelaceToInteger

maryValueFromShelleyValue :: ValueType Shelley -> ValueType Mary
maryValueFromShelleyValue = SH.inject
