{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Raw validity interval data extraction from 'Tx'
module Cardano.Read.Ledger.Tx.Validity
    ( -- * Validity type
      ValidityType
    , Validity (..)

      -- * Extraction
    , getEraValidity
    )
where

import Cardano.Ledger.Allegra.Scripts
    ( ValidityInterval
    )
import Cardano.Ledger.Allegra.TxBody
    ( vldtTxBodyL
    )
import Cardano.Ledger.Core
    ( bodyTxL
    )
import Cardano.Ledger.Shelley.TxBody
    ( ttlTxBodyL
    )
import Cardano.Ledger.Slot
    ( SlotNo
    )
import Cardano.Read.Ledger.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Era (..)
    , IsEra (..)
    , Mary
    , Shelley
    )
import Cardano.Read.Ledger.Tx.Eras
    ( onTx
    )
import Cardano.Read.Ledger.Tx.Tx
    ( Tx (..)
    )
import Control.Lens
    ( (^.)
    )
import Prelude

-- |
-- Era-specific validity type.
--
-- Validity constraints differ across eras:
--
-- * Byron: Unit @()@ (no explicit validity)
-- * Shelley: 'SlotNo' (time-to-live, transaction invalid after this slot)
-- * Allegra and later: 'ValidityInterval' (valid between two slots)
type family ValidityType era where
    ValidityType Byron = ()
    ValidityType Shelley = SlotNo
    ValidityType Allegra = ValidityInterval
    ValidityType Mary = ValidityInterval
    ValidityType Alonzo = ValidityInterval
    ValidityType Babbage = ValidityInterval
    ValidityType Conway = ValidityInterval

-- | Era-indexed transaction validity wrapper.
newtype Validity era = Validity (ValidityType era)

deriving instance Show (ValidityType era) => Show (Validity era)
deriving instance Eq (ValidityType era) => Eq (Validity era)

{-# INLINEABLE getEraValidity #-}

-- | Extract validity data from tx for any available era.
getEraValidity :: forall era. IsEra era => Tx era -> Validity era
getEraValidity = case theEra @era of
    Byron -> \_ -> Validity ()
    Shelley -> anyValidity ttlTxBodyL
    Allegra -> allegraValidity
    Mary -> allegraValidity
    Alonzo -> allegraValidity
    Babbage -> allegraValidity
    Conway -> allegraValidity
  where
    anyValidity l = onTx $ \tx -> Validity $ tx ^. bodyTxL . l
    allegraValidity = anyValidity vldtTxBodyL
