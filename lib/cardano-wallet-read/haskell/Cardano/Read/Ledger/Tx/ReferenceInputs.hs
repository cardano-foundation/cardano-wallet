{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Reference input data extraction from 'Tx'
module Cardano.Read.Ledger.Tx.ReferenceInputs
    ( -- * Reference input type
      ReferenceInputsType
    , ReferenceInputs (..)

      -- * Extraction
    , getEraReferenceInputs
    )
where

import Cardano.Ledger.Babbage.TxBody
    ( referenceInputsTxBodyL
    )
import Cardano.Ledger.Core
    ( bodyTxL
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
import Data.Set
    ( Set
    )
import Prelude

import Cardano.Ledger.Shelley.API qualified as SH

-- |
-- Era-specific reference inputs type.
--
-- Pre-Babbage eras return unit @()@ as reference inputs are not supported.
-- Babbage and later return a set of inputs read without being consumed.
type family ReferenceInputsType era where
    ReferenceInputsType Byron = ()
    ReferenceInputsType Shelley = ()
    ReferenceInputsType Allegra = ()
    ReferenceInputsType Mary = ()
    ReferenceInputsType Alonzo = ()
    ReferenceInputsType Babbage = Set SH.TxIn
    ReferenceInputsType Conway = Set SH.TxIn

-- | Era-indexed reference inputs wrapper.
newtype ReferenceInputs era = ReferenceInputs (ReferenceInputsType era)

deriving instance
    Show (ReferenceInputsType era) => Show (ReferenceInputs era)
deriving instance
    Eq (ReferenceInputsType era) => Eq (ReferenceInputs era)

{-# INLINEABLE getEraReferenceInputs #-}

-- | Extract the reference inputs from a transaction in any era.
getEraReferenceInputs
    :: forall era. IsEra era => Tx era -> ReferenceInputs era
getEraReferenceInputs = case theEra @era of
    Byron -> \_ -> ReferenceInputs ()
    Shelley -> \_ -> ReferenceInputs ()
    Allegra -> \_ -> ReferenceInputs ()
    Mary -> \_ -> ReferenceInputs ()
    Alonzo -> \_ -> ReferenceInputs ()
    Babbage -> referenceInputsBabbage
    Conway -> referenceInputsBabbage
  where
    referenceInputsBabbage = onTx $ \tx ->
        ReferenceInputs $ tx ^. bodyTxL . referenceInputsTxBodyL
