{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{- |
Copyright: © 2020-2022 IOHK
License: Apache-2.0

Raw era-dependent tx outputs data extraction from 'Tx'
-}
module Cardano.Read.Ledger.Tx.Outputs
    ( -- * Output types
      OutputsType
    , Outputs (..)

      -- * Extraction
    , getEraOutputs
    )
where

import Prelude

import Cardano.Chain.UTxO qualified as BY
import Cardano.Ledger.Alonzo.TxOut
    ( AlonzoTxOut
    )
import Cardano.Ledger.Babbage.TxOut
    ( BabbageTxOut
    )
import Cardano.Ledger.Core
    ( bodyTxL
    , outputsTxBodyL
    )
import Cardano.Ledger.Shelley.TxOut
    ( ShelleyTxOut
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
    ( view
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Sequence.Strict
    ( StrictSeq
    )

-- |
-- Era-specific output collection type.
--
-- The output type evolves across eras to support new features:
--
-- * Byron\/Shelley\/Allegra: Simple address + coin
-- * Mary: Multi-asset values
-- * Alonzo: Datum hashes
-- * Babbage\/Conway: Inline datums and reference scripts
type family OutputsType era where
    OutputsType Byron = NonEmpty BY.TxOut
    OutputsType Shelley = StrictSeq (ShelleyTxOut Shelley)
    OutputsType Allegra = StrictSeq (ShelleyTxOut Allegra)
    OutputsType Mary = StrictSeq (ShelleyTxOut Mary)
    OutputsType Alonzo = StrictSeq (AlonzoTxOut Alonzo)
    OutputsType Babbage = StrictSeq (BabbageTxOut Babbage)
    OutputsType Conway = StrictSeq (BabbageTxOut Conway)

-- | Era-indexed transaction outputs wrapper.
newtype Outputs era = Outputs (OutputsType era)

deriving instance Show (OutputsType era) => Show (Outputs era)
deriving instance Eq (OutputsType era) => Eq (Outputs era)

{-# INLINE getEraOutputs #-}

-- | Extract the transaction outputs from a transaction in any era.
getEraOutputs :: forall era. IsEra era => Tx era -> Outputs era
getEraOutputs = case theEra :: Era era of
    Byron -> onTx $ Outputs . BY.txOutputs . BY.taTx
    Shelley -> outputs
    Allegra -> outputs
    Mary -> outputs
    Alonzo -> outputs
    Babbage -> outputs
    Conway -> outputs
  where
    outputs = onTx $ Outputs . view (bodyTxL . outputsTxBodyL)
