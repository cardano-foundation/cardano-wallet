{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Raw era-dependent tx outputs data extraction from 'Tx'
--

module Cardano.Read.Ledger.Tx.Outputs
    ( OutputsType
    , Outputs (..)
    , getEraOutputs
    )
    where

import Prelude

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
import Cardano.Read.Ledger.Tx.Eras
    ( onTx
    )
import Cardano.Wallet.Read.Eras
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
import Cardano.Wallet.Read.Tx
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

import qualified Cardano.Chain.UTxO as BY

type family OutputsType era where
    OutputsType Byron = NonEmpty BY.TxOut
    OutputsType Shelley = StrictSeq (ShelleyTxOut Shelley)
    OutputsType Allegra = StrictSeq (ShelleyTxOut Allegra)
    OutputsType Mary = StrictSeq (ShelleyTxOut Mary)
    OutputsType Alonzo = StrictSeq (AlonzoTxOut Alonzo)
    OutputsType Babbage = StrictSeq (BabbageTxOut Babbage)
    OutputsType Conway = StrictSeq (BabbageTxOut Conway)

newtype Outputs era = Outputs (OutputsType era)

deriving instance Show (OutputsType era) => Show (Outputs era)
deriving instance Eq (OutputsType era) => Eq (Outputs era)

{-# INLINE getEraOutputs #-}
getEraOutputs :: forall era . IsEra era => Tx era -> Outputs era
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
