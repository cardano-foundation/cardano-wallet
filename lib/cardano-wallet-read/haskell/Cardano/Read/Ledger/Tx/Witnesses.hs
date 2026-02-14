{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Raw witnesses data extraction from 'Tx'
module Cardano.Read.Ledger.Tx.Witnesses
    ( -- * Witness type
      WitnessesType
    , Witnesses (..)

      -- * Extraction
    , getEraWitnesses
    ) where

import Cardano.Ledger.Alonzo.TxWits
    ( AlonzoTxWits
    )
import Cardano.Ledger.Core
    ( witsTxL
    )
import Cardano.Ledger.Shelley.TxWits
    ( ShelleyTxWits
    )
import Cardano.Read.Ledger.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Dijkstra
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
-- Era-specific witness type.
--
-- Witness types differ across eras:
--
-- * Byron: Unit @()@ (witnesses are inline in the transaction)
-- * Shelley through Mary: 'ShelleyTxWits' (key and script witnesses)
-- * Alonzo and later: 'AlonzoTxWits' (adds Plutus scripts and redeemers)
type family WitnessesType era where
    WitnessesType Byron = ()
    WitnessesType Shelley = ShelleyTxWits Shelley
    WitnessesType Allegra = ShelleyTxWits Allegra
    WitnessesType Mary = ShelleyTxWits Mary
    WitnessesType Alonzo = AlonzoTxWits Alonzo
    WitnessesType Babbage = AlonzoTxWits Babbage
    WitnessesType Conway = AlonzoTxWits Conway
    WitnessesType Dijkstra = AlonzoTxWits Dijkstra

-- | Era-indexed transaction witnesses wrapper.
newtype Witnesses era = Witnesses (WitnessesType era)

deriving instance Show (WitnessesType era) => Show (Witnesses era)
deriving instance Eq (WitnessesType era) => Eq (Witnesses era)

{-# INLINEABLE getEraWitnesses #-}

-- | Extract the transaction witnesses from a transaction in any era.
getEraWitnesses :: forall era. IsEra era => Tx era -> Witnesses era
getEraWitnesses = case theEra @era of
    Byron -> const $ Witnesses ()
    Shelley -> witnesses
    Allegra -> witnesses
    Mary -> witnesses
    Alonzo -> witnesses
    Babbage -> witnesses
    Conway -> witnesses
    Dijkstra -> witnesses
  where
    witnesses = onTx $ \tx -> Witnesses (tx ^. witsTxL)
