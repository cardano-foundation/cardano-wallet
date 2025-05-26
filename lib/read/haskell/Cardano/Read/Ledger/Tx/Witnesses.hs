{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Raw witnesses data extraction from 'Tx'
--

module Cardano.Read.Ledger.Tx.Witnesses
    ( WitnessesType
    , Witnesses (..)
    , getEraWitnesses
    ) where

import Prelude

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

type family WitnessesType era where
  WitnessesType Byron = ()
  WitnessesType Shelley = ShelleyTxWits Shelley
  WitnessesType Allegra = ShelleyTxWits Allegra
  WitnessesType Mary = ShelleyTxWits Mary
  WitnessesType Alonzo = AlonzoTxWits Alonzo
  WitnessesType Babbage = AlonzoTxWits Babbage
  WitnessesType Conway = AlonzoTxWits Conway

newtype Witnesses era = Witnesses (WitnessesType era)

deriving instance Show (WitnessesType era) => Show (Witnesses era)
deriving instance Eq (WitnessesType era) => Eq (Witnesses era)

{-# INLINABLE getEraWitnesses #-}
getEraWitnesses :: forall era .  IsEra era => Tx era ->  Witnesses era
getEraWitnesses = case theEra @era of
        Byron -> const $ Witnesses ()
        Shelley -> witnesses
        Allegra -> witnesses
        Mary -> witnesses
        Alonzo -> witnesses
        Babbage -> witnesses
        Conway -> witnesses
  where
    witnesses = onTx $ Witnesses . view witsTxL
