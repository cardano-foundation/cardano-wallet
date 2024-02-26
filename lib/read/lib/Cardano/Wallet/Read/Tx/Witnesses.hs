{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Raw witnesses data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.Witnesses
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
import Cardano.Wallet.Read.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Mary
    , Shelley
    )
import Cardano.Wallet.Read.Eras.EraFun
    ( EraFun (..)
    )
import Cardano.Wallet.Read.Tx
    ( Tx (..)
    )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx
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

getEraWitnesses :: EraFun Tx Witnesses
getEraWitnesses =
    EraFun
        { byronFun = \_ -> Witnesses ()
        , shelleyFun = witnesses
        , allegraFun = witnesses
        , maryFun = witnesses
        , alonzoFun = witnesses
        , babbageFun = witnesses
        , conwayFun = witnesses
        }
  where
    witnesses = onTx $ Witnesses . view witsTxL
