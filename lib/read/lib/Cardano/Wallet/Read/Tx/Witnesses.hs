{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Raw witnesses data extraction from 'Tx'
module Cardano.Wallet.Read.Tx.Witnesses
  ( WitnessesType
  , Witnesses (..)
  , getEraWitnesses
  )
where

import Cardano.Api
  ( AllegraEra
  , AlonzoEra
  , BabbageEra
  , ByronEra
  , ConwayEra
  , MaryEra
  , ShelleyEra
  )
import Cardano.Ledger.Alonzo.TxWits
  ( AlonzoTxWits
  )
import Cardano.Ledger.Core
  ( witsTxL
  )
import Cardano.Ledger.Shelley.TxWits
  ( ShelleyTxWits
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
import Ouroboros.Consensus.Shelley.Eras
  ( StandardAllegra
  , StandardAlonzo
  , StandardBabbage
  , StandardConway
  , StandardMary
  , StandardShelley
  )
import Prelude

type family WitnessesType era where
  WitnessesType ByronEra = ()
  WitnessesType ShelleyEra = ShelleyTxWits StandardShelley
  WitnessesType AllegraEra = ShelleyTxWits StandardAllegra
  WitnessesType MaryEra = ShelleyTxWits StandardMary
  WitnessesType AlonzoEra = AlonzoTxWits StandardAlonzo
  WitnessesType BabbageEra = AlonzoTxWits StandardBabbage
  WitnessesType ConwayEra = AlonzoTxWits StandardConway

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
