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
-- Raw mint data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.Metadata
    ( MetadataType
    , Metadata (..)
    , getEraMetadata
    ) where

import Prelude

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, ConwayEra, MaryEra,
    ShelleyEra )
import Cardano.Ledger.Allegra.TxAuxData
    ( AllegraTxAuxData )
import Cardano.Ledger.Alonzo.TxAuxData
    ( AlonzoTxAuxData )
import Cardano.Ledger.Core
    ( auxDataTxL )
import Cardano.Ledger.Shelley.TxAuxData
    ( ShelleyTxAuxData )
import Cardano.Wallet.Read.Eras.EraFun
    ( EraFun (..) )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx )
import Control.Lens
    ( view )
import Data.Maybe.Strict
    ( StrictMaybe )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardAllegra, StandardAlonzo, StandardBabbage, StandardConway,
    StandardMary, StandardShelley )

type family MetadataType era where
  MetadataType ByronEra = ()
  MetadataType ShelleyEra = StrictMaybe (ShelleyTxAuxData StandardShelley)
  MetadataType AllegraEra = StrictMaybe (AllegraTxAuxData StandardAllegra)
  MetadataType MaryEra = StrictMaybe (AllegraTxAuxData StandardMary)
  MetadataType AlonzoEra = StrictMaybe (AlonzoTxAuxData StandardAlonzo)
  MetadataType BabbageEra = StrictMaybe (AlonzoTxAuxData StandardBabbage)
  MetadataType ConwayEra = StrictMaybe (AlonzoTxAuxData StandardConway)

newtype Metadata era = Metadata (MetadataType era)

deriving instance Show (MetadataType era) => Show (Metadata era)
deriving instance Eq (MetadataType era) => Eq (Metadata era)

getEraMetadata :: EraFun Tx Metadata
getEraMetadata =
    EraFun
        { byronFun = \_ -> Metadata ()
        , shelleyFun = metadata
        , allegraFun = metadata
        , maryFun = metadata
        , alonzoFun = metadata
        , babbageFun = metadata
        , conwayFun = metadata
        }
  where
    metadata = onTx $ Metadata . view auxDataTxL
