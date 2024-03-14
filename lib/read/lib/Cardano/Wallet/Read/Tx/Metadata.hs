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
-- Raw mint data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.Metadata
    ( MetadataType
    , Metadata (..)
    , getEraMetadata
    ) where

import Prelude

import Cardano.Ledger.Allegra.TxAuxData
    ( AllegraTxAuxData
    )
import Cardano.Ledger.Alonzo.TxAuxData
    ( AlonzoTxAuxData
    )
import Cardano.Ledger.Core
    ( auxDataTxL
    )
import Cardano.Ledger.Shelley.TxAuxData
    ( ShelleyTxAuxData
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
import Cardano.Wallet.Read.Tx.Eras
    ( onTx
    )
import Control.Lens
    ( view
    )
import Data.Maybe.Strict
    ( StrictMaybe
    )

type family MetadataType era where
  MetadataType Byron = ()
  MetadataType Shelley = StrictMaybe (ShelleyTxAuxData Shelley)
  MetadataType Allegra = StrictMaybe (AllegraTxAuxData Allegra)
  MetadataType Mary = StrictMaybe (AllegraTxAuxData Mary)
  MetadataType Alonzo = StrictMaybe (AlonzoTxAuxData Alonzo)
  MetadataType Babbage = StrictMaybe (AlonzoTxAuxData Babbage)
  MetadataType Conway = StrictMaybe (AlonzoTxAuxData Conway)

newtype Metadata era = Metadata (MetadataType era)

deriving instance Show (MetadataType era) => Show (Metadata era)
deriving instance Eq (MetadataType era) => Eq (Metadata era)

{-# INLINABLE getEraMetadata #-}
getEraMetadata :: forall era . IsEra era => Tx era -> Metadata era
getEraMetadata = case theEra @era of
    Byron -> \_ -> Metadata ()
    Shelley -> metadata
    Allegra -> metadata
    Mary -> metadata
    Alonzo -> metadata
    Babbage -> metadata
    Conway -> metadata

  where
    metadata = onTx $ Metadata . view auxDataTxL
