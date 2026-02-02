{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{- |
Copyright: © 2020-2022 IOHK
License: Apache-2.0

Raw mint data extraction from 'Tx'
-}
module Cardano.Read.Ledger.Tx.Metadata
    ( -- * Metadata type
      MetadataType
    , Metadata (..)

      -- * Extraction
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
import Data.Maybe.Strict
    ( StrictMaybe
    )

-- |
-- Era-specific metadata (auxiliary data) type.
--
-- Byron returns unit @()@ as metadata is not supported.
-- Later eras return optional auxiliary data structures.
type family MetadataType era where
    MetadataType Byron = ()
    MetadataType Shelley = StrictMaybe (ShelleyTxAuxData Shelley)
    MetadataType Allegra = StrictMaybe (AllegraTxAuxData Allegra)
    MetadataType Mary = StrictMaybe (AllegraTxAuxData Mary)
    MetadataType Alonzo = StrictMaybe (AlonzoTxAuxData Alonzo)
    MetadataType Babbage = StrictMaybe (AlonzoTxAuxData Babbage)
    MetadataType Conway = StrictMaybe (AlonzoTxAuxData Conway)

-- | Era-indexed transaction metadata wrapper.
newtype Metadata era = Metadata (MetadataType era)

deriving instance Show (MetadataType era) => Show (Metadata era)
deriving instance Eq (MetadataType era) => Eq (Metadata era)

{-# INLINEABLE getEraMetadata #-}

-- | Extract the metadata from a transaction in any era.
getEraMetadata :: forall era. IsEra era => Tx era -> Metadata era
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
