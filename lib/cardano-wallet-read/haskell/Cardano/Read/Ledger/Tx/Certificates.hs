{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{- |
Copyright: © 2020-2022 IOHK
License: Apache-2.0

Raw certificate data extraction from 'Tx'
-}
module Cardano.Read.Ledger.Tx.Certificates
    ( -- * Certificate type
      CertificatesType
    , Certificates (..)

      -- * Extraction
    , getEraCertificates
    )
where

import Prelude

import Cardano.Ledger.Api
    ( bodyTxL
    , certsTxBodyL
    )
import Cardano.Ledger.Conway.TxCert
    ( ConwayTxCert
    )
import Cardano.Ledger.Shelley.TxCert
    ( ShelleyTxCert
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
import Data.Sequence.Strict
    ( StrictSeq
    )

-- |
-- Era-specific certificate type.
--
-- Byron returns unit @()@ as staking is not supported.
-- Shelley through Babbage use 'ShelleyTxCert' for staking operations.
-- Conway uses 'ConwayTxCert' which adds governance certificates.
type family CertificatesType era where
    CertificatesType Byron =
        ()
    CertificatesType Shelley =
        StrictSeq (ShelleyTxCert Shelley)
    CertificatesType Allegra =
        StrictSeq (ShelleyTxCert Allegra)
    CertificatesType Mary =
        StrictSeq (ShelleyTxCert Mary)
    CertificatesType Alonzo =
        StrictSeq (ShelleyTxCert Alonzo)
    CertificatesType Babbage =
        StrictSeq (ShelleyTxCert Babbage)
    CertificatesType Conway =
        StrictSeq (ConwayTxCert Conway)

-- | Era-indexed stake certificates wrapper.
newtype Certificates era = Certificates (CertificatesType era)

deriving instance
    Show (CertificatesType era) => Show (Certificates era)
deriving instance Eq (CertificatesType era) => Eq (Certificates era)

{-# INLINEABLE getEraCertificates #-}

-- | Extract certificates from a 'Tx' in any era.
getEraCertificates
    :: forall era. IsEra era => Tx era -> Certificates era
getEraCertificates = case theEra @era of
    Byron -> const $ Certificates ()
    Shelley -> certificates
    Allegra -> certificates
    Mary -> certificates
    Alonzo -> certificates
    Babbage -> certificates
    Conway -> certificates
  where
    certificates = onTx $ Certificates . view (bodyTxL . certsTxBodyL)
