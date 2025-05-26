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
-- Raw certificate data extraction from 'Tx'
module Cardano.Read.Ledger.Tx.Certificates
    ( CertificatesType
    , Certificates (..)
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

import qualified Cardano.Ledger.Api as Ledger

type family CertificatesType era where
    CertificatesType Byron =
        ()
    CertificatesType Shelley =
        StrictSeq (ShelleyTxCert Ledger.ShelleyEra)
    CertificatesType Allegra =
        StrictSeq (ShelleyTxCert Ledger.AllegraEra)
    CertificatesType Mary =
        StrictSeq (ShelleyTxCert Ledger.MaryEra)
    CertificatesType Alonzo =
        StrictSeq (ShelleyTxCert Ledger.AlonzoEra)
    CertificatesType Babbage =
        StrictSeq (ShelleyTxCert Ledger.BabbageEra)
    CertificatesType Conway =
        StrictSeq (ConwayTxCert Ledger.ConwayEra)

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
