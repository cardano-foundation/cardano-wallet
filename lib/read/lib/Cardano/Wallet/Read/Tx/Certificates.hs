{-# LANGUAGE DataKinds #-}
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
-- Raw certificate data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.Certificates
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
import Cardano.Ledger.Crypto
    ( StandardCrypto
    )
import Cardano.Ledger.Shelley.TxCert
    ( ShelleyTxCert
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
import Data.Sequence.Strict
    ( StrictSeq
    )

import qualified Cardano.Ledger.Api as Ledger

type family CertificatesType era where
    CertificatesType Byron =
        ()
    CertificatesType Shelley =
        StrictSeq (ShelleyTxCert (Ledger.ShelleyEra StandardCrypto))
    CertificatesType Allegra =
        StrictSeq (ShelleyTxCert (Ledger.AllegraEra StandardCrypto))
    CertificatesType Mary =
        StrictSeq (ShelleyTxCert (Ledger.MaryEra StandardCrypto))
    CertificatesType Alonzo =
        StrictSeq (ShelleyTxCert (Ledger.AlonzoEra StandardCrypto))
    CertificatesType Babbage =
        StrictSeq (ShelleyTxCert (Ledger.BabbageEra StandardCrypto))
    CertificatesType Conway =
        StrictSeq (ConwayTxCert (Ledger.ConwayEra StandardCrypto))

newtype Certificates era = Certificates (CertificatesType era)

deriving instance Show (CertificatesType era) => Show (Certificates era)
deriving instance Eq (CertificatesType era) => Eq (Certificates era)

-- | Extract certificates from a 'Tx' in any era.
getEraCertificates :: EraFun Tx Certificates
getEraCertificates =
    EraFun
        { byronFun = \_ -> Certificates ()
        , shelleyFun = certificates
        , allegraFun = certificates
        , maryFun = certificates
        , alonzoFun = certificates
        , babbageFun = certificates
        , conwayFun = onTx $ Certificates . view (bodyTxL . certsTxBodyL)
        }
  where
    certificates = onTx $ Certificates . view (bodyTxL . certsTxBodyL)
