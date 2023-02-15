{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, MaryEra, ShelleyEra )
import Cardano.Ledger.Core
    ( bodyTxL )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Ledger.Shelley.TxBody
    ( DCert, certsTxBodyL )
import Cardano.Wallet.Read.Eras
    ( EraFun (..) )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx )
import Control.Lens
    ( (^.) )
import Data.Sequence.Strict
    ( StrictSeq )

type family CertificatesType era where
    CertificatesType ByronEra = ()
    CertificatesType ShelleyEra = StrictSeq (DCert StandardCrypto)
    CertificatesType AllegraEra = StrictSeq (DCert StandardCrypto)
    CertificatesType MaryEra = StrictSeq (DCert StandardCrypto)
    CertificatesType AlonzoEra = StrictSeq (DCert StandardCrypto)
    CertificatesType BabbageEra = StrictSeq (DCert StandardCrypto)

newtype Certificates era = Certificates (CertificatesType era)

deriving instance Show (CertificatesType era) => Show (Certificates era)
deriving instance Eq (CertificatesType era) => Eq (Certificates era)

getEraCertificates :: EraFun Tx Certificates
getEraCertificates
    = EraFun
        { byronFun =  \_ -> Certificates ()
        , shelleyFun = onTx $ \tx -> Certificates $ tx ^. bodyTxL . certsTxBodyL
        , allegraFun = onTx $ \tx -> Certificates $ tx ^. bodyTxL . certsTxBodyL
        , maryFun = onTx $ \tx -> Certificates $ tx ^. bodyTxL . certsTxBodyL
        , alonzoFun = onTx $ \tx -> Certificates $ tx ^. bodyTxL . certsTxBodyL
        , babbageFun = onTx $ \tx -> Certificates $ tx ^. bodyTxL . certsTxBodyL
        }
