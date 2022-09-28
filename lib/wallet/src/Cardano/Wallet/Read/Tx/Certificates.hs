{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
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
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Ledger.Shelley.TxBody
    ( DCert )
import Cardano.Wallet.Read.Eras
    ( EraFun (..) )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx )
import Data.Sequence.Strict
    ( StrictSeq )
import GHC.Records
    ( HasField (..) )

import qualified Cardano.Ledger.Alonzo.Tx as AL
import qualified Cardano.Ledger.Shelley.API as SH

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
        , shelleyFun = onTx $ \((SH.Tx b _ _)) -> getCertificates b
        , allegraFun = onTx $ \((SH.Tx b _ _)) -> getCertificates b
        , maryFun = onTx $ \(SH.Tx b _ _) -> getCertificates b
        , alonzoFun = onTx $ \(AL.ValidatedTx b _ _ _) -> getCertificates b
        , babbageFun = onTx $ \(AL.ValidatedTx b _ _ _) -> getCertificates b
        }

getCertificates
    :: HasField "certs" a (CertificatesType b)
    => a -> Certificates b
getCertificates =  Certificates . getField @"certs"
