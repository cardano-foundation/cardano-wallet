{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Wallet.Types.Read.Tx.Certificates where

import Prelude

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, MaryEra, ShelleyEra )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Ledger.Shelley.TxBody
    ( DCert )
import Cardano.Wallet.Types.Read.Tx.Eras
    ( UseTxAllEra, UseTxEra (..) )
import Data.Sequence.Strict
    ( StrictSeq )
import Generics.SOP
    ( NP ((:*), Nil) )
import GHC.Records
    ( HasField (..) )

import qualified Cardano.Ledger.Alonzo.Tx as AL
import qualified Cardano.Ledger.Shelley.API as SH

type family CertsType era where
    CertsType ShelleyEra = StrictSeq (DCert StandardCrypto)
    CertsType AllegraEra = StrictSeq (DCert StandardCrypto)
    CertsType MaryEra = StrictSeq (DCert StandardCrypto)
    CertsType AlonzoEra = StrictSeq (DCert StandardCrypto)
    CertsType BabbageEra = StrictSeq (DCert StandardCrypto)
    CertsType _ = ()

newtype Certs era = Certs (CertsType era)

deriving instance Show (CertsType era) => Show (Certs era)
deriving instance Eq (CertsType era) => Eq (Certs era)

getEraCerts :: UseTxAllEra Certs
getEraCerts =
    UseTxEra (\_ -> Certs ())
    :* UseTxEra (\(SH.Tx b _ _) -> Certs $ getField @"certs" b)
    :* UseTxEra (\(SH.Tx b _ _) -> Certs $ getField @"certs" b)
    :* UseTxEra (\(SH.Tx b _ _) -> Certs $ getField @"certs" b)
    :* UseTxEra (\(AL.ValidatedTx b _ _ _) -> Certs $ getField @"certs" b)
    :* UseTxEra (\(AL.ValidatedTx b _ _ _) -> Certs $ getField @"certs" b)
    :* Nil
