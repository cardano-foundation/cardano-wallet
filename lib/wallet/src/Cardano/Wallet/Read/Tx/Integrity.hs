{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Raw script integrity data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.Integrity
    ( IntegrityType, Integrity (..), getEraIntegrity )
    where

import Prelude

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, ConwayEra, MaryEra, ShelleyEra )
import Cardano.Ledger.Alonzo.Tx
    ( ScriptIntegrityHash )
import Cardano.Ledger.Alonzo.TxBody
    ( scriptIntegrityHashTxBodyL )
import Cardano.Ledger.Core
    ( bodyTxL )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Wallet.Read.Eras
    ( EraFun (..) )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx )
import Control.Lens
    ( (^.) )
import Data.Maybe.Strict
    ( StrictMaybe )

type family IntegrityType era where
    IntegrityType ByronEra = ()
    IntegrityType ShelleyEra = ()
    IntegrityType AllegraEra = ()
    IntegrityType MaryEra = ()
    IntegrityType AlonzoEra = StrictMaybe (ScriptIntegrityHash StandardCrypto)
    IntegrityType BabbageEra = StrictMaybe (ScriptIntegrityHash StandardCrypto)
    IntegrityType ConwayEra = StrictMaybe (ScriptIntegrityHash StandardCrypto)

newtype Integrity era = Integrity (IntegrityType era)

deriving instance Show (IntegrityType era) => Show (Integrity era)
deriving instance Eq (IntegrityType era) => Eq (Integrity era)

getEraIntegrity :: EraFun Tx Integrity
getEraIntegrity
    = EraFun
        { byronFun = \_ -> Integrity ()
        , shelleyFun = \_ -> Integrity ()
        , allegraFun = \_ -> Integrity ()
        , maryFun = \_ -> Integrity ()
        , alonzoFun = onTx $ \tx -> Integrity
            $ tx ^. bodyTxL . scriptIntegrityHashTxBodyL
        , babbageFun = onTx $ \tx -> Integrity
            $ tx ^. bodyTxL . scriptIntegrityHashTxBodyL
        , conwayFun = onTx $ \tx -> Integrity
            $ tx ^. bodyTxL . scriptIntegrityHashTxBodyL
        }
