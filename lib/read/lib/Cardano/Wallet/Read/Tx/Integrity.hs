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
-- Raw script integrity data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.Integrity
    ( IntegrityType
    , Integrity (..)
    , getEraIntegrity
    )
    where

import Prelude

import Cardano.Ledger.Alonzo.Tx
    ( ScriptIntegrityHash
    )
import Cardano.Ledger.Alonzo.TxBody
    ( scriptIntegrityHashTxBodyL
    )
import Cardano.Ledger.Core
    ( bodyTxL
    )
import Cardano.Ledger.Crypto
    ( StandardCrypto
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
    ( (^.)
    )
import Data.Maybe.Strict
    ( StrictMaybe
    )

type family IntegrityType era where
    IntegrityType Byron = ()
    IntegrityType Shelley = ()
    IntegrityType Allegra = ()
    IntegrityType Mary = ()
    IntegrityType Alonzo = StrictMaybe (ScriptIntegrityHash StandardCrypto)
    IntegrityType Babbage = StrictMaybe (ScriptIntegrityHash StandardCrypto)
    IntegrityType Conway = StrictMaybe (ScriptIntegrityHash StandardCrypto)

newtype Integrity era = Integrity (IntegrityType era)

deriving instance Show (IntegrityType era) => Show (Integrity era)
deriving instance Eq (IntegrityType era) => Eq (Integrity era)

-- | Extract the script integrity data from a transaction in any available era.
getEraIntegrity :: EraFun Tx Integrity
getEraIntegrity =
    EraFun
        { byronFun = \_ -> Integrity ()
        , shelleyFun = \_ -> Integrity ()
        , allegraFun = \_ -> Integrity ()
        , maryFun = \_ -> Integrity ()
        , alonzoFun = alonzoIntegrity
        , babbageFun = alonzoIntegrity
        , conwayFun = alonzoIntegrity
        }
  where
    alonzoIntegrity = onTx $ \tx ->
        Integrity $
            tx ^. bodyTxL . scriptIntegrityHashTxBodyL