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

{-# INLINABLE getEraIntegrity #-}
-- | Extract the script integrity data from a transaction in any available era.
getEraIntegrity :: forall era. IsEra era => Tx era -> Integrity era
getEraIntegrity = case theEra @era of
    Byron -> \_ -> Integrity ()
    Shelley -> \_ -> Integrity ()
    Allegra -> \_ -> Integrity ()
    Mary -> \_ -> Integrity ()
    Alonzo -> alonzoIntegrity
    Babbage -> alonzoIntegrity
    Conway -> alonzoIntegrity
  where
    alonzoIntegrity = onTx $ \tx ->
        Integrity
            $ tx ^. bodyTxL . scriptIntegrityHashTxBodyL
