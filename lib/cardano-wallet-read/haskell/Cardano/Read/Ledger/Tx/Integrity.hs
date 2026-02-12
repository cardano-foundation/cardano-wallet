{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Raw script integrity data extraction from 'Tx'
module Cardano.Read.Ledger.Tx.Integrity
    ( -- * Integrity type
      IntegrityType
    , Integrity (..)

      -- * Extraction
    , getEraIntegrity
    )
where

import Cardano.Ledger.Alonzo.Tx
    ( ScriptIntegrityHash
    )
import Cardano.Ledger.Alonzo.TxBody
    ( scriptIntegrityHashTxBodyL
    )
import Cardano.Ledger.Core
    ( bodyTxL
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
    ( (^.)
    )
import Data.Maybe.Strict
    ( StrictMaybe
    )
import Prelude

-- |
-- Era-specific script integrity hash type.
--
-- Pre-Alonzo eras return unit @()@ as Plutus scripts are not supported.
-- Alonzo and later return an optional hash committing to redeemers,
-- datums, and cost models.
type family IntegrityType era where
    IntegrityType Byron = ()
    IntegrityType Shelley = ()
    IntegrityType Allegra = ()
    IntegrityType Mary = ()
    IntegrityType Alonzo =
        StrictMaybe ScriptIntegrityHash
    IntegrityType Babbage =
        StrictMaybe ScriptIntegrityHash
    IntegrityType Conway =
        StrictMaybe ScriptIntegrityHash

-- | Era-indexed script integrity hash wrapper.
newtype Integrity era = Integrity (IntegrityType era)

deriving instance Show (IntegrityType era) => Show (Integrity era)
deriving instance Eq (IntegrityType era) => Eq (Integrity era)

{-# INLINEABLE getEraIntegrity #-}

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
