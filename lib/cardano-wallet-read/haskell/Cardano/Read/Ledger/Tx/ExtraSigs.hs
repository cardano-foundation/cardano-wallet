{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{- |
Copyright: © 2020-2022 IOHK
License: Apache-2.0

Raw extra signers required data extraction from 'Tx'
-}
module Cardano.Read.Ledger.Tx.ExtraSigs
    ( ExtraSigsType
    , ExtraSigs (..)
    , getEraExtraSigs
    ) where

import Prelude

import Cardano.Ledger.Alonzo.TxBody
    ( reqSignerHashesTxBodyL
    )

import Cardano.Ledger.Core
    ( bodyTxL
    )
import Cardano.Ledger.Keys
    ( KeyHash
    , KeyRole (..)
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
import Data.Set
    ( Set
    )

-- |
-- Era-specific extra signers type.
--
-- Pre-Alonzo eras return unit @()@ as extra signers are not supported.
-- Alonzo and later return a set of key hashes that must sign the transaction.
type family ExtraSigsType era where
    ExtraSigsType Byron = ()
    ExtraSigsType Shelley = ()
    ExtraSigsType Allegra = ()
    ExtraSigsType Mary = ()
    ExtraSigsType Alonzo = Set (KeyHash 'Witness)
    ExtraSigsType Babbage = Set (KeyHash 'Witness)
    ExtraSigsType Conway = Set (KeyHash 'Witness)

-- | Era-indexed required extra signers wrapper.
newtype ExtraSigs era = ExtraSigs (ExtraSigsType era)

deriving instance Show (ExtraSigsType era) => Show (ExtraSigs era)
deriving instance Eq (ExtraSigsType era) => Eq (ExtraSigs era)

{-# INLINEABLE getEraExtraSigs #-}

-- | Get extra signatures required for a transaction in any era.
getEraExtraSigs :: forall era. IsEra era => Tx era -> ExtraSigs era
getEraExtraSigs = case theEra @era of
    Byron -> const $ ExtraSigs ()
    Shelley -> const $ ExtraSigs ()
    Allegra -> const $ ExtraSigs ()
    Mary -> const $ ExtraSigs ()
    Alonzo -> mkExtraSignatures
    Babbage -> mkExtraSignatures
    Conway -> mkExtraSignatures
  where
    mkExtraSignatures = onTx $ \tx ->
        ExtraSigs
            $ tx ^. bodyTxL . reqSignerHashesTxBodyL
