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
-- Raw extra signers required data extraction from 'Tx'
--

module Cardano.Read.Ledger.Tx.ExtraSigs where

import Prelude

import Cardano.Ledger.Alonzo.TxBody
    ( reqSignerHashesTxBodyL
    )
import Cardano.Ledger.Api
    ( StandardCrypto
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

type family ExtraSigsType era where
    ExtraSigsType Byron = ()
    ExtraSigsType Shelley = ()
    ExtraSigsType Allegra = ()
    ExtraSigsType Mary = ()
    ExtraSigsType Alonzo = Set (KeyHash 'Witness StandardCrypto)
    ExtraSigsType Babbage = Set (KeyHash 'Witness StandardCrypto)
    ExtraSigsType Conway = Set (KeyHash 'Witness StandardCrypto)

newtype ExtraSigs era = ExtraSigs (ExtraSigsType era)

deriving instance Show (ExtraSigsType era) => Show (ExtraSigs era)
deriving instance Eq (ExtraSigsType era) => Eq (ExtraSigs era)

{-# INLINABLE getEraExtraSigs #-}
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
