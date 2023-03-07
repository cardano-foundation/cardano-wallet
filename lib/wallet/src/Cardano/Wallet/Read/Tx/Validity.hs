{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Raw validity interval data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.Validity
    ( getEraValidity
    , ValidityType
    , Validity (..)
    )
    where

import Prelude

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, ConwayEra, MaryEra, ShelleyEra )
import Cardano.Ledger.Core
    ( bodyTxL )
import Cardano.Ledger.Shelley.TxBody
    ( ttlTxBodyL )
import Cardano.Ledger.ShelleyMA.Timelocks
    ( ValidityInterval )
import Cardano.Ledger.ShelleyMA.TxBody
    ( vldtTxBodyL )
import Cardano.Ledger.Slot
    ( SlotNo )
import Cardano.Wallet.Read.Eras
    ( EraFun (..) )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx )
import Control.Lens
    ( (^.) )

type family ValidityType era where
    ValidityType ByronEra = ()
    ValidityType ShelleyEra = SlotNo
    ValidityType AllegraEra = ValidityInterval
    ValidityType MaryEra = ValidityInterval
    ValidityType AlonzoEra = ValidityInterval
    ValidityType BabbageEra = ValidityInterval
    ValidityType ConwayEra = ValidityInterval

newtype Validity era = Validity (ValidityType era)

deriving instance Show (ValidityType era) => Show (Validity era)
deriving instance Eq (ValidityType era) => Eq (Validity era)

getEraValidity :: EraFun Tx Validity
getEraValidity
    = EraFun
        { byronFun = \_ -> Validity ()
        , shelleyFun = onTx $ \tx -> Validity $ tx ^. bodyTxL . ttlTxBodyL
        , allegraFun = onTx $ \tx -> Validity $ tx ^. bodyTxL . vldtTxBodyL
        , maryFun = onTx $ \tx -> Validity $ tx ^. bodyTxL . vldtTxBodyL
        , alonzoFun = onTx $ \tx -> Validity $ tx ^. bodyTxL . vldtTxBodyL
        , babbageFun = onTx $ \tx -> Validity $ tx ^. bodyTxL . vldtTxBodyL
        , conwayFun = onTx $ \tx -> Validity $ tx ^. bodyTxL . vldtTxBodyL
        }
