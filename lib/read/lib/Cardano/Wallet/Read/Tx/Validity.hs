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
    ( AllegraEra
    , AlonzoEra
    , BabbageEra
    , ByronEra
    , ConwayEra
    , MaryEra
    , ShelleyEra
    )
import Cardano.Ledger.Allegra.Scripts
    ( ValidityInterval
    )
import Cardano.Ledger.Allegra.TxBody
    ( vldtTxBodyL
    )
import Cardano.Ledger.Core
    ( bodyTxL
    )
import Cardano.Ledger.Shelley.TxBody
    ( ttlTxBodyL
    )
import Cardano.Ledger.Slot
    ( SlotNo
    )
import Cardano.Wallet.Read.Eras
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

-- | Extract validity data from tx for any available era.
getEraValidity :: EraFun Tx Validity
getEraValidity =
    EraFun
        { byronFun = \_ -> Validity ()
        , shelleyFun = anyValidity ttlTxBodyL
        , allegraFun = allegraValidity
        , maryFun = allegraValidity
        , alonzoFun = allegraValidity
        , babbageFun = allegraValidity
        , conwayFun = allegraValidity
        }
  where
    anyValidity l = onTx $ \tx -> Validity $ tx ^. bodyTxL . l
    allegraValidity = anyValidity vldtTxBodyL
