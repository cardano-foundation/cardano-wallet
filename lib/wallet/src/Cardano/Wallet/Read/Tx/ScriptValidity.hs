{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Raw mint data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.ScriptValidity
    ( ScriptValidityType
    , ScriptValidity (..)
    , getEraScriptValidity
    ) where

import Prelude

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, MaryEra, ShelleyEra )
import Cardano.Ledger.Alonzo.Tx
    ( IsValid, isValidTxL )
import Cardano.Wallet.Read.Eras
    ( EraFun (..) )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx )
import Control.Lens
    ( (^.) )

type family ScriptValidityType era where
  ScriptValidityType ByronEra = ()
  ScriptValidityType ShelleyEra = ()
  ScriptValidityType AllegraEra = ()
  ScriptValidityType MaryEra = ()
  ScriptValidityType AlonzoEra = IsValid
  ScriptValidityType BabbageEra = IsValid

newtype ScriptValidity era = ScriptValidity (ScriptValidityType era)

deriving instance Show (ScriptValidityType era) => Show (ScriptValidity era)
deriving instance Eq (ScriptValidityType era) => Eq (ScriptValidity era)

getEraScriptValidity :: EraFun Tx ScriptValidity
getEraScriptValidity = EraFun
    { byronFun = \_ -> ScriptValidity ()
    , shelleyFun =  \_ -> ScriptValidity ()
    , allegraFun = \_ -> ScriptValidity ()
    , maryFun = \_ -> ScriptValidity ()
    , alonzoFun = onTx $ \tx -> ScriptValidity (tx ^. isValidTxL)
    , babbageFun = onTx $ \tx -> ScriptValidity (tx ^. isValidTxL)
    }
