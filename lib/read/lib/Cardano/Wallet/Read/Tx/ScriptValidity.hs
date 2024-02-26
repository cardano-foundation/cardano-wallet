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
-- Raw mint data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.ScriptValidity
    ( ScriptValidityType
    , ScriptValidity (..)
    , getEraScriptValidity
    ) where

import Prelude

import Cardano.Ledger.Alonzo.Tx
    ( IsValid
    , isValidTxL
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

type family ScriptValidityType era where
  ScriptValidityType Byron = ()
  ScriptValidityType Shelley = ()
  ScriptValidityType Allegra = ()
  ScriptValidityType Mary = ()
  ScriptValidityType Alonzo = IsValid
  ScriptValidityType Babbage = IsValid
  ScriptValidityType Conway = IsValid

newtype ScriptValidity era = ScriptValidity (ScriptValidityType era)

deriving instance Show (ScriptValidityType era) => Show (ScriptValidity era)
deriving instance Eq (ScriptValidityType era) => Eq (ScriptValidity era)

getEraScriptValidity :: EraFun Tx ScriptValidity
getEraScriptValidity =
    EraFun
        { byronFun = \_ -> ScriptValidity ()
        , shelleyFun = \_ -> ScriptValidity ()
        , allegraFun = \_ -> ScriptValidity ()
        , maryFun = \_ -> ScriptValidity ()
        , alonzoFun = alonzoScriptValidity
        , babbageFun = alonzoScriptValidity
        , conwayFun = alonzoScriptValidity
        }
  where
    alonzoScriptValidity = onTx $
        \tx -> ScriptValidity $ tx ^. isValidTxL
