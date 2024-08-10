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
-- Raw mint data extraction from 'Tx'
--

module Cardano.Read.Ledger.Tx.ScriptValidity
    ( ScriptValidityType
    , ScriptValidity (..)
    , getEraScriptValidity
    ) where

import Prelude

import Cardano.Ledger.Alonzo.Tx
    ( IsValid
    , isValidTxL
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

{-# INLINABLE getEraScriptValidity #-}
getEraScriptValidity :: forall era. IsEra era => Tx era -> ScriptValidity era
getEraScriptValidity = case theEra @era of
    Byron -> \_ -> ScriptValidity ()
    Shelley -> \_ -> ScriptValidity ()
    Allegra -> \_ -> ScriptValidity ()
    Mary -> \_ -> ScriptValidity ()
    Alonzo -> alonzoScriptValidity
    Babbage -> alonzoScriptValidity
    Conway -> alonzoScriptValidity
  where
    alonzoScriptValidity = onTx $ \tx -> ScriptValidity $ tx ^. isValidTxL
