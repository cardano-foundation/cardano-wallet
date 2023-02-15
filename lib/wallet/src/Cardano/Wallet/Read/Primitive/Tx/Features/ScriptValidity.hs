{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Read.Primitive.Tx.Features.ScriptValidity
    ( getScriptValidity
    )
    where

import Prelude

import Cardano.Ledger.Alonzo.Tx
    ( IsValid (..) )
import Cardano.Wallet.Read.Eras
    ( EraFun (..), K (..) )
import Cardano.Wallet.Read.Tx.ScriptValidity
    ( ScriptValidity (..) )

import qualified Cardano.Wallet.Primitive.Types.Tx.Tx as W

getScriptValidity :: EraFun ScriptValidity (K (Maybe (W.TxScriptValidity) ))
getScriptValidity = EraFun
    { byronFun = noScriptValidity
    , shelleyFun = noScriptValidity
    , allegraFun = noScriptValidity
    , maryFun = noScriptValidity
    , alonzoFun = yesScriptValidity
    , babbageFun = yesScriptValidity
    , conwayFun = yesScriptValidity
    }
    where
        noScriptValidity _ = K Nothing
        yesScriptValidity (ScriptValidity (IsValid b))
            | b = K . Just $ W.TxScriptValid
            | otherwise = K . Just $ W.TxScriptInvalid

