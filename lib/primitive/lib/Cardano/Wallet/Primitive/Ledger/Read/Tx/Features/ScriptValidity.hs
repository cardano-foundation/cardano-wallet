{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.ScriptValidity
    ( getScriptValidity
    )
where

import Prelude

import Cardano.Ledger.Alonzo.Tx
    ( IsValid (..)
    )
import Cardano.Wallet.Read.Eras
    ( Era (..)
    , IsEra (..)
    )
import Cardano.Wallet.Read.Tx.ScriptValidity
    ( ScriptValidity (..)
    )

import qualified Cardano.Wallet.Primitive.Types.Tx.Tx as W

getScriptValidity :: forall era. IsEra era => ScriptValidity era -> Maybe W.TxScriptValidity
getScriptValidity = case theEra @era of
    Byron -> noScriptValidity
    Shelley -> noScriptValidity
    Allegra -> noScriptValidity
    Mary -> noScriptValidity
    Alonzo -> yesScriptValidity
    Babbage -> yesScriptValidity
    Conway -> yesScriptValidity
  where
    noScriptValidity _ = Nothing
    yesScriptValidity (ScriptValidity (IsValid b))
        | b = Just W.TxScriptValid
        | otherwise = Just W.TxScriptInvalid
