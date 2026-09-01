{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.ScriptValidity
    ( getScriptValidity
    )
where

import Cardano.Ledger.Alonzo.Tx
    ( IsPhase2Valid (..)
    )
import Cardano.Read.Ledger.Tx.ScriptValidity
    ( ScriptValidity (..)
    )
import Cardano.Wallet.Read.Eras
    ( Era (..)
    , IsEra (..)
    )
import Prelude

import qualified Cardano.Wallet.Primitive.Types.Tx.Tx as W

getScriptValidity
    :: forall era
     . IsEra era => ScriptValidity era -> Maybe W.TxScriptValidity
getScriptValidity = case theEra @era of
    Byron -> noScriptValidity
    Shelley -> noScriptValidity
    Allegra -> noScriptValidity
    Mary -> noScriptValidity
    Alonzo -> yesScriptValidity
    Babbage -> yesScriptValidity
    Conway -> yesScriptValidity
    Dijkstra -> yesScriptValidity
  where
    noScriptValidity _ = Nothing
    yesScriptValidity (ScriptValidity Phase2Valid) = Just W.TxScriptValid
    yesScriptValidity (ScriptValidity Phase2Invalid) = Just W.TxScriptInvalid
