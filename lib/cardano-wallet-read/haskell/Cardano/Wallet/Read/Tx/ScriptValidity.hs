{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Copyright: © 2024 IOHK
-- License: Apache-2.0
--
-- Script validity of a transaction.
module Cardano.Wallet.Read.Tx.ScriptValidity
    ( type IsPhase2Valid
    , pattern IsValidC
    , getScriptValidity
    ) where

import Cardano.Ledger.Alonzo.Tx
    ( IsPhase2Valid (..)
    )
import Cardano.Read.Ledger.Tx.ScriptValidity
    ( ScriptValidity (..)
    , ScriptValidityType
    , getEraScriptValidity
    )
import Cardano.Wallet.Read.Eras
    ( Era (..)
    , IsEra (..)
    )
import Cardano.Wallet.Read.Tx.Tx
    ( Tx (..)
    )
import Prelude

isPhase2ValidToBool :: IsPhase2Valid -> Bool
isPhase2ValidToBool Phase2Valid = True
isPhase2ValidToBool Phase2Invalid = False

pattern IsValidC :: Bool -> IsPhase2Valid
pattern IsValidC b <- (isPhase2ValidToBool -> b)
    where
        IsValidC True = Phase2Valid
        IsValidC False = Phase2Invalid

{-# COMPLETE IsValidC #-}

{-# INLINEABLE getScriptValidity #-}
getScriptValidity :: forall era. IsEra era => Tx era -> IsPhase2Valid
getScriptValidity = case theEra :: Era era of
    Byron -> onScriptValidity trueValid
    Shelley -> onScriptValidity trueValid
    Allegra -> onScriptValidity trueValid
    Mary -> onScriptValidity trueValid
    Alonzo -> onScriptValidity id
    Babbage -> onScriptValidity id
    Conway -> onScriptValidity id
    Dijkstra -> onScriptValidity id
  where
    trueValid = const Phase2Valid

-- Helper function for type inference.
onScriptValidity
    :: IsEra era
    => (ScriptValidityType era -> t)
    -> Tx era
    -> t
onScriptValidity f x =
    case getEraScriptValidity x of
        ScriptValidity v -> f v
