{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright: © 2024 IOHK
License: Apache-2.0

Script validity of a transaction.
-}
module Cardano.Wallet.Read.Tx.ScriptValidity
    ( IsValid (IsValidC)
    , getScriptValidity
    ) where

import Prelude

import Cardano.Ledger.Alonzo.Tx
    ( IsValid (IsValid)
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

{-# COMPLETE IsValidC #-}
pattern IsValidC :: Bool -> IsValid
pattern IsValidC x = IsValid x

{-# INLINABLE getScriptValidity #-}
getScriptValidity :: forall era. IsEra era => Tx era -> IsValid
getScriptValidity = case theEra :: Era era of
    Byron -> onScriptValidity trueValid
    Shelley -> onScriptValidity trueValid
    Allegra -> onScriptValidity trueValid
    Mary -> onScriptValidity trueValid
    Alonzo -> onScriptValidity id
    Babbage -> onScriptValidity id
    Conway -> onScriptValidity id
  where
    trueValid = const (IsValid True)

-- Helper function for type inference.
onScriptValidity
    :: IsEra era
    => (ScriptValidityType era -> t)
    -> Tx era -> t
onScriptValidity f x =
    case getEraScriptValidity x of
        ScriptValidity v -> f v
