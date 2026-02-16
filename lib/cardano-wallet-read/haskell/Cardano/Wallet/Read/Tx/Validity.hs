{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Validity interval of a transaction.
module Cardano.Wallet.Read.Tx.Validity
    ( ValidityInterval (ValidityIntervalC)
    , invalidBefore
    , invalidHereafter
    , getValidityInterval
    ) where

import Cardano.Ledger.Api
    ( ValidityInterval (ValidityInterval)
    )
import Cardano.Read.Ledger.Block.SlotNo
    ( SlotNo (..)
    , fromLedgerSlotNo
    , toLedgerSlotNo
    )
import Cardano.Read.Ledger.Tx.Validity
    ( Validity (..)
    , ValidityType
    , getEraValidity
    )
import Cardano.Wallet.Read.Eras
    ( Era (..)
    , IsEra (..)
    )
import Cardano.Wallet.Read.Tx.Tx
    ( Tx (..)
    )
import Data.Maybe.Strict
    ( StrictMaybe (..)
    , maybeToStrictMaybe
    , strictMaybeToMaybe
    )
import Prelude

{-# COMPLETE ValidityIntervalC #-}

-- | Alternative view on 'Cardano.Ledger.Api.ValidityInterval'
-- from "Cardano.Ledger.Api".
pattern ValidityIntervalC
    :: Maybe SlotNo
    -- ^ 'invalidBefore'
    -> Maybe SlotNo
    -- ^ 'invalidHereafter'
    -> ValidityInterval
pattern ValidityIntervalC{invalidBefore, invalidHereafter} <-
    (toPair -> (invalidBefore, invalidHereafter))
    where
        ValidityIntervalC x y = fromPair (x, y)

fromPair :: (Maybe SlotNo, Maybe SlotNo) -> ValidityInterval
fromPair (x, y) = ValidityInterval (f x) (f y)
  where
    f = fmap toLedgerSlotNo . maybeToStrictMaybe

toPair :: ValidityInterval -> (Maybe SlotNo, Maybe SlotNo)
toPair (ValidityInterval x y) = (f x, f y)
  where
    f = fmap fromLedgerSlotNo . strictMaybeToMaybe

{-# INLINEABLE getValidityInterval #-}

-- | Get the validity interval of a transaction.
getValidityInterval
    :: forall era. IsEra era => Tx era -> ValidityInterval
getValidityInterval = case theEra :: Era era of
    Byron -> onValidity (const $ ValidityInterval SNothing SNothing)
    Shelley -> onValidity (ValidityInterval SNothing . SJust)
    Allegra -> onValidity id
    Mary -> onValidity id
    Alonzo -> onValidity id
    Babbage -> onValidity id
    Conway -> onValidity id
    Dijkstra -> onValidity id

-- Helper function for type inference.
onValidity
    :: IsEra era
    => (ValidityType era -> t)
    -> Tx era
    -> t
onValidity f x =
    case getEraValidity x of
        Validity v -> f v
