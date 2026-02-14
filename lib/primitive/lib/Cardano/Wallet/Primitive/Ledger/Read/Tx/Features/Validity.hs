{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Validity
    ( getValidity
    , afterShelleyValidityInterval
    , shelleyValidityInterval
    )
where

import Cardano.Ledger.Api
    ( ValidityInterval (..)
    )
import Cardano.Read.Ledger.Tx.Validity
    ( Validity (..)
    )
import Cardano.Wallet.Primitive.Types.ValidityIntervalExplicit
    ( ValidityIntervalExplicit (ValidityIntervalExplicit)
    )
import Cardano.Wallet.Read.Eras
    ( Era (..)
    , IsEra (..)
    )
import Data.Maybe.Strict
    ( StrictMaybe (..)
    )
import Data.Quantity
    ( Quantity (..)
    )
import Prelude

import qualified Ouroboros.Network.Block as O

{-# INLINEABLE getValidity #-}
getValidity
    :: forall era
     . IsEra era => Validity era -> Maybe ValidityIntervalExplicit
getValidity = case theEra @era of
    Byron -> \_validity -> Nothing
    Shelley -> \(Validity ttl) -> Just $ shelleyValidityInterval ttl
    Allegra -> afterShelleyValidity
    Mary -> afterShelleyValidity
    Alonzo -> afterShelleyValidity
    Babbage -> afterShelleyValidity
    Conway -> afterShelleyValidity
    Dijkstra -> afterShelleyValidity
  where
    afterShelleyValidity (Validity validity) =
        Just $ afterShelleyValidityInterval validity

afterShelleyValidityInterval
    :: ValidityInterval -> ValidityIntervalExplicit
afterShelleyValidityInterval (ValidityInterval from to) =
    case (from, to) of
        (SNothing, SJust (O.SlotNo s)) ->
            ValidityIntervalExplicit (Quantity 0) (Quantity s)
        (SNothing, SNothing) ->
            ValidityIntervalExplicit (Quantity 0) (Quantity maxBound)
        (SJust (O.SlotNo s1), SJust (O.SlotNo s2)) ->
            ValidityIntervalExplicit (Quantity s1) (Quantity s2)
        (SJust (O.SlotNo s1), SNothing) ->
            ValidityIntervalExplicit (Quantity s1) (Quantity maxBound)

shelleyValidityInterval :: O.SlotNo -> ValidityIntervalExplicit
shelleyValidityInterval (O.SlotNo n) =
    ValidityIntervalExplicit (Quantity 0) (Quantity n)
