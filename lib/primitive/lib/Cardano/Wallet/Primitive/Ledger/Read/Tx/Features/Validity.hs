{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Validity
    ( getValidity
    , afterShelleyValidityInterval
    , shelleyValidityInterval
    )
    where

import Prelude

import Cardano.Ledger.Api
    ( ValidityInterval (..)
    )
import Cardano.Wallet.Primitive.Types.ValidityIntervalExplicit
    ( ValidityIntervalExplicit (ValidityIntervalExplicit)
    )
import Cardano.Wallet.Read.Eras
    ( EraFun (..)
    , K (..)
    )
import Cardano.Wallet.Read.Tx.Validity
    ( Validity (..)
    )
import Data.Maybe.Strict
    ( StrictMaybe (..)
    )
import Data.Quantity
    ( Quantity (..)
    )

import qualified Ouroboros.Network.Block as O

getValidity :: EraFun Validity (K (Maybe ValidityIntervalExplicit))
getValidity =
    EraFun
        { byronFun = \_validity -> K Nothing
        , shelleyFun = \(Validity ttl) -> K . Just $ shelleyValidityInterval ttl
        , allegraFun = afterShelleyValidity
        , maryFun = afterShelleyValidity
        , alonzoFun = afterShelleyValidity
        , babbageFun = afterShelleyValidity
        , conwayFun = afterShelleyValidity
        }
  where
    afterShelleyValidity (Validity validity)
        = K . Just $ afterShelleyValidityInterval validity

afterShelleyValidityInterval :: ValidityInterval -> ValidityIntervalExplicit
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
shelleyValidityInterval (O.SlotNo n)
    = ValidityIntervalExplicit (Quantity 0) (Quantity n)
