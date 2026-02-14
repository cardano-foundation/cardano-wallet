{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Convert from "Cardano.Wallet.Read" to primitive types.
module Cardano.Wallet.Primitive.Ledger.Read.PParams
    ( primitiveProtocolParameters
    )
where

import Cardano.Wallet.Primitive.Ledger.Byron
    ( protocolParametersFromPP
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( fromAllegraPParams
    , fromAlonzoPParams
    , fromBabbagePParams
    , fromConwayPParams
    , fromDijkstraPParams
    , fromMaryPParams
    , fromShelleyPParams
    )
import Cardano.Wallet.Primitive.Types.EraInfo
    ( EraInfo (..)
    )
import Ouroboros.Consensus.HardFork.History.Summary
    ( Bound
    )

import qualified Cardano.Wallet.Primitive.Types.ProtocolParameters as W
import qualified Cardano.Wallet.Read as Read

{-----------------------------------------------------------------------------
    Protocol Parameters
------------------------------------------------------------------------------}

-- | Compute wallet primitive 'W.ProtocolParameters' from ledger 'PParams'.
primitiveProtocolParameters
    :: forall era
     . Read.IsEra era
    => EraInfo Bound
    -> Read.PParams era
    -> W.ProtocolParameters
primitiveProtocolParameters eraBounds (Read.PParams pparams) =
    case Read.theEra :: Read.Era era of
        Read.Byron -> protocolParametersFromPP eraBounds pparams
        Read.Shelley -> fromShelleyPParams eraBounds pparams
        Read.Allegra -> fromAllegraPParams eraBounds pparams
        Read.Mary -> fromMaryPParams eraBounds pparams
        Read.Alonzo -> fromAlonzoPParams eraBounds pparams
        Read.Babbage -> fromBabbagePParams eraBounds pparams
        Read.Conway -> fromConwayPParams eraBounds pparams
        Read.Dijkstra -> fromDijkstraPParams eraBounds pparams
