{-# LANGUAGE GADTs #-}
-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- A local state query that retrieves the current protocol parameters.
--
module Cardano.Wallet.Network.LocalStateQuery.PParams
    ( protocolParams
    , protocolParamsLegacy
    , slottingParamsLegacy
    ) where

import Prelude

import Cardano.Wallet.Network.Implementation.Ouroboros
    ( LSQ (..)
    )
import Cardano.Wallet.Network.LocalStateQuery.Extra
    ( byronOrShelleyBased
    , onAnyEra
    )
import Cardano.Wallet.Primitive.Ledger.Byron
    ( protocolParametersFromUpdateState
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( fromAllegraPParams
    , fromAlonzoPParams
    , fromBabbagePParams
    , fromConwayPParams
    , fromMaryPParams
    , fromShelleyPParams
    , slottingParametersFromGenesis
    )
import Cardano.Wallet.Primitive.Types.EraInfo
    ( EraInfo (..)
    )
import Cardano.Wallet.Primitive.Types.NetworkParameters
    ( NetworkParameters (..)
    )
import Cardano.Wallet.Primitive.Types.ProtocolParameters
    ( ProtocolParameters
    )
import Cardano.Wallet.Primitive.Types.SlottingParameters
    ( SlottingParameters
    )
import Internal.Cardano.Write.Tx
    ( MaybeInRecentEra (..)
    )
import Ouroboros.Consensus.Cardano
    ( CardanoBlock
    )
import Ouroboros.Consensus.Cardano.Block
    ( BlockQuery (..)
    )
import Ouroboros.Consensus.HardFork.Combinator
    ( QueryAnytime (..)
    )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardCrypto
    )
import Ouroboros.Consensus.Shelley.Ledger.Config
    ( getCompactGenesis
    )

import qualified Internal.Cardano.Write.Tx as Write
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley

{-----------------------------------------------------------------------------
    Local State Query for PParams
------------------------------------------------------------------------------}
type LSQ' m = LSQ (CardanoBlock StandardCrypto) m

protocolParams :: LSQ' m (MaybeInRecentEra Write.PParams)
protocolParams =
    onAnyEra
        (pure InNonRecentEraByron)
        (pure InNonRecentEraShelley)
        (pure InNonRecentEraAllegra)
        (pure InNonRecentEraMary)
        (pure InNonRecentEraAlonzo)
        (InRecentEraBabbage <$> LSQry Shelley.GetCurrentPParams)
        (InRecentEraConway <$> LSQry Shelley.GetCurrentPParams)

slottingParamsLegacy :: NetworkParameters -> LSQ' m SlottingParameters
slottingParamsLegacy np =
    byronOrShelleyBased
        (pure $ slottingParameters np)
        ( (slottingParametersFromGenesis . getCompactGenesis)
            <$> LSQry Shelley.GetGenesisConfig
        )

protocolParamsLegacy :: LSQ' m ProtocolParameters
protocolParamsLegacy = do
    eraBounds <-
        EraInfo
            <$> LSQry (QueryAnytimeByron GetEraStart)
            <*> LSQry (QueryAnytimeShelley GetEraStart)
            <*> LSQry (QueryAnytimeAllegra GetEraStart)
            <*> LSQry (QueryAnytimeMary GetEraStart)
            <*> LSQry (QueryAnytimeAlonzo GetEraStart)
            <*> LSQry (QueryAnytimeBabbage GetEraStart)

    onAnyEra
        ( protocolParametersFromUpdateState eraBounds
            <$> LSQry Byron.GetUpdateInterfaceState
        )
        ( fromShelleyPParams eraBounds
            <$> LSQry Shelley.GetCurrentPParams
        )
        ( fromAllegraPParams eraBounds
            <$> LSQry Shelley.GetCurrentPParams
        )
        ( fromMaryPParams eraBounds
            <$> LSQry Shelley.GetCurrentPParams
        )
        ( fromAlonzoPParams eraBounds
            <$> LSQry Shelley.GetCurrentPParams
        )
        ( fromBabbagePParams eraBounds
            <$> LSQry Shelley.GetCurrentPParams
        )
        ( fromConwayPParams eraBounds
            <$> LSQry Shelley.GetCurrentPParams
        )
