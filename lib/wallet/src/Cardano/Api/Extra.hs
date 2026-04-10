{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Module containing extra 'Cardano.Api' functionality needed by the wallet.
module Cardano.Api.Extra
    ( inAnyCardanoEra
    , CardanoApiEra
    , cardanoApiEraConstraints
    , shelleyBasedEraFromRecentEra
    , cardanoEraFromRecentEra
    , toCardanoApiTx
    , fromCardanoApiTx
    ) where

import Cardano.Balance.Tx.Eras
    ( Conway
    , IsRecentEra (recentEra)
    , RecentEra (..)
    )
import Data.Typeable
    ( Typeable
    )
import Prelude

import qualified Cardano.Api as Cardano
import qualified Cardano.Balance.Tx.Tx as Write

-- | Helper function for more easily creating an existential
-- @InAnyCardanoEra Tx@.
inAnyCardanoEra
    :: Cardano.IsCardanoEra era
    => Cardano.Tx era
    -> Cardano.InAnyCardanoEra Cardano.Tx
inAnyCardanoEra = Cardano.InAnyCardanoEra Cardano.cardanoEra

-- | Temporary shim: maps balance-tx RecentEra types to Cardano.Api era types.
-- Will be removed when cardano-api dependency is fully eliminated.
type family CardanoApiEra era = cardanoApiEra | cardanoApiEra -> era

type instance CardanoApiEra Conway = Cardano.ConwayEra

-- | Bring Cardano.Api era constraints into scope for a 'RecentEra'.
cardanoApiEraConstraints
    :: RecentEra era
    -> ( ( Cardano.IsCardanoEra (CardanoApiEra era)
         , Cardano.IsShelleyBasedEra (CardanoApiEra era)
         , Cardano.ShelleyLedgerEra (CardanoApiEra era) ~ era
         , Typeable (CardanoApiEra era)
         )
         => a
       )
    -> a
cardanoApiEraConstraints RecentEraConway f = f
cardanoApiEraConstraints RecentEraDijkstra _ =
    error "cardanoApiEraConstraints: Dijkstra era not yet supported"

-- | Temporary shim for 'shelleyBasedEraFromRecentEra'.
shelleyBasedEraFromRecentEra
    :: RecentEra era
    -> Cardano.ShelleyBasedEra (CardanoApiEra era)
shelleyBasedEraFromRecentEra RecentEraConway =
    Cardano.ShelleyBasedEraConway
shelleyBasedEraFromRecentEra RecentEraDijkstra =
    error "shelleyBasedEraFromRecentEra: Dijkstra era not yet supported"

-- | Temporary shim for 'cardanoEraFromRecentEra'.
cardanoEraFromRecentEra
    :: RecentEra era
    -> Cardano.CardanoEra (CardanoApiEra era)
cardanoEraFromRecentEra RecentEraConway =
    Cardano.ConwayEra
cardanoEraFromRecentEra RecentEraDijkstra =
    error "cardanoEraFromRecentEra: Dijkstra era not yet supported"

-- | Convert a ledger-era transaction to a Cardano.Api transaction.
toCardanoApiTx
    :: forall era
     . IsRecentEra era
    => Write.Tx era
    -> Cardano.Tx (CardanoApiEra era)
toCardanoApiTx tx = case recentEra :: RecentEra era of
    RecentEraConway ->
        Cardano.ShelleyTx Cardano.ShelleyBasedEraConway tx
    RecentEraDijkstra ->
        error "toCardanoApiTx: Dijkstra era not yet supported"

-- | Convert a Cardano.Api transaction to a ledger-era transaction.
fromCardanoApiTx
    :: forall era
     . IsRecentEra era
    => Cardano.Tx (CardanoApiEra era)
    -> Write.Tx era
fromCardanoApiTx tx = case recentEra :: RecentEra era of
    RecentEraConway -> case tx of
        Cardano.ShelleyTx _ ledgerTx -> ledgerTx
    RecentEraDijkstra ->
        error "fromCardanoApiTx: Dijkstra era not yet supported"
