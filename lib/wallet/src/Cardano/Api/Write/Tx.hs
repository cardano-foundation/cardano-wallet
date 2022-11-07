{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Api.Write.Tx
    ( module Cardano.Wallet.Write.Tx
    , RecentCardanoEra (..)
    , IsRecentCardanoEra (..)
    , InAnyRecentEra (..)
    , withRecentEra
    , asAnyRecentEra
    )

    where

import Prelude

import Cardano.Api
    ( InAnyCardanoEra (..), IsShelleyBasedEra, ShelleyBasedEra )
import Cardano.Wallet.Write.Tx

import qualified Cardano.Api as Cardano

type RecentCardanoEra era = RecentEra (ShelleyBasedEra era)

-- | Variation of 'IsRecentEra' which is more easy to use with @cardano-api@
-- types.
class
    ( IsShelleyBasedEra era
    , IsRecentEra (ShelleyLedgerEra era)
    , CardanoApiEra (ShelleyLedgerEra era) ~ era
    )
    => IsRecentCardanoEra era where
    recentCardanoEra :: RecentEra (ShelleyLedgerEra era)

instance IsRecentCardanoEra Cardano.BabbageEra where
    recentCardanoEra = RecentEraBabbage

instance IsRecentCardanoEra Cardano.AlonzoEra where
    recentCardanoEra = RecentEraAlonzo

data InAnyRecentEra thing where
     InAnyRecentEra
         :: IsRecentCardanoEra era -- Provide class constraint
         => RecentEra (ShelleyLedgerEra era)   -- and explicit value.
         -> thing era
         -> InAnyRecentEra thing

withRecentEra
    :: InAnyRecentEra thing
    -> (forall era. (IsRecentCardanoEra era) => thing era -> a)
    -> a
withRecentEra (InAnyRecentEra _era tx) f
    = f tx

-- | "Downcast" something existentially wrapped in 'Cardano.InAnyCardanoEra'.
asAnyRecentEra
    :: Cardano.InAnyCardanoEra a
    -> Maybe (InAnyRecentEra a)
asAnyRecentEra = \case
    InAnyCardanoEra Cardano.BabbageEra a ->
        Just $ InAnyRecentEra RecentEraBabbage a
    InAnyCardanoEra Cardano.AlonzoEra a ->
        Just $ InAnyRecentEra RecentEraAlonzo a
    _ -> Nothing
