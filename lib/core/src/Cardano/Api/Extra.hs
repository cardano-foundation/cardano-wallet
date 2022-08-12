{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Module containing extra 'Cardano.Api' functionality needed by the wallet.
module Cardano.Api.Extra
    ( withShelleyBasedTx
    , inAnyCardanoEra
    , asAnyShelleyBasedEra
    ) where

import Prelude

import Cardano.Api
    ( CardanoEra (..)
    , InAnyCardanoEra (..)
    , InAnyShelleyBasedEra (..)
    , IsCardanoEra (cardanoEra)
    , IsShelleyBasedEra
    , ScriptExecutionError (..)
    , ShelleyBasedEra (..)
    , TransactionValidityError (..)
    , Tx
    )
import Cardano.Wallet.Orphans
    ()

-- | Apply an era-parameterized function to an existentially-wrapped
-- tx.
withShelleyBasedTx
    :: InAnyShelleyBasedEra Tx
    -> (forall era. IsShelleyBasedEra era => Tx era -> a)
    -> a
withShelleyBasedTx (InAnyShelleyBasedEra _era tx) f
    = f tx

-- | Helper function for more easily creating an existential
-- @InAnyCardanoEra Tx@.
inAnyCardanoEra :: IsCardanoEra era => Tx era -> InAnyCardanoEra Tx
inAnyCardanoEra = InAnyCardanoEra cardanoEra

-- | "Downcast" an existentially wrapped tx.
asAnyShelleyBasedEra
    :: InAnyCardanoEra a
    -> Maybe (InAnyShelleyBasedEra a)
asAnyShelleyBasedEra = \case
    InAnyCardanoEra ByronEra _ ->
        Nothing
    InAnyCardanoEra ShelleyEra a ->
        Just $ InAnyShelleyBasedEra ShelleyBasedEraShelley a
    InAnyCardanoEra AllegraEra a ->
        Just $ InAnyShelleyBasedEra ShelleyBasedEraAllegra a
    InAnyCardanoEra MaryEra a ->
        Just $ InAnyShelleyBasedEra ShelleyBasedEraMary a
    InAnyCardanoEra AlonzoEra a ->
        Just $ InAnyShelleyBasedEra ShelleyBasedEraAlonzo a
    InAnyCardanoEra BabbageEra a ->
        Just $ InAnyShelleyBasedEra ShelleyBasedEraBabbage a

-- TODO: Open PR to cardano-api
deriving instance Eq ScriptExecutionError

-- NOTE: Needs Eq PastHorizonException orphan
deriving instance Eq TransactionValidityError
