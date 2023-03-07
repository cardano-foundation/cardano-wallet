{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Module containing extra 'Cardano.Api' functionality needed by the wallet.
module Cardano.Api.Extra
    ( withShelleyBasedTx
    , inAnyCardanoEra
    , asAnyShelleyBasedEra
    , fromShelleyBasedScript
    ) where

import Prelude

import Cardano.Api
    ( CardanoEra (..)
    , InAnyCardanoEra (..)
    , InAnyShelleyBasedEra (..)
    , IsCardanoEra (cardanoEra)
    , IsShelleyBasedEra
    , PlutusScriptVersion (..)
    , Script (..)
    , ScriptInEra (..)
    , ScriptLanguageInEra (..)
    , ShelleyBasedEra (..)
    , Tx (..)
    )
import Cardano.Api.Shelley
    ( PlutusScript (PlutusScriptSerialised)
    , ShelleyLedgerEra
    , fromAllegraTimelock
    , fromShelleyMultiSig
    )

import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Core as Ledger

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
    InAnyCardanoEra ConwayEra a ->
        Just $ InAnyShelleyBasedEra ShelleyBasedEraConway a

-- Copied from cardano-api because it is not exported.
fromShelleyBasedScript
    :: ShelleyBasedEra era
    -> Ledger.Script (ShelleyLedgerEra era)
    -> ScriptInEra era
fromShelleyBasedScript era script = case era of
    ShelleyBasedEraShelley ->
        ScriptInEra SimpleScriptInShelley $
        SimpleScript $
        fromShelleyMultiSig script
    ShelleyBasedEraAllegra ->
        ScriptInEra SimpleScriptInAllegra $
        SimpleScript $
        fromAllegraTimelock script
    ShelleyBasedEraMary ->
        ScriptInEra SimpleScriptInMary $
        SimpleScript $
        fromAllegraTimelock script
    ShelleyBasedEraAlonzo ->
        case script of
            Alonzo.TimelockScript s ->
                ScriptInEra SimpleScriptInAlonzo $
                SimpleScript $
                fromAllegraTimelock s
            Alonzo.PlutusScript Alonzo.PlutusV1 s ->
                ScriptInEra PlutusScriptV1InAlonzo $
                PlutusScript PlutusScriptV1 $
                PlutusScriptSerialised s
            Alonzo.PlutusScript Alonzo.PlutusV2 _ ->
                error
                    "fromShelleyBasedScript: \
                    \PlutusV2 not supported in Alonzo era"
    ShelleyBasedEraBabbage ->
        case script of
            Alonzo.TimelockScript s ->
                ScriptInEra SimpleScriptInBabbage $
                SimpleScript $
                fromAllegraTimelock s
            Alonzo.PlutusScript Alonzo.PlutusV1 s ->
                ScriptInEra PlutusScriptV1InBabbage $
                PlutusScript PlutusScriptV1 $
                PlutusScriptSerialised s
            Alonzo.PlutusScript Alonzo.PlutusV2 s ->
                ScriptInEra PlutusScriptV2InBabbage $
                PlutusScript PlutusScriptV2 $
                PlutusScriptSerialised s

    ShelleyBasedEraConway ->
        case script of
            Alonzo.TimelockScript s ->
                ScriptInEra SimpleScriptInConway $
                SimpleScript $
                fromAllegraTimelock s
            Alonzo.PlutusScript Alonzo.PlutusV1 s ->
                ScriptInEra PlutusScriptV1InConway $
                PlutusScript PlutusScriptV1 $
                PlutusScriptSerialised s
            Alonzo.PlutusScript Alonzo.PlutusV2 s ->
                ScriptInEra PlutusScriptV2InConway $
                PlutusScript PlutusScriptV2 $
                PlutusScriptSerialised s
