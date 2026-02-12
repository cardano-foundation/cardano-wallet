{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
module Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Scripts
    ( alonzoAnyExplicitScript
    , babbageAnyExplicitScript
    , conwayAnyExplicitScript
    )
where

import Cardano.Ledger.Alonzo
    ( AlonzoEra
    )
import Cardano.Ledger.Api
    ( ScriptHash
    , hashScript
    )
import Cardano.Ledger.Babbage
    ( AlonzoScript
    , BabbageEra
    )
import Cardano.Ledger.Conway
    ( ConwayEra
    )
import Cardano.Ledger.Mary.Value
    ( PolicyID (..)
    )
import Cardano.Wallet.Primitive.Ledger.Convert
    ( toPlutusScriptInfo
    , toWalletScript
    , toWalletTokenPolicyId
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Mint
    ( fromLedgerScriptHash
    )
import Cardano.Wallet.Primitive.Types.AnyExplicitScripts
    ( AnyExplicitScript (..)
    )
import Cardano.Wallet.Primitive.Types.TokenMapWithScripts
    ( PlutusScriptInfo (PlutusScriptInfo)
    , ScriptReference (ViaSpending)
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId
    ( TokenPolicyId
    )
import Cardano.Wallet.Primitive.Types.WitnessCount
    ( WitnessCountCtx
    , toKeyRole
    )
import Prelude

import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo

alonzoAnyExplicitScript
    :: WitnessCountCtx -> AlonzoScript AlonzoEra -> AnyExplicitScript
alonzoAnyExplicitScript witCtx = \case
    Alonzo.TimelockScript script ->
        NativeExplicitScript
            (toWalletScript (toKeyRole witCtx) script)
            ViaSpending
    script@(Alonzo.PlutusScript s) ->
        PlutusExplicitScript
            ( PlutusScriptInfo
                (toPlutusScriptInfo @AlonzoEra s)
                (fromLedgerScriptHash $ hashScript @AlonzoEra script)
            )
            ViaSpending

babbageAnyExplicitScript
    :: WitnessCountCtx
    -> ( ScriptReference
       , ScriptHash
       , AlonzoScript BabbageEra
       )
    -> (TokenPolicyId, AnyExplicitScript)
babbageAnyExplicitScript witCtx (scriptRef, scriptH, script) =
    (toWalletTokenPolicyId (PolicyID scriptH), toAnyScript script)
  where
    toAnyScript = \case
        Alonzo.TimelockScript timelockScript ->
            NativeExplicitScript
                (toWalletScript (toKeyRole witCtx) timelockScript)
                scriptRef
        Alonzo.PlutusScript s ->
            PlutusExplicitScript
                ( PlutusScriptInfo
                    (toPlutusScriptInfo @BabbageEra s)
                    (fromLedgerScriptHash $ hashScript @BabbageEra script)
                )
                scriptRef

conwayAnyExplicitScript
    :: WitnessCountCtx
    -> ( ScriptReference
       , ScriptHash
       , AlonzoScript ConwayEra
       )
    -> (TokenPolicyId, AnyExplicitScript)
conwayAnyExplicitScript witCtx (scriptRef, scriptH, script) =
    (toWalletTokenPolicyId (PolicyID scriptH), toAnyScript script)
  where
    toAnyScript = \case
        Alonzo.TimelockScript timelockScript ->
            NativeExplicitScript
                (toWalletScript (toKeyRole witCtx) timelockScript)
                scriptRef
        Alonzo.PlutusScript s ->
            PlutusExplicitScript
                ( PlutusScriptInfo
                    (toPlutusScriptInfo @ConwayEra s)
                    (fromLedgerScriptHash $ hashScript @ConwayEra script)
                )
                scriptRef
