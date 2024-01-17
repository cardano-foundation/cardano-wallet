{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
module Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Scripts
    ( alonzoAnyExplicitScript
    , babbageAnyExplicitScript
    , conwayAnyExplicitScript
    , toPlutusVer
    )
    where

import Prelude

import Cardano.Ledger.Api
    ( Alonzo
    , Babbage
    , Conway
    , ScriptHash
    , StandardCrypto
    , hashScript
    )
import Cardano.Ledger.Babbage
    ( AlonzoScript
    )
import Cardano.Ledger.Mary.Value
    ( PolicyID (..)
    )
import Cardano.Wallet.Primitive.Ledger.Convert
    ( toWalletScript
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
    , PlutusVersion (..)
    , ScriptReference (ViaSpending)
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId
    ( TokenPolicyId
    )
import Cardano.Wallet.Primitive.Types.WitnessCount
    ( WitnessCountCtx
    , toKeyRole
    )

import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Plutus.Language as Plutus

alonzoAnyExplicitScript
    :: WitnessCountCtx -> AlonzoScript Alonzo -> AnyExplicitScript
alonzoAnyExplicitScript witCtx = \case
    Alonzo.TimelockScript script ->
        NativeExplicitScript
            (toWalletScript (toKeyRole witCtx) script)
            ViaSpending
    script@(Alonzo.PlutusScript (Plutus.Plutus ver _)) ->
        PlutusExplicitScript
            (PlutusScriptInfo
                (toPlutusVer ver)
                (fromLedgerScriptHash $ hashScript @Alonzo script)
            )
            ViaSpending

babbageAnyExplicitScript
    :: WitnessCountCtx
    -> ( ScriptReference
        , ScriptHash StandardCrypto
        , AlonzoScript Babbage
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
        Alonzo.PlutusScript (Plutus.Plutus ver _) ->
            PlutusExplicitScript
                (PlutusScriptInfo
                    (toPlutusVer ver)
                    (fromLedgerScriptHash $ hashScript @Babbage script)
                )
                scriptRef

conwayAnyExplicitScript
    :: WitnessCountCtx
    -> ( ScriptReference
        , ScriptHash StandardCrypto
        , AlonzoScript Conway
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
        Alonzo.PlutusScript (Plutus.Plutus ver _) ->
            PlutusExplicitScript
                (PlutusScriptInfo
                    (toPlutusVer ver)
                    (fromLedgerScriptHash $ hashScript @Conway script)
                )
                scriptRef

toPlutusVer :: Plutus.Language -> PlutusVersion
toPlutusVer Plutus.PlutusV1 = PlutusVersionV1
toPlutusVer Plutus.PlutusV2 = PlutusVersionV2
toPlutusVer Plutus.PlutusV3 = PlutusVersionV3
