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
import Ouroboros.Consensus.Shelley.Eras
    ( StandardAlonzo
    , StandardBabbage
    , StandardConway
    )

import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo

alonzoAnyExplicitScript
    :: WitnessCountCtx -> AlonzoScript Alonzo -> AnyExplicitScript
alonzoAnyExplicitScript witCtx = \case
    Alonzo.TimelockScript script ->
        NativeExplicitScript
            (toWalletScript (toKeyRole witCtx) script)
            ViaSpending
    script@(Alonzo.PlutusScript s) ->
        PlutusExplicitScript
            (PlutusScriptInfo
                (toPlutusScriptInfo @StandardAlonzo s)
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
        Alonzo.PlutusScript s ->
            PlutusExplicitScript
                (PlutusScriptInfo
                    (toPlutusScriptInfo @StandardBabbage s)
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
        Alonzo.PlutusScript s ->
            PlutusExplicitScript
                (PlutusScriptInfo
                    (toPlutusScriptInfo @StandardConway s)
                    (fromLedgerScriptHash $ hashScript @Conway script)
                )
                scriptRef
