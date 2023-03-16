{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Read.Primitive.Tx.Conway
    ( fromConwayTx
    )
    where

import Prelude

import Cardano.Api
    ( ConwayEra )
import Cardano.Ledger.Era
    ( Era (..) )
import Cardano.Ledger.Serialization
    ( sizedValue )
import Cardano.Ledger.Shelley.API
    ( StrictMaybe (SJust, SNothing) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenPolicyId (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..) )
import Cardano.Wallet.Read.Eras
    ( conway, inject )
import Cardano.Wallet.Read.Primitive.Tx.Features.Certificates
    ( anyEraCerts )
import Cardano.Wallet.Read.Primitive.Tx.Features.Fee
    ( fromShelleyCoin )
import Cardano.Wallet.Read.Primitive.Tx.Features.Inputs
    ( fromShelleyTxIn )
import Cardano.Wallet.Read.Primitive.Tx.Features.Metadata
    ( fromConwayMetadata )
import Cardano.Wallet.Read.Primitive.Tx.Features.Mint
    ( conwayMint, fromLedgerScriptHash )
import Cardano.Wallet.Read.Primitive.Tx.Features.Outputs
    ( fromConwayTxOut )
import Cardano.Wallet.Read.Primitive.Tx.Features.Validity
    ( afterShelleyValidityInterval )
import Cardano.Wallet.Read.Primitive.Tx.Features.Withdrawals
    ( fromShelleyWdrl )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.CBOR
    ( renderTxToCBOR )
import Cardano.Wallet.Read.Tx.Hash
    ( alonzoTxHash )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( toWalletScript, toWalletTokenPolicyId )
import Cardano.Wallet.Transaction
    ( AnyExplicitScript (..)
    , PlutusScriptInfo (..)
    , PlutusVersion (..)
    , ReferenceInput (..)
    , ScriptReference (..)
    , TokenMapWithScripts (..)
    , ValidityIntervalExplicit (..)
    , WitnessCount (..)
    , WitnessCountCtx
    , toKeyRole
    )
import Data.Foldable
    ( toList )
import Data.Maybe
    ( catMaybes )
import Ouroboros.Consensus.Cardano.Block
    ( StandardConway )

import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Core as SL.Core
import qualified Cardano.Ledger.Mary.Value as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

fromConwayTx
    :: Alonzo.AlonzoTx (Cardano.ShelleyLedgerEra ConwayEra)
    -> WitnessCountCtx
    -> ( W.Tx
       , [W.Certificate]
       , TokenMapWithScripts
       , TokenMapWithScripts
       , Maybe ValidityIntervalExplicit
       , WitnessCount
       )
fromConwayTx tx@(Alonzo.AlonzoTx bod wits (Alonzo.IsValid isValid) aux) witCtx =
    ( W.Tx
        { txId =
            transactionId
        , txCBOR =
            Just $ renderTxToCBOR $ inject conway $ Tx tx
        , fee =
            Just $ fromShelleyCoin fee
        , resolvedInputs =
            map ((,Nothing) . fromShelleyTxIn) (toList inps)
        , resolvedCollateralInputs =
            map ((,Nothing) . fromShelleyTxIn) (toList collateralInps)
        , outputs =
            map (fst . fromConwayTxOut . sizedValue) (toList outs)
        , collateralOutput =
            case fmap (fst . fromConwayTxOut . sizedValue) collateralReturn of
                SNothing -> Nothing
                SJust txout -> Just txout
        , withdrawals =
            fromShelleyWdrl wdrls
        , metadata =
            fromConwayMetadata <$> SL.strictMaybeToMaybe aux
        , scriptValidity =
            validity
        }
    , anyEraCerts certs
    , assetsToMint
    , assetsToBurn
    , Just $ afterShelleyValidityInterval ttl
    , countWits
    )
  where
    Babbage.BabbageTxBody
        inps
        collateralInps
        refInps
        outs
        collateralReturn
        _collateralTotal
        certs
        wdrls
        fee
        ttl
        _upd
        _reqSignerHashes
        mint
        _wwpHash
        _adHash
        _network
        = bod

    transactionId = W.Hash $ alonzoTxHash tx
    scriptWithHashIx ix txout =
        case (snd . fromConwayTxOut $ txout) of
            Just s -> Just ( ViaReferenceInput (ReferenceInput $ TxIn transactionId ix)
                           , hashConwayScript s
                           , s)
            Nothing -> Nothing
    scriptsFromTxOuts =
        catMaybes $
        L.zipWith scriptWithHashIx [0..] (map sizedValue $ toList outs)
    anyScriptsFromTxOuts =
        Map.fromList $
        map fromLedgerToAnyScript scriptsFromTxOuts
    scriptsFromWits =
        map (\(scriptH,script) -> (ViaSpending, scriptH,script)) $
        Map.toList $
        Alonzo.txscripts' wits
    anyScriptsFromWits =
        Map.fromList $
        map fromLedgerToAnyScript scriptsFromWits
    allAnyScripts = Map.union anyScriptsFromWits anyScriptsFromTxOuts

    (assetsToMint, assetsToBurn) = conwayMint refInps mint wits

    countWits = WitnessCount
        (fromIntegral $ Set.size $ Alonzo.txwitsVKey' wits)
        (Map.elems allAnyScripts)
        (fromIntegral $ Set.size $ Alonzo.txwitsBoot' wits)

    hashConwayScript = SL.Core.hashScript @(Cardano.ShelleyLedgerEra ConwayEra)

    fromLedgerToAnyScript
        :: ( ScriptReference
           , (SL.ScriptHash (Crypto StandardConway))
           , (SL.Core.Script StandardConway) )
        -> (TokenPolicyId, AnyExplicitScript)
    fromLedgerToAnyScript (scriptRef, scriptH, script) =
        (toWalletTokenPolicyId . SL.PolicyID $ scriptH, toAnyScript script)
      where
        toAnyScript (Alonzo.TimelockScript script') =
            NativeExplicitScript (toWalletScript (toKeyRole witCtx) script') scriptRef
        toAnyScript s@(Alonzo.PlutusScript ver _) =
            PlutusExplicitScript (PlutusScriptInfo (toPlutusVer ver)
                          (fromLedgerScriptHash $ hashConwayScript s)) scriptRef

        toPlutusVer Alonzo.PlutusV1 = PlutusVersionV1
        toPlutusVer Alonzo.PlutusV2 = PlutusVersionV2

    validity =
        if isValid
        then Just W.TxScriptValid
        else Just W.TxScriptInvalid
