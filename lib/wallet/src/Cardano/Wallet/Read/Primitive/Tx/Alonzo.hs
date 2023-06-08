{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Read.Primitive.Tx.Alonzo
    ( fromAlonzoTx
    )
    where

import Prelude

import Cardano.Api
    ( AlonzoEra )
import Cardano.Ledger.Era
    ( Era (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenPolicyId )
import Cardano.Wallet.Read.Eras
    ( alonzo, inject )
import Cardano.Wallet.Read.Primitive.Tx.Features.Certificates
    ( anyEraCerts )
import Cardano.Wallet.Read.Primitive.Tx.Features.Fee
    ( fromShelleyCoin )
import Cardano.Wallet.Read.Primitive.Tx.Features.Inputs
    ( fromShelleyTxIn )
import Cardano.Wallet.Read.Primitive.Tx.Features.Metadata
    ( fromAlonzoMetadata )
import Cardano.Wallet.Read.Primitive.Tx.Features.Mint
    ( alonzoMint, fromLedgerScriptHash )
import Cardano.Wallet.Read.Primitive.Tx.Features.Outputs
    ( fromAlonzoTxOut )
import Cardano.Wallet.Read.Primitive.Tx.Features.Validity
    ( afterShelleyValidityInterval )
import Cardano.Wallet.Read.Primitive.Tx.Features.Withdrawals
    ( fromShelleyWdrl )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.CBOR
    ( renderTxToCBOR )
import Cardano.Wallet.Read.Tx.Hash
    ( shelleyTxHash )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( toWalletScript, toWalletTokenPolicyId )
import Cardano.Wallet.Transaction
    ( AnyExplicitScript (..)
    , PlutusScriptInfo (..)
    , PlutusVersion (..)
    , ScriptReference (..)
    , TokenMapWithScripts (..)
    , ValidityIntervalExplicit (..)
    , WitnessCount (..)
    , WitnessCountCtx
    , toKeyRole
    )
import Data.Foldable
    ( toList )
import Data.Map.Strict
    ( Map )
import Ouroboros.Consensus.Cardano.Block
    ( StandardAlonzo )

import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Core as SL.Core
import qualified Cardano.Ledger.Mary.Value as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

fromAlonzoTx
    :: Alonzo.AlonzoTx (Cardano.ShelleyLedgerEra AlonzoEra)
    -> WitnessCountCtx
    -> ( W.Tx
       , [W.Certificate]
       , TokenMapWithScripts
       , TokenMapWithScripts
       , Maybe ValidityIntervalExplicit
       , WitnessCount
       )
fromAlonzoTx tx@(Alonzo.AlonzoTx bod wits (Alonzo.IsValid isValid) aux) witCtx =
    ( W.Tx
        { txId =
            W.Hash $ shelleyTxHash tx
        , txCBOR =
            Just $ renderTxToCBOR $ inject alonzo $ Tx tx
        , fee =
            Just $ fromShelleyCoin fee
        , resolvedInputs =
            map ((,Nothing) . fromShelleyTxIn) (toList ins)
        , resolvedCollateralInputs =
            map ((,Nothing) . fromShelleyTxIn) (toList collateral)
        , outputs =
            map fromAlonzoTxOut (toList outs)
        , collateralOutput =
            -- Collateral outputs are not supported in Alonzo.
            Nothing
        , withdrawals =
            fromShelleyWdrl wdrls
        , metadata =
            fromAlonzoMetadata<$> SL.strictMaybeToMaybe aux
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
    Alonzo.AlonzoTxBody
        ins
        collateral
        outs
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
    (assetsToMint, assetsToBurn) = alonzoMint mint wits
    scriptMap = fromAlonzoScriptMap $ Alonzo.txscripts' wits

    countWits = WitnessCount
        (fromIntegral $ Set.size $ Alonzo.txwitsVKey' wits)
        (Map.elems scriptMap)
        (fromIntegral $ Set.size $ Alonzo.txwitsBoot' wits)

    hashAlonzoScript =
        fromLedgerScriptHash .
        SL.Core.hashScript @(Cardano.ShelleyLedgerEra AlonzoEra)

    fromAlonzoScriptMap
        :: Map
            (SL.ScriptHash (Crypto StandardAlonzo))
            (SL.Core.Script StandardAlonzo)
        -> Map TokenPolicyId AnyExplicitScript
    fromAlonzoScriptMap =
        Map.map toAnyScript .
        Map.mapKeys (toWalletTokenPolicyId . SL.PolicyID)
      where
        toAnyScript (Alonzo.TimelockScript script) =
            NativeExplicitScript (toWalletScript (toKeyRole witCtx) script) ViaSpending
        toAnyScript s@(Alonzo.PlutusScript ver _) =
            PlutusExplicitScript (PlutusScriptInfo (toPlutusVer ver)
                          (hashAlonzoScript s)) ViaSpending

        toPlutusVer Alonzo.PlutusV1 = PlutusVersionV1
        toPlutusVer Alonzo.PlutusV2 = PlutusVersionV2

    validity =
        if isValid
        then Just W.TxScriptValid
        else Just W.TxScriptInvalid
