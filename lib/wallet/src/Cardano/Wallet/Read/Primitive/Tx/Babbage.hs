{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Read.Primitive.Tx.Babbage
  ( fromBabbageTx
  )
where

import Cardano.Api
  ( BabbageEra
  )
import Cardano.Api.Shelley qualified as Cardano
import Cardano.Ledger.Alonzo
  ( AlonzoScript
  )
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Alonzo.Tx qualified as Alonzo
import Cardano.Ledger.Api
  ( StandardCrypto
  , addrTxWitsL
  , auxDataTxL
  , bodyTxL
  , bootAddrTxWitsL
  , certsTxBodyL
  , collateralInputsTxBodyL
  , collateralReturnTxBodyL
  , feeTxBodyL
  , inputsTxBodyL
  , isValidTxL
  , mintTxBodyL
  , outputsTxBodyL
  , referenceInputsTxBodyL
  , scriptTxWitsL
  , vldtTxBodyL
  , witsTxL
  )
import Cardano.Ledger.Api.Scripts qualified as Ledger
import Cardano.Ledger.BaseTypes qualified as SL
import Cardano.Ledger.Core qualified as Core
import Cardano.Ledger.Language qualified as Language
import Cardano.Ledger.Mary.Value qualified as SL
import Cardano.Wallet.Primitive.Types qualified as W
import Cardano.Wallet.Primitive.Types.Hash qualified as W
import Cardano.Wallet.Primitive.Types.TokenPolicy
  ( TokenPolicyId
  )
import Cardano.Wallet.Primitive.Types.Tx qualified as W
import Cardano.Wallet.Primitive.Types.Tx.TxIn
  ( TxIn (..)
  )
import Cardano.Wallet.Read.Eras
  ( babbage
  , inject
  )
import Cardano.Wallet.Read.Primitive.Tx.Features.Certificates
  ( anyEraCerts
  )
import Cardano.Wallet.Read.Primitive.Tx.Features.Inputs
  ( fromShelleyTxIn
  )
import Cardano.Wallet.Read.Primitive.Tx.Features.Metadata
  ( fromBabbageMetadata
  )
import Cardano.Wallet.Read.Primitive.Tx.Features.Mint
  ( babbageMint
  , fromLedgerScriptHash
  )
import Cardano.Wallet.Read.Primitive.Tx.Features.Outputs
  ( fromBabbageTxOut
  )
import Cardano.Wallet.Read.Primitive.Tx.Features.Validity
  ( afterShelleyValidityInterval
  )
import Cardano.Wallet.Read.Primitive.Tx.Features.Withdrawals
  ( fromLedgerWithdrawals
  )
import Cardano.Wallet.Read.Tx
  ( Tx (..)
  )
import Cardano.Wallet.Read.Tx.CBOR
  ( renderTxToCBOR
  )
import Cardano.Wallet.Read.Tx.Hash
  ( shelleyTxHash
  )
import Cardano.Wallet.Read.Tx.Withdrawals
  ( shelleyWithdrawals
  )
import Cardano.Wallet.Shelley.Compatibility.Ledger
  ( toWalletScript
  , toWalletTokenPolicyId
  )
import Cardano.Wallet.Shelley.Compatibility.Ledger qualified as Ledger
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
import Cardano.Wallet.Write.Tx
  ( BabbageTxOut
  , StandardBabbage
  )
import Control.Lens
  ( folded
  , (<&>)
  , (^.)
  , (^..)
  )
import Data.List qualified as L
import Data.Map
  ( Map
  )
import Data.Map.Strict qualified as Map
import Data.Maybe.Strict
  ( strictMaybeToMaybe
  )
import Data.Set qualified as Set
import Data.Word
  ( Word32
  )
import Prelude

fromBabbageTx
  :: Alonzo.AlonzoTx (Cardano.ShelleyLedgerEra BabbageEra)
  -> WitnessCountCtx
  -> ( W.Tx
     , [W.Certificate]
     , TokenMapWithScripts
     , TokenMapWithScripts
     , Maybe ValidityIntervalExplicit
     , WitnessCount
     )
fromBabbageTx tx witCtx =
  ( W.Tx
      { txId
      , txCBOR =
          Just $ renderTxToCBOR $ inject babbage $ Tx tx
      , fee =
          Just $ Ledger.toWalletCoin $ tx ^. bodyTxL . feeTxBodyL
      , resolvedInputs =
          (,Nothing) . fromShelleyTxIn
            <$> tx ^.. bodyTxL . inputsTxBodyL . folded
      , resolvedCollateralInputs =
          (,Nothing) . fromShelleyTxIn
            <$> tx ^.. bodyTxL . collateralInputsTxBodyL . folded
      , outputs =
          fst . fromBabbageTxOut <$> tx ^.. bodyTxL . outputsTxBodyL . folded
      , collateralOutput =
          strictMaybeToMaybe
            $ fst . fromBabbageTxOut <$> tx ^. bodyTxL . collateralReturnTxBodyL
      , withdrawals =
          fromLedgerWithdrawals . shelleyWithdrawals $ tx
      , metadata =
          fromBabbageMetadata <$> SL.strictMaybeToMaybe (tx ^. auxDataTxL)
      , scriptValidity =
          Just $ case tx ^. isValidTxL of
            Alonzo.IsValid True -> W.TxScriptValid
            Alonzo.IsValid False -> W.TxScriptInvalid
      }
  , anyEraCerts $ tx ^. bodyTxL . certsTxBodyL
  , assetsToMint
  , assetsToBurn
  , Just $ afterShelleyValidityInterval $ tx ^. bodyTxL . vldtTxBodyL
  , WitnessCount
      (fromIntegral $ Set.size $ tx ^. witsTxL . addrTxWitsL)
      (Map.elems $ Map.union anyScriptsFromWits anyScriptsFromTxOuts)
      (fromIntegral $ Set.size $ tx ^. witsTxL . bootAddrTxWitsL)
  )
  where
    txId = W.Hash $ shelleyTxHash tx

    anyScriptsFromTxOuts :: Map TokenPolicyId AnyExplicitScript
    anyScriptsFromTxOuts =
      Map.fromList
        [ fromLedgerToAnyScript ledgerScript
        | Just ledgerScript <-
            L.zipWith
              scriptWithHashIx
              [0 ..]
              (tx ^.. bodyTxL . outputsTxBodyL . folded)
        ]
      where
        scriptWithHashIx
          :: Word32
          -> BabbageTxOut StandardBabbage
          -> Maybe
              ( ScriptReference
              , Ledger.ScriptHash StandardCrypto
              , AlonzoScript StandardBabbage
              )
        scriptWithHashIx ix txout =
          snd (fromBabbageTxOut txout) <&> \script ->
            ( ViaReferenceInput (ReferenceInput (TxIn txId ix))
            , hashBabbageScript script
            , script
            )

    anyScriptsFromWits :: Map TokenPolicyId AnyExplicitScript
    anyScriptsFromWits =
      Map.fromList
        [ fromLedgerToAnyScript (ViaSpending, scriptH, script)
        | (scriptH, script) <- Map.toList (tx ^. witsTxL . scriptTxWitsL)
        ]

    (assetsToMint, assetsToBurn) =
      babbageMint
        (tx ^. bodyTxL . referenceInputsTxBodyL)
        (tx ^. bodyTxL . mintTxBodyL)
        (tx ^. witsTxL)

    fromLedgerToAnyScript
      :: ( ScriptReference
         , Ledger.ScriptHash StandardCrypto
         , AlonzoScript StandardBabbage
         )
      -> (TokenPolicyId, AnyExplicitScript)
    fromLedgerToAnyScript (scriptRef, scriptH, script) =
      (toWalletTokenPolicyId (SL.PolicyID scriptH), toAnyScript script)
      where
        toAnyScript = \case
          Alonzo.TimelockScript timelockScript ->
            NativeExplicitScript
              (toWalletScript (toKeyRole witCtx) timelockScript)
              scriptRef
          Alonzo.PlutusScript ver _ ->
            PlutusExplicitScript
              ( PlutusScriptInfo
                  (toPlutusVer ver)
                  (fromLedgerScriptHash $ hashBabbageScript script)
              )
              scriptRef

        toPlutusVer Language.PlutusV1 = PlutusVersionV1
        toPlutusVer Language.PlutusV2 = PlutusVersionV2
        toPlutusVer Language.PlutusV3 = PlutusVersionV3

    hashBabbageScript = Core.hashScript @(Cardano.ShelleyLedgerEra BabbageEra)
