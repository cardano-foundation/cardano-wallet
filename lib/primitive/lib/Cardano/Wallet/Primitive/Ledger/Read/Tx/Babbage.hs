{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Primitive.Ledger.Read.Tx.Babbage
    ( fromBabbageTx
    , fromBabbageTx'
    )
    where

import Prelude

import Cardano.Ledger.Api
    ( Babbage
    , ScriptHash
    , StandardCrypto
    , addrTxWitsL
    , auxDataTxL
    , bodyTxL
    , bootAddrTxWitsL
    , collateralInputsTxBodyL
    , collateralReturnTxBodyL
    , feeTxBodyL
    , hashScript
    , inputsTxBodyL
    , isValidTxL
    , mintTxBodyL
    , outputsTxBodyL
    , referenceInputsTxBodyL
    , scriptTxWitsL
    , vldtTxBodyL
    , witsTxL
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Certificates
    ( anyEraCerts
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Inputs
    ( fromShelleyTxIn
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Metadata
    ( fromBabbageMetadata
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Mint
    ( babbageMint
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Outputs
    ( fromBabbageTxOut
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Scripts
    ( babbageAnyExplicitScript
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Validity
    ( afterShelleyValidityInterval
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Withdrawals
    ( fromLedgerWithdrawals
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId
    ( TokenPolicyId
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (txId)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..)
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

import Cardano.Ledger.Babbage
    ( AlonzoScript
    , BabbageTxOut
    )
import Cardano.Wallet.Primitive.Types.AnyExplicitScripts
    ( AnyExplicitScript (..)
    )
import Cardano.Wallet.Primitive.Types.TokenMapWithScripts
    ( ReferenceInput (ReferenceInput)
    , ScriptReference (ViaReferenceInput, ViaSpending)
    , TokenMapWithScripts
    )
import Cardano.Wallet.Primitive.Types.ValidityIntervalExplicit
    ( ValidityIntervalExplicit
    )
import Cardano.Wallet.Primitive.Types.WitnessCount
    ( WitnessCount (WitnessCount)
    , WitnessCountCtx
    )
import Control.Lens
    ( folded
    , (<&>)
    , (^.)
    , (^..)
    )
import Data.Map
    ( Map
    )
import Data.Maybe.Strict
    ( strictMaybeToMaybe
    )
import Data.Word
    ( Word32
    )

import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Wallet.Primitive.Ledger.Convert as Ledger
import qualified Cardano.Wallet.Primitive.Types.Certificates as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Read as Read
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

fromBabbageTx
    :: Alonzo.AlonzoTx Babbage
    -> WitnessCountCtx
    -> ( W.Tx
       , [W.Certificate]
       , TokenMapWithScripts
       , TokenMapWithScripts
       , Maybe ValidityIntervalExplicit
       , WitnessCount
       )
fromBabbageTx tx witCtx =
    ( tx'
    , Read.unK . Read.babbageFun anyEraCerts $ Read.Tx tx
    , assetsToMint
    , assetsToBurn
    , Just $ afterShelleyValidityInterval $ tx ^. bodyTxL.vldtTxBodyL
    , WitnessCount
        (fromIntegral $ Set.size $ tx ^. witsTxL.addrTxWitsL)
        (Map.elems $ Map.union anyScriptsFromWits anyScriptsFromTxOuts)
        (fromIntegral $ Set.size $ tx ^. witsTxL.bootAddrTxWitsL)
    )
  where
    tx' = fromBabbageTx' tx
    txId' = txId tx'

    anyScriptsFromTxOuts :: Map TokenPolicyId AnyExplicitScript
    anyScriptsFromTxOuts =
        Map.fromList
            [ babbageAnyExplicitScript witCtx ledgerScript
            | Just ledgerScript <- L.zipWith scriptWithHashIx
                [0..] (tx ^.. bodyTxL.outputsTxBodyL.folded)
            ]
      where
        scriptWithHashIx
            :: Word32
            -> BabbageTxOut Babbage
            -> Maybe
                ( ScriptReference
                , ScriptHash StandardCrypto
                , AlonzoScript Babbage
                )
        scriptWithHashIx ix txout =
            snd (fromBabbageTxOut txout) <&> \script ->
                ( ViaReferenceInput (ReferenceInput (TxIn txId' ix))
                , hashScript @Babbage script
                , script
                )

    anyScriptsFromWits :: Map TokenPolicyId AnyExplicitScript
    anyScriptsFromWits =
        Map.fromList
            [ babbageAnyExplicitScript witCtx (ViaSpending, scriptH, script)
            | (scriptH, script) <- Map.toList (tx ^. witsTxL.scriptTxWitsL)
            ]

    (assetsToMint, assetsToBurn) =
        babbageMint
            (tx ^. bodyTxL.referenceInputsTxBodyL)
            (tx ^. bodyTxL.mintTxBodyL)
            (tx ^. witsTxL)

fromBabbageTx' :: Alonzo.AlonzoTx Babbage -> W.Tx
fromBabbageTx' tx =
    W.Tx
        { txId = W.Hash $ shelleyTxHash tx
        , txCBOR =
            Just $ renderTxToCBOR $ Read.inject Read.babbage $ Read.Tx tx
        , fee =
            Just $ Ledger.toWalletCoin $ tx ^. bodyTxL.feeTxBodyL
        , resolvedInputs =
            (,Nothing) . fromShelleyTxIn
                <$> tx ^.. bodyTxL.inputsTxBodyL.folded
        , resolvedCollateralInputs =
            (,Nothing) . fromShelleyTxIn
                <$> tx ^.. bodyTxL.collateralInputsTxBodyL.folded
        , outputs =
            fst . fromBabbageTxOut <$> tx ^.. bodyTxL.outputsTxBodyL.folded
        , collateralOutput =
            strictMaybeToMaybe $
                fst . fromBabbageTxOut <$> tx ^. bodyTxL.collateralReturnTxBodyL
        , withdrawals =
            fromLedgerWithdrawals . shelleyWithdrawals $ tx
        , metadata =
            fromBabbageMetadata <$> SL.strictMaybeToMaybe (tx ^. auxDataTxL)
        , scriptValidity =
            Just $ case tx ^. isValidTxL of
                Alonzo.IsValid True -> W.TxScriptValid
                Alonzo.IsValid False -> W.TxScriptInvalid
        }
