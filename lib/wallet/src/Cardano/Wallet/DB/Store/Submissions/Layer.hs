{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Access to the submissions store.

module Cardano.Wallet.DB.Store.Submissions.Layer
    ( mkLocalTxSubmission
    , emptyTxSubmissions
    , resubmitTx
    , getInSubmissionTransactions
    , rollForwardTxSubmissions
    , removePendingOrExpiredTx
    , rollBackSubmissions
    , pruneByFinality
    , addTxSubmission
    , getInSubmissionTransaction
    )
    where

import Prelude hiding
    ( (.) )

import Cardano.Wallet.DB.Errors
    ( ErrNoSuchTransaction (..), ErrRemoveTx (..) )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..) )
import Cardano.Wallet.DB.Store.Submissions.Operations
    ( DeltaTxSubmissions
    , SubmissionMeta (SubmissionMeta, submissionMetaResubmitted)
    , TxSubmissions
    , TxSubmissionsStatus
    , submissionMetaFromTxMeta
    )
import Cardano.Wallet.Primitive.Types
    ( SlotNo (SlotNo) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( LocalTxSubmissionStatus (LocalTxSubmissionStatus), SealedTx )
import Cardano.Wallet.Submissions.Operations
    ( Operation (..) )
import Cardano.Wallet.Submissions.Submissions
    ( TxStatusMeta (..), mkEmpty, transactions, transactionsL )
import Cardano.Wallet.Submissions.TxStatus
    ( TxStatus (..), expirySlot, getTx, status )
import Cardano.Wallet.Transaction.Built
    ( BuiltTx (..) )
import Control.Category
    ( (.) )
import Control.Lens
    ( ix, (^.), (^..), (^?) )
import Data.Bifunctor
    ( second )
import Data.Maybe
    ( fromMaybe )

import qualified Data.Map.Strict as Map

emptyTxSubmissions :: TxSubmissions
emptyTxSubmissions = mkEmpty 0

addTxSubmission
    :: BuiltTx
    -> SlotNo
    -> DeltaTxSubmissions
addTxSubmission BuiltTx{..} resubmitted =
    let txId = TxId $ builtTx ^. #txId
        expiry = case builtTxMeta ^. #expiry of
            Nothing -> SlotNo maxBound
            Just slot -> slot
    in AddSubmission expiry (txId, builtSealedTx)
        $ submissionMetaFromTxMeta builtTxMeta resubmitted

resubmitTx
    :: Hash "Tx"
    -> SlotNo
    -> TxSubmissions
    -> [DeltaTxSubmissions]
resubmitTx (TxId -> txId) resubmitted walletSubmissions
    = fromMaybe [] $ do
        (TxStatusMeta datas meta) <-
            Map.lookup txId $ walletSubmissions ^. transactionsL
        (_, sealed) <- getTx datas
        pure $ case expirySlot datas of
            Nothing -> []
            Just expiry ->
                [ AddSubmission expiry (txId, sealed)
                    $ meta{submissionMetaResubmitted = resubmitted}
                , Forget txId
                ]


getInSubmissionTransactions :: TxSubmissions -> [TxSubmissionsStatus]
getInSubmissionTransactions submissions
    = submissions ^.. transactionsL . traverse

getInSubmissionTransaction :: Hash "Tx" -> TxSubmissions -> Maybe TxSubmissionsStatus
getInSubmissionTransaction txId submissions
    = submissions ^? transactionsL . ix (TxId txId)

rollForwardTxSubmissions
    :: SlotNo -> [(SlotNo, Hash "Tx")] -> DeltaTxSubmissions
rollForwardTxSubmissions tip txs = RollForward tip (second TxId <$> txs)

removePendingOrExpiredTx :: TxSubmissions -> Hash "Tx"
    -> Either ErrRemoveTx DeltaTxSubmissions
removePendingOrExpiredTx walletSubmissions txId = do
    let
        errNoTx = ErrRemoveTxNoSuchTransaction $ ErrNoSuchTransaction txId
        errInLedger = ErrRemoveTxAlreadyInLedger txId
    case status (TxId txId) (transactions walletSubmissions) of
        Unknown -> Left errNoTx
        InLedger{} -> Left errInLedger
        _ -> Right $ Forget (TxId txId)

rollBackSubmissions :: SlotNo -> DeltaTxSubmissions
rollBackSubmissions = RollBack

pruneByFinality :: SlotNo -> DeltaTxSubmissions
pruneByFinality = Prune

mkLocalTxSubmission
    :: TxSubmissionsStatus
    -> [LocalTxSubmissionStatus SealedTx]
mkLocalTxSubmission (TxStatusMeta status' SubmissionMeta{..})
    = maybe
        []
        (\(TxId txId, sealed) -> pure $
            LocalTxSubmissionStatus (txId) sealed submissionMetaResubmitted
        )
        $ getTx status'
