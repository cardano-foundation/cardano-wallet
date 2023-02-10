{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- An implementation of the DBPendingTxs which uses Persistent and SQLite.

module Cardano.Wallet.DB.Store.Submissions.Layer
    ( mkDbPendingTxs
    , mkLocalTxSubmission)
    where

import Prelude hiding
    ( (.) )

import Cardano.Wallet.DB
    ( DBPendingTxs (..), ErrNoSuchTransaction (..), ErrRemoveTx (..) )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..) )
import Cardano.Wallet.DB.Store.Submissions.Operations
    ( DeltaTxSubmissions
    , SubmissionMeta (SubmissionMeta, submissionMetaResubmitted)
    , TxSubmissionsStatus
    , submissionMetaFromTxMeta
    )
import Cardano.Wallet.DB.WalletState
    ( ErrNoSuchWallet (..) )
import Cardano.Wallet.Primitive.Types
    ( SlotNo (SlotNo), WalletId )
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
    ( (^.), (^..) )
import Control.Monad.Except
    ( ExceptT (ExceptT) )
import Data.Bifunctor
    ( second )
import Data.DBVar
    ( DBVar, modifyDBMaybe, readDBVar, updateDBVar )
import Data.DeltaMap
    ( DeltaMap (..) )
import Database.Persist.Sql
    ( SqlPersistT )

import qualified Data.Map.Strict as Map

mkDbPendingTxs
    :: DBVar (SqlPersistT IO) (DeltaMap WalletId DeltaTxSubmissions)
    -> DBPendingTxs (SqlPersistT IO)
mkDbPendingTxs dbvar = DBPendingTxs
    { emptyTxSubmissions_ = \wid ->
        updateDBVar dbvar
            $ Insert wid $ mkEmpty 0
    , addTxSubmission_ =  \wid BuiltTx{..} resubmitted -> do
        let txId = TxId $ builtTx ^. #txId
            expiry = case builtTxMeta ^. #expiry of
                Nothing -> SlotNo maxBound
                Just slot -> slot
        updateDBVar dbvar
            $ Adjust wid
            $ AddSubmission expiry (txId, builtSealedTx)
            $ submissionMetaFromTxMeta builtTxMeta resubmitted

    , resubmitTx_ = \wid (TxId -> txId) _sealed resubmitted -> do
        submissions <- readDBVar dbvar
        sequence_ $ do
            walletSubmissions <-
                Map.lookup wid submissions
            (TxStatusMeta datas meta) <-
                Map.lookup txId $ walletSubmissions ^. transactionsL
            (_, sealed) <- getTx datas
            pure $ case expirySlot datas of
                Nothing -> pure ()
                Just expiry -> do
                    updateDBVar dbvar
                        $ Adjust wid
                        $ Forget txId
                    updateDBVar dbvar
                        $ Adjust wid
                        $ AddSubmission expiry (txId, sealed)
                        $ meta{submissionMetaResubmitted = resubmitted}

    , getInSubmissionTransactions_ = \wid -> do
            submissions <- readDBVar dbvar
            pure $ case Map.lookup wid submissions of
                Nothing  -> []
                Just xs -> xs ^.. transactionsL . traverse

    , rollForwardTxSubmissions_ = \wid tip txs ->
        updateDBVar dbvar
            $ Adjust wid $ RollForward tip (second TxId <$> txs)

    , removePendingOrExpiredTx_ = \wid txId -> do
        let errNoSuchWallet = ErrRemoveTxNoSuchWallet
                $ ErrNoSuchWallet wid
            errNoTx = ErrRemoveTxNoSuchTransaction
                $ ErrNoSuchTransaction wid txId
            errInLedger = ErrRemoveTxAlreadyInLedger txId
        ExceptT $ modifyDBMaybe dbvar $ \ws -> do
            case Map.lookup wid ws of
                Nothing -> (Nothing, Left errNoSuchWallet)
                Just sub ->
                    case status (TxId txId) (transactions sub) of
                        Unknown -> (Nothing, Left errNoTx)
                        InLedger{} -> (Nothing, Left errInLedger)
                        _ -> (Just $ Adjust wid $ Forget (TxId txId), Right ())

    ,   rollBackSubmissions_ = \wid slot ->
            updateDBVar dbvar $ Adjust wid $ RollBack slot

    ,   pruneByFinality_ = \wid slot ->
            updateDBVar dbvar $ Adjust wid $ Prune slot
    }

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
