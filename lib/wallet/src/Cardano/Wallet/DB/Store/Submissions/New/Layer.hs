{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- An implementation of the DBPendingTxs which uses Persistent and SQLite.

module Cardano.Wallet.DB.Store.Submissions.New.Layer
    ( mkDbPendingTxs
    )
    where

import Prelude hiding
    ( (.) )

import Cardano.Wallet.DB
    ( DBPendingTxs (..)
    , ErrNoSuchTransaction (..)
    , ErrPutLocalTxSubmission (..)
    , ErrRemoveTx (..)
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..) )
import Cardano.Wallet.DB.Store.Submissions.New.Operations
    ( DeltaTxSubmissions
    , SubmissionMeta (SubmissionMeta, submissionMetaResubmitted)
    , TxSubmissionsStatus
    , submissionMetaFromTxMeta
    )
import Cardano.Wallet.DB.WalletState
    ( ErrNoSuchWallet (..) )
import Cardano.Wallet.Primitive.Types
    ( SlotNo (SlotNo), WalletId )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( LocalTxSubmissionStatus (LocalTxSubmissionStatus), SealedTx )
import Cardano.Wallet.Read.Eras
    ( K (..) )
import Cardano.Wallet.Read.Eras.EraFun
    ( EraFun )
import Cardano.Wallet.Read.Primitive.Tx.Features.Validity
    ( getValidity )
import Cardano.Wallet.Read.Tx.Cardano
    ( anythingFromSealedTx )
import Cardano.Wallet.Read.Tx.Hash
    ( getEraTxHash )
import Cardano.Wallet.Read.Tx.Validity
    ( getEraValidity )
import Cardano.Wallet.Submissions.Operations
    ( Operation (..) )
import Cardano.Wallet.Submissions.Submissions
    ( TxStatusMeta (..), transactions, transactionsL )
import Cardano.Wallet.Submissions.TxStatus
    ( TxStatus (..), getTx, status )
import Cardano.Wallet.Transaction
    ( ValidityIntervalExplicit (invalidHereafter) )
import Control.Category
    ( (.) )
import Control.Lens
    ( to, (^.), (^..) )
import Control.Monad.Except
    ( ExceptT (ExceptT) )
import Data.Bifunctor
    ( second )
import Data.DBVar
    ( DBVar, modifyDBMaybe, readDBVar, updateDBVar )
import Data.DeltaMap
    ( DeltaMap (..) )
import Data.Quantity
    ( Quantity (..) )
import Database.Persist.Sql
    ( SqlPersistT )

import qualified Cardano.Wallet.Read.Tx as Read
import qualified Data.Map.Strict as Map

-- TODO: This implementation is not completed / fully tested yet.
mkDbPendingTxs
    :: DBVar (SqlPersistT IO) (DeltaMap WalletId DeltaTxSubmissions) -- ^
    -> DBPendingTxs (SqlPersistT IO)
mkDbPendingTxs dbvar = DBPendingTxs
    { putLocalTxSubmission_ = \wid txid tx sl -> do
        let errNoSuchWallet = ErrPutLocalTxSubmissionNoSuchWallet $
                ErrNoSuchWallet wid
        ExceptT $ modifyDBMaybe dbvar $ \ws -> do
            case Map.lookup wid ws of
                Nothing -> (Nothing, Left errNoSuchWallet)
                Just _  ->
                    let
                        delta = Just
                            $ Adjust wid
                            $ AddSubmission sl (TxId txid, tx)
                            $ error "pls pass meta to putLocalTxSubmission!"
                    in  (delta, Right ())

    , addTxSubmission_ =  \wid (_ , meta, sealed) resubmitted -> do
        let (expiry, txId) = extractPendingData sealed
        updateDBVar dbvar
            $ Adjust wid
            $ AddSubmission expiry (txId , sealed)
            $ submissionMetaFromTxMeta meta resubmitted

    , resubmitTx_ = \wid (TxId -> txId) _sealed resubmitted -> do
        submissions <- readDBVar dbvar
        sequence_ $ do
            walletSubmissions <-
                Map.lookup wid submissions
            (TxStatusMeta datas meta) <-
                Map.lookup txId $ walletSubmissions ^. transactionsL
            (_, sealed) <- getTx datas
            let (expiry,_) = extractPendingData sealed
            pure $ do
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
                Just xs -> xs ^.. transactionsL . traverse . to (fmap snd)

    , readLocalTxSubmissionPending_ = \wid -> do
            v <- readDBVar dbvar
            pure $ case Map.lookup wid v of
                Nothing -> []
                Just sub -> do
                    (_k, x) <- Map.assocs $ sub ^. transactionsL
                    mkLocalTxSubmission x

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
            updateDBVar dbvar
                $ Adjust wid $ RollBack slot

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

extractPendingData :: SealedTx -> (SlotNo, TxId)
extractPendingData sealed = let
    value :: EraFun Read.Tx (K a) -> a
    value f = anythingFromSealedTx f sealed

    txId = TxId $ Hash $ value getEraTxHash
    validity = value $ getValidity . getEraValidity

    Quantity expiry = maybe (Quantity 0) invalidHereafter validity

    in (SlotNo expiry, txId)
