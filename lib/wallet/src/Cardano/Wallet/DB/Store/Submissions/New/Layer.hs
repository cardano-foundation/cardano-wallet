{-# LANGUAGE RecordWildCards #-}
-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- An implementation of the DBPendingTxs which uses Persistent and SQLite.

module Cardano.Wallet.DB.Store.Submissions.New.Layer
    ( mkDbPendingTxs
    )
    where

import Prelude

import Cardano.Wallet.DB
    ( DBPendingTxs (..) )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..) )
import Cardano.Wallet.DB.Store.Submissions.New.Operations
    ( DeltaTxSubmissions
    , SubmissionMeta (SubmissionMeta, submissionMetaResubmitted)
    , TxSubmissionsStatus
    )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Cardano.Wallet.Primitive.Types.Tx
    ( LocalTxSubmissionStatus (LocalTxSubmissionStatus), SealedTx )
import Cardano.Wallet.Submissions.Submissions
    ( TxStatusMeta (..), transactionsL )
import Cardano.Wallet.Submissions.TxStatus
    ( getTx )
import Control.Lens
    ( (^.) )
import Control.Monad.Except
    ( ExceptT (ExceptT) )
import Data.DBVar
    ( DBVar, readDBVar )
import Data.DeltaMap
    ( DeltaMap )
import Database.Persist.Sql
    ( SqlPersistT )

import qualified Data.Map.Strict as Map

-- TODO: This implementation is not completed / fully tested yet.
mkDbPendingTxs
    :: DBVar (SqlPersistT IO) (DeltaMap WalletId DeltaTxSubmissions)
    -> DBPendingTxs (SqlPersistT IO)
mkDbPendingTxs dbvar = DBPendingTxs
    { putLocalTxSubmission_ = \wid txid tx sl ->
        error "putLocalTxSubmissions not implemented"
    , readLocalTxSubmissionPending_ = \wid -> do
            v <- readDBVar dbvar
            pure $ case Map.lookup wid v of
                Nothing -> [] -- shouldn't we be throwing an exception here ?
                Just sub -> do
                    (_k, x) <- Map.assocs $ sub ^. transactionsL
                    mkLocalTxSubmission x
    , updatePendingTxForExpiry_ = \wid tip -> ExceptT $
        error "updatePendingTxForExpiry_ not implemented"
    , removePendingOrExpiredTx_ = \wid txId ->
        error "removePendingOrExpiredTx_ not implemented"
    }

mkLocalTxSubmission
    :: TxSubmissionsStatus
    -> [LocalTxSubmissionStatus SealedTx]
mkLocalTxSubmission (TxStatusMeta status SubmissionMeta{..})
    = maybe
        []
        (\(TxId txId, sealed) -> pure $
            LocalTxSubmissionStatus (txId) sealed submissionMetaResubmitted
        )
        $ getTx status
