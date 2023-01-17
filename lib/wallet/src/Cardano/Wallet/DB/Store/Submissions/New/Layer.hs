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
import Cardano.Wallet.DB.Store.Submissions.New.Operations
    ( DeltaTxSubmissions )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Control.Monad.Except
    ( ExceptT (ExceptT) )
import Data.DBVar
    ( DBVar )
import Data.DeltaMap
    ( DeltaMap )
import Database.Persist.Sql
    ( SqlPersistT )

mkDbPendingTxs
    :: DBVar (SqlPersistT IO) (DeltaMap WalletId DeltaTxSubmissions)
    -> DBPendingTxs stm
mkDbPendingTxs dbvar = DBPendingTxs
    { putLocalTxSubmission_ = \wid txid tx sl ->
        error "putLocalTxSubmissions not implemented"
    , readLocalTxSubmissionPending_
        = error "readLocalTxSubmissionPending_ not implemented"
    , updatePendingTxForExpiry_ = \wid tip -> ExceptT $
        error "updatePendingTxForExpiry_ not implemented"
    , removePendingOrExpiredTx_ = \wid txId ->
        error "removePendingOrExpiredTx_ not implemented"
    }
