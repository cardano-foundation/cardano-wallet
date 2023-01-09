{-# LANGUAGE GADTs #-}
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

import Cardano.Wallet
    ( ErrNoSuchWallet (..) )
import Cardano.Wallet.DB
    ( DBPendingTxs (..)
    , ErrPutLocalTxSubmission (..)
    , ErrRemoveTx (ErrRemoveTxNoSuchWallet)
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..) )
import Cardano.Wallet.DB.Store.Submissions.New.Operations
    ( DeltaTxSubmissions
    , SubmissionMeta (SubmissionMeta, submissionMetaResubmitted)
    , TxSubmissions
    , TxSubmissionsStatus
    )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Cardano.Wallet.Primitive.Types.Tx
    ( LocalTxSubmissionStatus (LocalTxSubmissionStatus), SealedTx )
import Cardano.Wallet.Submissions.Operations
    ( Operation (..) )
import Cardano.Wallet.Submissions.Submissions
    ( TxStatusMeta (..), transactionsL )
import Cardano.Wallet.Submissions.TxStatus
    ( getTx )
import Control.Lens
    ( (^.) )
import Control.Monad.Except
    ( ExceptT (ExceptT) )
import Control.Monad.Trans.Except
    ( withExceptT )
import Data.Bifunctor
    ( second )
import Data.DBVar
    ( DBVar, modifyDBMaybe, readDBVar )
import Data.DeltaMap
    ( DeltaMap (..) )
import Data.Foldable
    ( toList )
import Database.Persist.Sql
    ( SqlPersistT )

import qualified Data.Map.Strict as Map

catchWalletMissing
    :: Monad m
    => DBVar m (DeltaMap WalletId DeltaTxSubmissions)
    -> WalletId
    -> (TxSubmissions ->
        (Maybe (DeltaMap WalletId DeltaTxSubmissions)
            , Either ErrNoSuchWallet a)
        )
    -> ExceptT ErrNoSuchWallet m a
catchWalletMissing dbvar wid f
    = ExceptT
    $ modifyDBMaybe dbvar
    $ \ws -> maybe (Nothing, Left $ ErrNoSuchWallet wid) f (Map.lookup wid ws)

mkDbPendingTxs ::
  DBVar (SqlPersistT IO) (DeltaMap WalletId DeltaTxSubmissions) ->
  DBPendingTxs (SqlPersistT IO)
mkDbPendingTxs dbvar = let
    missingWallet = catchWalletMissing dbvar
    in DBPendingTxs
    {   putLocalTxSubmission_ =
            \wid txid tx sl -> withExceptT ErrPutLocalTxSubmissionNoSuchWallet
                $ missingWallet wid $
                    \_ ->
                        let delta = Just $
                                Adjust wid $
                                AddSubmission sl (TxId txid, tx) $ error "pls pass meta to putLocalTxSubmission!"
                        in (delta, Right ())
    ,   readLocalTxSubmissionPending_ = \wid -> do
            v <- readDBVar dbvar
            pure $ case Map.lookup wid v of
                Nothing -> [] -- shouldn't we be throwing an exception here ?
                Just sub -> do
                    x <- toList $ sub ^. transactionsL
                    mkLocalTxSubmission x
    ,   updatePendingTxForExpiry_ =
            \wid tip xs -> missingWallet wid $
                \_ ->
                let delta =
                        Just $
                        Adjust wid $
                            RollForward tip (second TxId <$> xs)
                in (delta, Right ())
    ,   removePendingOrExpiredTx_ =
            \wid txId ->
                withExceptT ErrRemoveTxNoSuchWallet
                    $ missingWallet wid
                    $ \_ -> (Just $ Adjust wid $ Forget (TxId txId), Right ())
    ,   rollBackSubmissions_ =
            \wid slot -> missingWallet wid
                $ \_ -> (Just $ Adjust wid $ RollBack slot, Right ())
    ,   pruneByFinality_ =
            \wid slot -> missingWallet wid
                $ \_ -> (Just $ Adjust wid $ Prune slot, Right ())
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
