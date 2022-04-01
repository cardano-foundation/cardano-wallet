{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Dummy implementation of the database-layer, using 'MVar'. This may be good
-- for testing to compare with an implementation on a real data store, or to use
-- when compiling the wallet for targets which don't have SQLite.

module Cardano.Wallet.DB.MVar
    ( newDBLayer
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Wallet.DB
    ( DBLayer (..)
    , ErrNoSuchTransaction (..)
    , ErrNoSuchWallet (..)
    , ErrPutLocalTxSubmission (..)
    , ErrRemoveTx (..)
    , ErrWalletAlreadyExists (..)
    )
import Cardano.Wallet.DB.Model
    ( Database
    , Err (..)
    , ModelOp
    , emptyDatabase
    , mCheckWallet
    , mInitializeWallet
    , mIsStakeKeyRegistered
    , mListCheckpoints
    , mListWallets
    , mPutCheckpoint
    , mPutDelegationCertificate
    , mPutDelegationRewardBalance
    , mPutLocalTxSubmission
    , mPutPrivateKey
    , mPutTxHistory
    , mPutWalletMeta
    , mReadCheckpoint
    , mReadDelegationRewardBalance
    , mReadGenesisParameters
    , mReadLocalTxSubmissionPending
    , mReadPrivateKey
    , mReadTxHistory
    , mReadWalletMeta
    , mRemovePendingOrExpiredTx
    , mRemoveWallet
    , mRollbackTo
    , mUpdatePendingTxForExpiry
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..) )
import Cardano.Wallet.Primitive.Passphrase
    ( PassphraseHash )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter )
import Cardano.Wallet.Primitive.Types
    ( SortOrder (..), WalletId, wholeRange )
import Cardano.Wallet.Primitive.Types.Tx
    ( TransactionInfo (..) )
import Control.Monad.IO.Unlift
    ( MonadUnliftIO (..) )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Data.Functor.Identity
    ( Identity (..) )
import UnliftIO.Exception
    ( Exception, throwIO )
import UnliftIO.MVar
    ( MVar, modifyMVar, newMVar, withMVar )

-- | Instantiate a new in-memory "database" layer that simply stores data in
-- a local MVar. Data vanishes if the software is shut down.
newDBLayer
    :: forall m s k.
       ( MonadUnliftIO m
       , MonadFail m )
    => TimeInterpreter Identity
    -> m (DBLayer m s k)
newDBLayer timeInterpreter = do
    lock <- newMVar ()
    db <- newMVar (emptyDatabase :: Database WalletId s (k 'RootK XPrv, PassphraseHash))
    return $ DBLayer

        {-----------------------------------------------------------------------
                                      Wallets
        -----------------------------------------------------------------------}

        { initializeWallet = \pk cp meta txs gp -> ExceptT $
                alterDB errWalletAlreadyExists db $
                mInitializeWallet pk cp meta txs gp

        , removeWallet = ExceptT . alterDB errNoSuchWallet db . mRemoveWallet

        , listWallets = readDB db mListWallets

        {-----------------------------------------------------------------------
                                    Checkpoints
        -----------------------------------------------------------------------}
        , walletsDB = error "MVar.walletsDB: not implemented"

        , putCheckpoint = \pk cp -> ExceptT $
            alterDB errNoSuchWallet db $
            mPutCheckpoint pk cp

        , readCheckpoint = readDB db . mReadCheckpoint

        , listCheckpoints = readDB db . mListCheckpoints

        , rollbackTo = \pk pt -> ExceptT $
            alterDB errNoSuchWallet db $
            mRollbackTo pk pt

        , prune = \_ _ -> error "MVar.prune: not implemented"

        {-----------------------------------------------------------------------
                                   Wallet Metadata
        -----------------------------------------------------------------------}

        , putWalletMeta = \pk meta -> ExceptT $
            alterDB errNoSuchWallet db $
            mPutWalletMeta pk meta

        , readWalletMeta = readDB db . mReadWalletMeta timeInterpreter

        , putDelegationCertificate = \pk cert sl -> ExceptT $
            alterDB errNoSuchWallet db $
            mPutDelegationCertificate pk cert sl

        , isStakeKeyRegistered =
            ExceptT . alterDB errNoSuchWallet db . mIsStakeKeyRegistered

        {-----------------------------------------------------------------------
                                     Tx History
        -----------------------------------------------------------------------}

        , putTxHistory = \pk txh -> ExceptT $
            alterDB errNoSuchWallet db $
            mPutTxHistory pk txh

        , readTxHistory = \pk minWithdrawal order range mstatus ->
            readDB db $
                mReadTxHistory
                    timeInterpreter
                    pk
                    minWithdrawal
                    order
                    range
                    mstatus

        -- TODO: shift implementation to mGetTx
        , getTx = \pk tid -> ExceptT $
            alterDB errNoSuchWallet db (mCheckWallet pk) >>= \case
                Left err -> pure $ Left err
                Right _ -> do
                    txInfos <- readDB db
                        $ mReadTxHistory
                            timeInterpreter
                            pk
                            Nothing
                            Descending
                            wholeRange
                            Nothing
                    let txPresent (TransactionInfo{..}) = txInfoId == tid
                    case filter txPresent txInfos of
                        [] -> pure $ Right Nothing
                        t:_ -> pure $ Right $ Just t

        {-----------------------------------------------------------------------
                                       Keystore
        -----------------------------------------------------------------------}

        , putPrivateKey = \pk prv -> ExceptT $
            alterDB errNoSuchWallet db $
            mPutPrivateKey pk prv

        , readPrivateKey = readDB db . mReadPrivateKey

        {-----------------------------------------------------------------------
                                       Pending Tx
        -----------------------------------------------------------------------}

        , putLocalTxSubmission = \pk txid tx sl -> ExceptT $
            alterDB (fmap ErrPutLocalTxSubmissionNoSuchWallet . errNoSuchWallet) db $
            mPutLocalTxSubmission pk txid tx sl

        , readLocalTxSubmissionPending =
            readDB db . mReadLocalTxSubmissionPending

        , updatePendingTxForExpiry = \pk tip -> ExceptT $
            alterDB errNoSuchWallet db $
            mUpdatePendingTxForExpiry pk tip

        , removePendingOrExpiredTx = \pk tid -> ExceptT $
            alterDB errCannotRemovePendingTx db $
            mRemovePendingOrExpiredTx pk tid

        {-----------------------------------------------------------------------
                                 Protocol Parameters
        -----------------------------------------------------------------------}

        , readGenesisParameters = readDB db . mReadGenesisParameters

        {-----------------------------------------------------------------------
                                 Delegation Rewards
        -----------------------------------------------------------------------}

        , putDelegationRewardBalance = \pk amt -> ExceptT $
            alterDB errNoSuchWallet db (mPutDelegationRewardBalance pk amt)

        , readDelegationRewardBalance =
            readDB db . mReadDelegationRewardBalance

        {-----------------------------------------------------------------------
                                      Execution
        -----------------------------------------------------------------------}

        , atomically = \action -> withMVar lock $ \() -> action
        }

-- | Apply an operation to the model database, then update the mutable variable.
alterDB
    :: MonadUnliftIO m
    => (Err WalletId -> Maybe err)
    -- ^ Error type converter
    -> MVar (Database WalletId s xprv)
    -- ^ The database variable
    -> ModelOp WalletId s xprv a
    -- ^ Operation to run on the database
    -> m (Either err a)
alterDB convertErr db op = modifyMVar db (bubble . op)
  where
    bubble (Left e, !db') = case convertErr e of
        Just e' -> pure (db', Left e')
        Nothing -> throwIO $ MVarDBError e
    bubble (Right a, !db') = pure (db', Right a)

-- | Run a query operation on the model database. Any error results are turned
-- into a runtime exception.
readDB
    :: MonadUnliftIO m
    => MVar (Database WalletId s xprv)
    -- ^ The database variable
    -> ModelOp WalletId s xprv a
    -- ^ Operation to run on the database
    -> m a
readDB db op = alterDB Just db op >>= either (throwIO . MVarDBError) pure

errNoSuchWallet :: Err WalletId -> Maybe ErrNoSuchWallet
errNoSuchWallet (NoSuchWallet wid) = Just (ErrNoSuchWallet wid)
errNoSuchWallet _ = Nothing

errCannotRemovePendingTx :: Err WalletId -> Maybe ErrRemoveTx
errCannotRemovePendingTx (NoSuchWallet wid) =
    Just (ErrRemoveTxNoSuchWallet (ErrNoSuchWallet wid))
errCannotRemovePendingTx (NoSuchTx wid tid) =
    Just (ErrRemoveTxNoSuchTransaction (ErrNoSuchTransaction wid tid))
errCannotRemovePendingTx (CantRemoveTxInLedger _ tid) =
    Just (ErrRemoveTxAlreadyInLedger tid)
errCannotRemovePendingTx _ = Nothing

errWalletAlreadyExists
    :: Err WalletId
    -> Maybe ErrWalletAlreadyExists
errWalletAlreadyExists (WalletAlreadyExists wid) =
    Just (ErrWalletAlreadyExists wid)
errWalletAlreadyExists _ = Nothing

-- | Error which happens when model returns an unexpected value.
newtype MVarDBError = MVarDBError (Err WalletId)
    deriving (Show)

instance Exception MVarDBError
