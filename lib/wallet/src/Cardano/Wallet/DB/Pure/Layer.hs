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

module Cardano.Wallet.DB.Pure.Layer
    ( newDBLayer
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Wallet.DB
    ( DBLayer (..), ErrWalletAlreadyExists (..), ErrWalletNotInitialized (..) )
import Cardano.Wallet.DB.Pure.Implementation
    ( Database
    , Err (..)
    , ModelOp
    , emptyDatabase
    , mCheckWallet
    , mGetWalletId
    , mInitializeWallet
    , mIsStakeKeyRegistered
    , mListCheckpoints
    , mPutCheckpoint
    , mPutDelegationCertificate
    , mPutDelegationRewardBalance
    , mPutPrivateKey
    , mPutTxHistory
    , mPutWalletMeta
    , mReadCheckpoint
    , mReadDelegationRewardBalance
    , mReadGenesisParameters
    , mReadPrivateKey
    , mReadTxHistory
    , mReadWalletMeta
    , mRollbackTo
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

        , listWallets = pure <$> readDB db mGetWalletId

        {-----------------------------------------------------------------------
                                    Checkpoints
        -----------------------------------------------------------------------}
        , walletsDB = error "MVar.walletsDB: not implemented"

        , putCheckpoint = \_pk cp -> ExceptT $
            alterDB errWalletNotInitialized db $
            mPutCheckpoint cp

        , readCheckpoint = const $ readDB db mReadCheckpoint

        , listCheckpoints = const $ readDB db mListCheckpoints

        , rollbackTo = \_pk pt -> ExceptT $
            alterDB errWalletNotInitialized db $
            mRollbackTo pt

        , prune = \_ _ -> error "MVar.prune: not implemented"

        {-----------------------------------------------------------------------
                                   Wallet Metadata
        -----------------------------------------------------------------------}

        , putWalletMeta = \_pk meta -> ExceptT $
            alterDB errWalletNotInitialized db $
            mPutWalletMeta meta

        , readWalletMeta = const $ readDB db $ mReadWalletMeta timeInterpreter

        , putDelegationCertificate = \_pk cert sl -> ExceptT $
            alterDB errWalletNotInitialized db $
            mPutDelegationCertificate cert sl

        , isStakeKeyRegistered =
            const $ ExceptT . alterDB errWalletNotInitialized db $ mIsStakeKeyRegistered

        {-----------------------------------------------------------------------
                                     Tx History
        -----------------------------------------------------------------------}

        , putTxHistory = \_pk txh -> ExceptT $
            alterDB errWalletNotInitialized db $
            mPutTxHistory txh

        , readTransactions = \_pk minWithdrawal order range mstatus _mlimit ->
            readDB db $
                mReadTxHistory
                    timeInterpreter
                    minWithdrawal
                    order
                    range
                    mstatus

        -- TODO: shift implementation to mGetTx
        , getTx = \_pk tid -> ExceptT $
            alterDB errWalletNotInitialized db (mCheckWallet) >>= \case
                Left err -> pure $ Left err
                Right _ -> do
                    txInfos <- readDB db
                        $ mReadTxHistory
                            timeInterpreter
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

        , putPrivateKey = \_pk prv -> ExceptT $
            alterDB errWalletNotInitialized db $
            mPutPrivateKey prv

        , readPrivateKey = const $ readDB db mReadPrivateKey

        {-----------------------------------------------------------------------
                                       Pending Tx
        -----------------------------------------------------------------------}

        , addTxSubmission =
            error "addTxSubmission not tested in State Machine tests"

        , readLocalTxSubmissionPending =
            error "readLocalTxSubmissionPending not tested in State Machine tests"

        , resubmitTx =
            error "resubmitTx not tested in State Machine tests"

        , rollForwardTxSubmissions =
            error "rollForwardTxSubmissions not tested in State Machine tests"

        , removePendingOrExpiredTx = error
            "removePendingOrExpiredTx not implemented in State Machine tests"

        {-----------------------------------------------------------------------
                                 Protocol Parameters
        -----------------------------------------------------------------------}

        , readGenesisParameters = const $ readDB db mReadGenesisParameters

        {-----------------------------------------------------------------------
                                 Delegation Rewards
        -----------------------------------------------------------------------}

        , putDelegationRewardBalance = \_pk amt -> ExceptT $
            alterDB errWalletNotInitialized db (mPutDelegationRewardBalance amt)

        , readDelegationRewardBalance =
            const $ readDB db mReadDelegationRewardBalance

        {-----------------------------------------------------------------------
                                      Execution
        -----------------------------------------------------------------------}

        , atomically = \action -> withMVar lock $ \() -> action
        }

-- | Apply an operation to the model database, then update the mutable variable.
alterDB
    :: MonadUnliftIO m
    => (Err -> Maybe err)
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

noWallet :: a
noWallet = error "wallet not initialized"

errWalletNotInitialized :: Err -> Maybe ErrWalletNotInitialized
errWalletNotInitialized WalletNotInitialized = Just ErrWalletNotInitialized
errWalletNotInitialized _ = Nothing

errWalletAlreadyExists
    :: Err
    -> Maybe ErrWalletAlreadyExists
errWalletAlreadyExists WalletAlreadyInitialized  =
    Just (ErrWalletAlreadyExists noWallet)
errWalletAlreadyExists _ = Nothing

-- | Error which happens when model returns an unexpected value.
newtype MVarDBError = MVarDBError Err
    deriving (Show)

instance Exception MVarDBError
