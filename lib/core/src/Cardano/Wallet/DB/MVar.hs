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
    , ErrNoSuchWallet (..)
    , ErrRemovePendingTx (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    )
import Cardano.Wallet.DB.Model
    ( Database
    , Err (..)
    , ErrErasePendingTx (..)
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
    , mPutPrivateKey
    , mPutProtocolParameters
    , mPutTxHistory
    , mPutWalletMeta
    , mReadCheckpoint
    , mReadDelegationRewardBalance
    , mReadPrivateKey
    , mReadProtocolParameters
    , mReadTxHistory
    , mReadWalletMeta
    , mRemovePendingTx
    , mRemoveWallet
    , mRollbackTo
    , mUpdatePendingTx
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..) )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter )
import Cardano.Wallet.Primitive.Types
    ( Hash, SortOrder (..), TransactionInfo (..), WalletId, wholeRange )
import Control.Concurrent.MVar
    ( MVar, modifyMVar, newMVar, withMVar )
import Control.DeepSeq
    ( NFData, deepseq )
import Control.Exception
    ( Exception, throwIO )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Data.Functor.Identity
    ( Identity (..) )

-- | Instantiate a new in-memory "database" layer that simply stores data in
-- a local MVar. Data vanishes if the software is shut down.
newDBLayer
    :: forall s k. (NFData (k 'RootK XPrv), NFData s)
    => TimeInterpreter Identity
    -> IO (DBLayer IO s k)
newDBLayer timeInterpreter = do
    lock <- newMVar ()
    db <- newMVar (emptyDatabase :: Database (PrimaryKey WalletId) s (k 'RootK XPrv, Hash "encryption"))
    return $ DBLayer

        {-----------------------------------------------------------------------
                                      Wallets
        -----------------------------------------------------------------------}

        { initializeWallet = \pk cp meta txs txp -> ExceptT $ do
            cp `deepseq` meta `deepseq`
                alterDB errWalletAlreadyExists db (mInitializeWallet pk cp meta txs txp)

        , removeWallet = ExceptT . alterDB errNoSuchWallet db . mRemoveWallet

        , listWallets = readDB db mListWallets

        {-----------------------------------------------------------------------
                                    Checkpoints
        -----------------------------------------------------------------------}

        , putCheckpoint = \pk cp -> ExceptT $ do
            cp `deepseq` alterDB errNoSuchWallet db (mPutCheckpoint pk cp)

        , readCheckpoint = readDB db . mReadCheckpoint

        , listCheckpoints = readDB db . mListCheckpoints

        , rollbackTo = \pk pt -> ExceptT $
            alterDB errNoSuchWallet db (mRollbackTo pk pt)

        , prune = \_ -> error "MVar.prune: not implemented"

        {-----------------------------------------------------------------------
                                   Wallet Metadata
        -----------------------------------------------------------------------}

        , putWalletMeta = \pk meta -> ExceptT $ do
            meta `deepseq` alterDB errNoSuchWallet db (mPutWalletMeta pk meta)

        , readWalletMeta = readDB db . mReadWalletMeta timeInterpreter

        , putDelegationCertificate = \pk cert sl -> ExceptT $ do
            cert `deepseq` sl `deepseq`
                alterDB errNoSuchWallet db (mPutDelegationCertificate pk cert sl)

        , isStakeKeyRegistered =
            ExceptT . alterDB errNoSuchWallet db . mIsStakeKeyRegistered

        {-----------------------------------------------------------------------
                                     Tx History
        -----------------------------------------------------------------------}

        , putTxHistory = \pk txh -> ExceptT $ do
            txh `deepseq` alterDB errNoSuchWallet db (mPutTxHistory pk txh)

        , readTxHistory = \pk minWithdrawal order range mstatus ->
            readDB db $
                mReadTxHistory
                    timeInterpreter
                    pk
                    minWithdrawal
                    order
                    range
                    mstatus

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

        , putPrivateKey = \pk prv -> ExceptT $ do
            prv `deepseq` alterDB errNoSuchWallet db (mPutPrivateKey pk prv)

        , readPrivateKey = readDB db . mReadPrivateKey

        {-----------------------------------------------------------------------
                                       Pending Tx
        -----------------------------------------------------------------------}

        , updatePendingTx = \pk tip -> ExceptT $ do
            alterDB errNoSuchWallet db (mUpdatePendingTx pk tip)

        , removePendingTx = \pk tid -> ExceptT $ do
            alterDB errCannotRemovePendingTx db (mRemovePendingTx pk tid)

        {-----------------------------------------------------------------------
                                 Protocol Parameters
        -----------------------------------------------------------------------}

        , putProtocolParameters = \pk txp -> ExceptT $ do
            txp `deepseq`
                alterDB errNoSuchWallet db (mPutProtocolParameters pk txp)

        , readProtocolParameters = readDB db . mReadProtocolParameters

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
    :: (Err (PrimaryKey WalletId) -> Maybe err)
    -- ^ Error type converter
    -> MVar (Database (PrimaryKey WalletId) s xprv)
    -- ^ The database variable
    -> ModelOp (PrimaryKey WalletId) s xprv a
    -- ^ Operation to run on the database
    -> IO (Either err a)
alterDB convertErr db op = modifyMVar db (bubble . op)
  where
    bubble (Left e, db') = case convertErr e of
        Just e' -> pure (db', Left e')
        Nothing -> throwIO $ MVarDBError e
    bubble (Right a, db') = pure (db', Right a)

-- | Run a query operation on the model database. Any error results are turned
-- into a runtime exception.
readDB
    :: MVar (Database (PrimaryKey WalletId) s xprv)
    -- ^ The database variable
    -> ModelOp (PrimaryKey WalletId) s xprv a
    -- ^ Operation to run on the database
    -> IO a
readDB db op = alterDB Just db op >>= either (throwIO . MVarDBError) pure

errNoSuchWallet :: Err (PrimaryKey WalletId) -> Maybe ErrNoSuchWallet
errNoSuchWallet (NoSuchWallet (PrimaryKey wid)) = Just (ErrNoSuchWallet wid)
errNoSuchWallet _ = Nothing

errCannotRemovePendingTx :: Err (PrimaryKey WalletId) -> Maybe ErrRemovePendingTx
errCannotRemovePendingTx (CannotRemovePendingTx (ErrErasePendingTxNoSuchWallet (PrimaryKey wid))) =
    Just (ErrRemovePendingTxNoSuchWallet (ErrNoSuchWallet wid))
errCannotRemovePendingTx (CannotRemovePendingTx (ErrErasePendingTxNoTx tid)) =
    Just (ErrRemovePendingTxNoSuchTransaction tid)
errCannotRemovePendingTx (CannotRemovePendingTx (ErrErasePendingTxNoPendingTx tid)) =
    Just (ErrRemovePendingTxTransactionNoMorePending tid)
errCannotRemovePendingTx _ = Nothing

errWalletAlreadyExists
    :: Err (PrimaryKey WalletId)
    -> Maybe ErrWalletAlreadyExists
errWalletAlreadyExists (WalletAlreadyExists (PrimaryKey wid)) =
    Just (ErrWalletAlreadyExists wid)
errWalletAlreadyExists _ = Nothing

-- | Error which happens when model returns an unexpected value.
newtype MVarDBError = MVarDBError (Err (PrimaryKey WalletId))
    deriving (Show)

instance Exception MVarDBError
