{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Dummy implementation of the database-layer, using 'MVar'. This may be good
-- for testing to compare with an implementation on a real data store, or to use
-- when compiling the wallet for targets which don't have SQLite.

module Cardano.Wallet.DB.MVar
    ( newDBLayer
    ) where

import Prelude

import Cardano.Wallet.DB
    ( DBLayer (..)
    , ErrNoSuchWallet (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    )
import Cardano.Wallet.DB.Model
    ( Database
    , Err (..)
    , ModelOp
    , emptyDatabase
    , mCreateWallet
    , mListWallets
    , mPutCheckpoint
    , mPutPrivateKey
    , mPutTxHistory
    , mPutWalletMeta
    , mReadCheckpoint
    , mReadPrivateKey
    , mReadTxHistory
    , mReadWalletMeta
    , mRemoveWallet
    , mRollbackTo
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), XPrv )
import Cardano.Wallet.Primitive.Types
    ( DefineTx, Hash, WalletId )
import Control.Concurrent.MVar
    ( MVar, modifyMVar, newMVar, withMVar )
import Control.DeepSeq
    ( NFData, deepseq )
import Control.Exception
    ( Exception, throwIO )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT )

-- | Instantiate a new in-memory "database" layer that simply stores data in
-- a local MVar. Data vanishes if the software is shut down.
newDBLayer
    :: forall s t k. (NFData (k 'RootK XPrv), DefineTx t)
    => IO (DBLayer IO s t k)
newDBLayer = do
    lock <- newMVar ()
    db <- newMVar (emptyDatabase :: Database (PrimaryKey WalletId) s t (k 'RootK XPrv, Hash "encryption"))
    return $ DBLayer

        {-----------------------------------------------------------------------
                                      Wallets
        -----------------------------------------------------------------------}

        { createWallet = \pk cp meta txs -> ExceptT $ do
            cp `deepseq` meta `deepseq`
                alterDB errWalletAlreadyExists db (mCreateWallet pk cp meta txs)

        , removeWallet = ExceptT . alterDB errNoSuchWallet db . mRemoveWallet

        , listWallets = readDB db mListWallets

        {-----------------------------------------------------------------------
                                    Checkpoints
        -----------------------------------------------------------------------}

        , putCheckpoint = \pk cp -> ExceptT $ do
            cp `deepseq` alterDB errNoSuchWallet db (mPutCheckpoint pk cp)

        , readCheckpoint = readDB db . mReadCheckpoint

        , rollbackTo = \pk pt -> ExceptT $
            alterDB errNoSuchWallet  db (mRollbackTo pk pt)

        , prune = \_ -> error "MVar.prune: not implemented"

        {-----------------------------------------------------------------------
                                   Wallet Metadata
        -----------------------------------------------------------------------}

        , putWalletMeta = \pk meta -> ExceptT $ do
            meta `deepseq` alterDB errNoSuchWallet db (mPutWalletMeta pk meta)

        , readWalletMeta = readDB db . mReadWalletMeta

        {-----------------------------------------------------------------------
                                     Tx History
        -----------------------------------------------------------------------}

        , putTxHistory = \pk txh -> ExceptT $ do
            txh `deepseq` alterDB errNoSuchWallet db (mPutTxHistory pk txh)

        , readTxHistory = \pk order range mstatus ->
                readDB db (mReadTxHistory pk order range mstatus)

        {-----------------------------------------------------------------------
                                       Keystore
        -----------------------------------------------------------------------}

        , putPrivateKey = \pk prv -> ExceptT $ do
            prv `deepseq` alterDB errNoSuchWallet db (mPutPrivateKey pk prv)

        , readPrivateKey = readDB db . mReadPrivateKey

        {-----------------------------------------------------------------------
                                       Lock
        -----------------------------------------------------------------------}

        , withLock = \action ->
            ExceptT $ withMVar lock $ \() -> runExceptT action
        }

-- | Apply an operation to the model database, then update the mutable variable.
alterDB
    :: (Err (PrimaryKey WalletId) -> Maybe err)
    -- ^ Error type converter
    -> MVar (Database (PrimaryKey WalletId) s t xprv)
    -- ^ The database variable
    -> ModelOp (PrimaryKey WalletId) s t xprv a
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
    :: MVar (Database (PrimaryKey WalletId) s t xprv)
    -- ^ The database variable
    -> ModelOp (PrimaryKey WalletId) s t xprv a
    -- ^ Operation to run on the database
    -> IO a
readDB db op = alterDB Just db op >>= either (throwIO . MVarDBError) pure

errNoSuchWallet :: Err (PrimaryKey WalletId) -> Maybe ErrNoSuchWallet
errNoSuchWallet (NoSuchWallet (PrimaryKey wid)) = Just (ErrNoSuchWallet wid)
errNoSuchWallet _ = Nothing

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
