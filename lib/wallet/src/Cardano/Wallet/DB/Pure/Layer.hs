{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
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
    ( withBootDBLayer
    , throwErrorReadDB
    ) where

import Prelude

import Cardano.Wallet.DB
    ( DBLayer (..)
    , DBLayerParams
    )
import Cardano.Wallet.DB.Pure.Implementation
    ( Database
    , Err (..)
    , ModelOp
    , mInitializeWallet
    , mListCheckpoints
    , mPutTxHistory
    , mReadCheckpoint
    , mReadGenesisParameters
    , mReadTxHistory
    , mRollbackTo
    )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter
    )
import Cardano.Wallet.Primitive.Types
    ( SortOrder (..)
    , WalletId
    )
import Cardano.Wallet.Primitive.Types.Range
    ( wholeRange
    )
import Cardano.Wallet.Primitive.Types.Tx.TransactionInfo
    ( TransactionInfo (..)
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newMVar
    , withMVar
    )
import Control.Monad
    ( join
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Data.Functor.Identity
    ( Identity (..)
    )
import Data.Maybe
    ( fromMaybe
    )
import UnliftIO.Exception
    ( Exception
    , throwIO
    )

-- | Instantiate a new in-memory "database" layer that simply stores data in
-- a local MVar. Data vanishes if the software is shut down.
withBootDBLayer
    :: forall m s
     . ( MonadIO m
       )
    => TimeInterpreter Identity
    -> WalletId
    -> DBLayerParams s
    -> (DBLayer IO s -> m ())
    -> m ()
withBootDBLayer timeInterpreter wid params k = do
    lock <- liftIO $ newMVar ()
    db <- liftIO $ newMVar $ mInitializeWallet wid params
    k $ DBLayer

        {-----------------------------------------------------------------------
                                      Wallets
        -----------------------------------------------------------------------}

        { walletId_ = wid

        {-----------------------------------------------------------------------
                                    Checkpoints
        -----------------------------------------------------------------------}
        , walletState = error "MVar.walletState: not implemented"
        , transactionsStore = error "MVar.transactionsStore: not implemented"

        , readCheckpoint = throwErrorReadDB db mReadCheckpoint

        , listCheckpoints = fromMaybe [] <$> readDBMaybe db mListCheckpoints

        , rollbackTo = noErrorAlterDB db
            . mRollbackTo

        {-----------------------------------------------------------------------
                                     Tx History
        -----------------------------------------------------------------------}

        , putTxHistory = noErrorAlterDB db . mPutTxHistory

        , readTransactions = \minWithdrawal order range mstatus _mlimit maddress ->
            fmap (fromMaybe []) $
            readDBMaybe db $
                mReadTxHistory
                    timeInterpreter
                    minWithdrawal
                    order
                    range
                    mstatus
                    maddress

        -- TODO: shift implementation to mGetTx
        , getTx = \tid -> do
                txInfos <- fmap (fromMaybe [])
                    $ readDBMaybe db
                    $ mReadTxHistory
                        timeInterpreter
                        Nothing
                        Descending
                        wholeRange
                        Nothing
                        Nothing
                let txPresent (TransactionInfo{..}) = txInfoId == tid
                case filter txPresent txInfos of
                    [] -> pure Nothing
                    t:_ -> pure $ Just t

        {-----------------------------------------------------------------------
                                       Pending Tx
        -----------------------------------------------------------------------}

        , resubmitTx =
            error "resubmitTx not tested in State Machine tests"

        , rollForwardTxSubmissions =
            error "rollForwardTxSubmissions not tested in State Machine tests"

        {-----------------------------------------------------------------------
                                 Protocol Parameters
        -----------------------------------------------------------------------}

        , readGenesisParameters = join <$> readDBMaybe db mReadGenesisParameters

        {-----------------------------------------------------------------------
                                      Execution
        -----------------------------------------------------------------------}

        , getSchemaVersion =
            error "getSchemaVersion not tested in State Machine tests"

        , atomically = \action -> withMVar lock $ \() -> action
        }

-- | Read the database, but return 'Nothing' if the operation fails.
readDBMaybe :: MonadIO m
    => MVar (Database WalletId s xprv)
    -> ModelOp WalletId s xprv a
    -> m (Maybe a)
readDBMaybe db = fmap (either (const Nothing) Just) . readDB db

-- | Apply an operation to the model database, then update the mutable variable.
-- Failures are converted to 'Err' using the provided function.
-- Failures that cannot be converted are rethrown as 'MVarDBError'.
alterDB
    :: MonadIO m
    => (Err -> Maybe err)
    -- ^ Error type converter
    -> MVar (Database WalletId s xprv)
    -- ^ The database variable
    -> ModelOp WalletId s xprv a
    -- ^ Operation to run on the database
    -> m (Either err a)
alterDB convertErr db op = liftIO $ modifyMVar db (bubble . op)
  where
    bubble (Left e, !db') = case convertErr e of
        Just e' -> pure (db', Left e')
        Nothing -> throwIO $ MVarDBError e
    bubble (Right a, !db') = pure (db', Right a)

noErrorAlterDB
    :: MonadIO m
    => MVar (Database WalletId s xprv)
    -> ModelOp WalletId s xprv a
    -> m a
noErrorAlterDB db op = do
    r <- alterDB (const Nothing) db op
    case r of
        Left e -> throwIO $ MVarDBError e
        Right a -> pure a

throwErrorReadDB
    :: MonadIO m
    => MVar (Database WalletId s xprv)
    -> ModelOp WalletId s xprv b
    -> m b
throwErrorReadDB db op = do
    mr <- readDB db op
    case mr of
        Left e -> throwIO $ MVarDBError e
        Right r -> pure r

-- | Run a query operation on the model database.
readDB
    :: MonadIO m
    => MVar (Database WalletId s xprv)
    -- ^ The database variable
    -> ModelOp WalletId s xprv a
    -- ^ Operation to run on the database
    -> m (Either Err a)
readDB = alterDB Just -- >>= either (throwIO . MVarDBError) pure

-- | Error which happens when model returns an unexpected value.
newtype MVarDBError = MVarDBError Err
    deriving (Show)

instance Exception MVarDBError
