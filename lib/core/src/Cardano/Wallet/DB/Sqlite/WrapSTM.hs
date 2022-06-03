{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.DB.Sqlite.WrapSTM () where

import Prelude

import Control.Applicative
    ( Alternative )
import Control.Monad.Class.MonadSTM
    ( MonadSTM (..) )
import Control.Monad.Reader
    ( MonadPlus, ReaderT, liftIO )
import Database.Persist.Sql
    ( SqlBackend )

import qualified Control.Concurrent.STM as STM

{-------------------------------------------------------------------------------
                     Provide ReaderT instance for MonadSTM
-------------------------------------------------------------------------------}

instance MonadSTM (ReaderT SqlBackend IO) where
    type STM (ReaderT SqlBackend IO) = WrapSTM
    atomically = liftIO . STM.atomically . unWrapSTM

    type TVar (ReaderT SqlBackend IO) = TVar IO
    type TMVar (ReaderT SqlBackend IO) = TMVar IO
    type TBQueue (ReaderT SqlBackend IO) = TBQueue IO
    type TQueue (ReaderT SqlBackend IO) = TQueue IO

    newTVar        =       WrapSTM . STM.newTVar
    readTVar       =       WrapSTM . STM.readTVar
    writeTVar      = \v -> WrapSTM . STM.writeTVar v
    retry          = WrapSTM STM.retry
    orElse         = \(WrapSTM a) (WrapSTM b) -> WrapSTM (STM.orElse a b)
    modifyTVar     = \v -> WrapSTM . STM.modifyTVar v
    modifyTVar'    = \v -> WrapSTM . STM.modifyTVar' v
    stateTVar      = \v -> WrapSTM . STM.stateTVar v
    swapTVar       = \v -> WrapSTM . STM.swapTVar v
    check          =       WrapSTM . STM.check
    newTMVar       =       WrapSTM . STM.newTMVar
    newEmptyTMVar  =       WrapSTM STM.newEmptyTMVar
    takeTMVar      =       WrapSTM . STM.takeTMVar
    tryTakeTMVar   =       WrapSTM . STM.tryTakeTMVar
    putTMVar       = \v -> WrapSTM . STM.putTMVar v
    tryPutTMVar    = \v -> WrapSTM . STM.tryPutTMVar v
    readTMVar      =       WrapSTM . STM.readTMVar
    tryReadTMVar   =       WrapSTM . STM.tryReadTMVar
    swapTMVar      = \v -> WrapSTM . STM.swapTMVar v
    isEmptyTMVar   =       WrapSTM . STM.isEmptyTMVar
    newTQueue      =       WrapSTM STM.newTQueue
    readTQueue     =       WrapSTM . STM.readTQueue
    tryReadTQueue  =       WrapSTM . STM.tryReadTQueue
    peekTQueue     =       WrapSTM . STM.peekTQueue
    tryPeekTQueue  =       WrapSTM . STM.tryPeekTQueue
    flushTBQueue   =       WrapSTM . STM.flushTBQueue
    writeTQueue    = \q -> WrapSTM . STM.writeTQueue q
    isEmptyTQueue  =       WrapSTM . STM.isEmptyTQueue
    newTBQueue     =       WrapSTM . STM.newTBQueue
    readTBQueue    =       WrapSTM . STM.readTBQueue
    tryReadTBQueue =       WrapSTM . STM.tryReadTBQueue
    peekTBQueue    =       WrapSTM . STM.peekTBQueue
    tryPeekTBQueue =       WrapSTM . STM.tryPeekTBQueue
    writeTBQueue   = \q -> WrapSTM . STM.writeTBQueue q
    lengthTBQueue  =       WrapSTM . STM.lengthTBQueue
    isEmptyTBQueue =       WrapSTM . STM.isEmptyTBQueue
    isFullTBQueue  =       WrapSTM . STM.isFullTBQueue

    newTVarIO       = liftIO . STM.newTVarIO
    readTVarIO      = liftIO . STM.readTVarIO
    newTMVarIO      = liftIO . STM.newTMVarIO
    newEmptyTMVarIO = liftIO STM.newEmptyTMVarIO
    newTQueueIO     = liftIO STM.newTQueueIO
    newTBQueueIO    = liftIO . STM.newTBQueueIO

-- | MonadSTM is an injective typeclass, so we need a unique newtype to target.
newtype WrapSTM a = WrapSTM { unWrapSTM :: STM.STM a }
    deriving (Applicative, Functor, Monad)

deriving instance MonadPlus WrapSTM
deriving instance Alternative WrapSTM
