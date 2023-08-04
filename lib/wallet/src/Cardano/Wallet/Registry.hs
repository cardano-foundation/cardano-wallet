{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Registry
  ( -- * Worker Registry
    WorkerRegistry
  , empty
  , lookup
  , register
  , unregister
  , delete

    -- * Worker
  , Worker
  , MkWorker (..)
  , defaultWorkerAfter
  , workerThread
  , workerId
  , workerResource

    -- * Context
  , HasWorkerCtx (..)

    -- * Logging
  , WorkerLog (..)
  , AfterThreadLog (..)
  , traceAfterThread
  )
where

import Cardano.BM.Data.Severity
  ( Severity (..)
  )
import Cardano.BM.Data.Tracer
  ( HasPrivacyAnnotation (..)
  , HasSeverityAnnotation (..)
  )
import Cardano.Wallet
  ( HasLogger
  , logger
  )
import Cardano.Wallet.Logging
  ( LoggedException (..)
  )
import Control.Monad
  ( void
  )
import Control.Monad.IO.Class
  ( MonadIO
  , liftIO
  )
import Control.Tracer
  ( Tracer
  , contramap
  , traceWith
  )
import Data.Foldable
  ( traverse_
  )
import Data.Generics.Internal.VL.Lens
  ( (^.)
  )
import Data.Generics.Labels
  (
  )
import Data.Generics.Product.Typed
  ( HasType
  )
import Data.Kind
  ( Type
  )
import Data.Map.Strict
  ( Map
  )
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Class
  ( ToText (..)
  )
import GHC.Generics
  ( Generic
  )
import UnliftIO.Concurrent
  ( ThreadId
  , forkFinally
  , killThread
  )
import UnliftIO.Exception
  ( SomeException
  , isSyncException
  , withException
  )
import UnliftIO.MVar
  ( MVar
  , modifyMVar_
  , newEmptyMVar
  , newMVar
  , putMVar
  , readMVar
  , takeMVar
  , tryPutMVar
  )
import Prelude hiding
  ( log
  , lookup
  )

{-------------------------------------------------------------------------------
                                Worker Context
-------------------------------------------------------------------------------}

-- | A class to link an existing context to a worker context.
class HasType resource (WorkerCtx ctx) => HasWorkerCtx resource ctx where
  type WorkerCtx ctx :: Type
  type WorkerMsg ctx :: Type
  type WorkerKey ctx :: Type
  hoistResource
    :: resource
    -> (WorkerMsg ctx -> WorkerLog (WorkerKey ctx) (WorkerMsg ctx))
    -> ctx
    -> WorkerCtx ctx

{-------------------------------------------------------------------------------
                                Worker Registry
-------------------------------------------------------------------------------}

-- | A registry to keep track of worker threads and acquired resources.
newtype WorkerRegistry key resource
  = WorkerRegistry (MVar (Map key (Worker key resource)))

-- | Construct a new empty registry
empty
  :: Ord key => IO (WorkerRegistry key resource)
empty =
  WorkerRegistry <$> newMVar mempty

-- | Lookup the registry for a given worker
lookup
  :: (MonadIO m, Ord key)
  => WorkerRegistry key resource
  -> key
  -> m (Maybe (Worker key resource))
lookup (WorkerRegistry mvar) k =
  liftIO (Map.lookup k <$> readMVar mvar)

-- | Register a new worker
insert
  :: Ord key
  => WorkerRegistry key resource
  -> Worker key resource
  -> IO ()
insert (WorkerRegistry mvar) wrk =
  modifyMVar_ mvar (pure . Map.insert (workerId wrk) wrk)

-- | Delete a worker from the registry, but don't cancel the running task.
delete
  :: Ord key
  => WorkerRegistry key resource
  -> key
  -> IO (Maybe (Worker key resource))
delete (WorkerRegistry mvar) k = do
  mWorker <- Map.lookup k <$> readMVar mvar
  modifyMVar_ mvar (pure . Map.delete k)
  pure mWorker

-- | Unregister a worker from the registry, terminating the running task.
unregister
  :: Ord key
  => WorkerRegistry key resource
  -> key
  -> IO ()
unregister registry k =
  delete registry k >>= traverse_ (killThread . workerThread)

{-------------------------------------------------------------------------------
                                    Worker
-------------------------------------------------------------------------------}

-- | A worker which holds and manipulate a particular acquired resource. That
-- resource can be, for example, a handle to a database connection.
data Worker key resource = Worker
  { workerId :: key
  , workerThread :: ThreadId
  , workerResource :: resource
  }
  deriving (Generic)

-- | See 'register'
data MkWorker key resource msg ctx = MkWorker
  { workerBefore :: WorkerCtx ctx -> key -> IO ()
  -- ^ A task to execute before the main worker's task. When creating a
  -- worker, this task is guaranteed to have terminated once 'register'
  -- returns.
  , workerMain :: WorkerCtx ctx -> key -> IO ()
  -- ^ A task for the worker, possibly infinite
  , workerAfter
      :: Tracer IO (WorkerLog key msg)
      -> Either SomeException ()
      -> IO ()
  -- ^ Action to run when the worker exits. It will be run
  --   * when the 'workerMain' action exits (successfully or not)
  --   * if 'workerAcquire' fails
  --   * or if the 'workerBefore' action throws an exception.
  , workerAcquire :: (resource -> IO ()) -> IO ()
  -- ^ A bracket-style factory to acquire a resource
  }

defaultWorkerAfter
  :: Tracer IO (WorkerLog key msg)
  -> Either SomeException a
  -> IO ()
defaultWorkerAfter tr = traceAfterThread (contramap MsgThreadAfter tr)

-- | Register a new worker for a given key.
--
-- A worker maintains an acquired resource. It expects a task as an argument
-- and will terminate as soon as its task is over. In practice, we provide a
-- never-ending task that keeps the worker alive forever.
--
-- Returns 'Nothing' if the worker fails to acquire the necessary resource or
-- terminates unexpectedly before entering its 'main' action.
register
  :: forall resource ctx key msg
   . ( Ord key
     , key ~ WorkerKey ctx
     , msg ~ WorkerMsg ctx
     , HasLogger IO (WorkerLog key msg) ctx
     , HasWorkerCtx resource ctx
     )
  => WorkerRegistry key resource
  -> ctx
  -> key
  -> MkWorker key resource msg ctx
  -> IO (Maybe (Worker key resource))
register registry ctx k (MkWorker before main after acquire) = do
  resourceVar <- newEmptyMVar
  let
    work = acquire $ \resource -> do
      let
        ctx' = hoistResource resource (MsgFromWorker k) ctx
      before ctx' k `withException` (after tr . Left)
      putMVar resourceVar (Just resource)
      main ctx' k
  threadId <- work `forkFinally` cleanup resourceVar
  takeMVar resourceVar >>= traverse (create threadId)
  where
    tr = ctx ^. logger @IO @(WorkerLog key msg)
    create threadId resource = do
      let
        worker =
          Worker
            { workerId = k
            , workerThread = threadId
            , workerResource = resource
            }
      registry `insert` worker
      return worker
    cleanup resourceVar result = do
      void $ registry `delete` k
      void $ tryPutMVar resourceVar Nothing
      after tr result

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

-- | Log messages relating to a registry worker thread.
data WorkerLog key msg
  = MsgThreadAfter AfterThreadLog
  | MsgFromWorker key msg
  deriving (Show, Eq)

instance (ToText key, ToText msg) => ToText (WorkerLog key msg) where
  toText = \case
    MsgThreadAfter msg ->
      "Worker has exited: " <> toText msg
    MsgFromWorker key msg
      | toText key == mempty -> toText msg
      | otherwise -> T.take 8 (toText key) <> ": " <> toText msg

instance HasPrivacyAnnotation (WorkerLog key msg)

instance HasSeverityAnnotation msg => HasSeverityAnnotation (WorkerLog key msg) where
  getSeverityAnnotation = \case
    MsgThreadAfter msg -> getSeverityAnnotation msg
    MsgFromWorker _ msg -> getSeverityAnnotation msg

-- | Log messages describing how a worker thread exits.
data AfterThreadLog
  = MsgThreadFinished
  | MsgThreadCancelled
  | MsgUnhandledException (LoggedException SomeException)
  deriving (Show, Eq)

instance ToText AfterThreadLog where
  toText = \case
    MsgThreadFinished -> "Action has finished"
    MsgThreadCancelled -> "Thread was cancelled"
    MsgUnhandledException err -> "Unhandled exception: " <> toText err

instance HasPrivacyAnnotation AfterThreadLog

instance HasSeverityAnnotation AfterThreadLog where
  getSeverityAnnotation = \case
    MsgThreadFinished -> Notice
    MsgThreadCancelled -> Notice
    MsgUnhandledException _ -> Error

-- | Trace an 'AfterThreadLog' message from a caught exception.
traceAfterThread
  :: Tracer m AfterThreadLog
  -> Either SomeException a
  -> m ()
traceAfterThread tr =
  traceWith tr . \case
    Right _ -> MsgThreadFinished
    Left e ->
      if isSyncException e
        then MsgUnhandledException $ LoggedException e
        else MsgThreadCancelled
