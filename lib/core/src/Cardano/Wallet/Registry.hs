{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Registry
    ( -- * Worker Registry
      WorkerRegistry
    , empty
    , keys
    , lookup
    , register
    , unregister

      -- * Worker
    , Worker
    , MkWorker(..)
    , defaultWorkerAfter
    , workerThread
    , workerId
    , workerResource
    , liftWorker

      -- * Context
    , HasWorkerCtx (..)

      -- * Logging
    , WorkerLog (..)
    , AfterThreadLog (..)
    , traceAfterThread
    ) where

import Prelude hiding
    ( log, lookup )

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Wallet
    ( HasLogger, logger )
import Cardano.Wallet.Logging
    ( LoggedException (..) )
import Control.Monad
    ( void )
import Control.Monad.IO.Unlift
    ( MonadUnliftIO (..), askRunInIO, liftIO )
import Control.Tracer
    ( Tracer, contramap, natTracer, traceWith )
import Data.Foldable
    ( traverse_ )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Labels
    ()
import Data.Generics.Product.Typed
    ( HasType )
import Data.Kind
    ( Type )
import Data.Map.Strict
    ( Map )
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text.Class
    ( ToText (..) )
import GHC.Generics
    ( Generic )
import UnliftIO.Concurrent
    ( ThreadId, forkFinally, killThread )
import UnliftIO.Exception
    ( SomeException, catch, isSyncException, throwIO, withException )
import UnliftIO.MVar
    ( MVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar )

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
newtype WorkerRegistry key resource =
    WorkerRegistry (MVar (Map key (Worker key resource)))

-- | Construct a new empty registry
empty
    :: (MonadUnliftIO m, Ord key)
    => m (WorkerRegistry key resource)
empty =
    WorkerRegistry <$> newMVar mempty

-- | Lookup the registry for a given worker
lookup
    :: (MonadUnliftIO m, Ord key)
    => WorkerRegistry key resource
    -> key
    -> m (Maybe (Worker key resource))
lookup (WorkerRegistry mvar) k =
    liftIO (Map.lookup k <$> readMVar mvar)

-- | Get all registered keys in the registry
keys
    :: MonadUnliftIO m
    => WorkerRegistry key resource
    -> m [key]
keys (WorkerRegistry mvar) =
    Map.keys <$> readMVar mvar

-- | Register a new worker
insert
    :: (MonadUnliftIO m, Ord key)
    => WorkerRegistry key resource
    -> Worker key resource
    -> m ()
insert (WorkerRegistry mvar) wrk =
    modifyMVar_ mvar (pure . Map.insert (workerId wrk) wrk)

-- | Delete a worker from the registry, but don't cancel the running task.
--
delete
    :: (MonadUnliftIO m, Ord key)
    => WorkerRegistry key resource
    -> key
    -> m (Maybe (Worker key resource))
delete (WorkerRegistry mvar) k = do
    mWorker <- Map.lookup k <$> readMVar mvar
    modifyMVar_ mvar (pure . Map.delete k)
    pure mWorker

-- | Unregister a worker from the registry, terminating the running task.
--
unregister
    :: (MonadUnliftIO m, Ord key)
    => WorkerRegistry key resource
    -> key
    -> m ()
unregister registry k =
    delete registry k >>= traverse_ (killThread . workerThread)

{-------------------------------------------------------------------------------
                                    Worker
-------------------------------------------------------------------------------}

-- | A worker which holds and manipulate a paticular acquired resource. That
-- resource can be, for example, a handle to a database connection.
data Worker key resource = Worker
    { workerId :: key
    , workerThread :: ThreadId
    , workerResource :: resource
    } deriving (Generic)

-- | See 'register'
data MkWorker m key resource msg ctx = MkWorker
    { workerBefore :: WorkerCtx ctx -> key -> m ()
        -- ^ A task to execute before the main worker's task. When creating a
        -- worker, this task is guaranteed to have terminated once 'register'
        -- returns.
    , workerMain :: WorkerCtx ctx -> key -> m ()
        -- ^ A task for the worker, possibly infinite
    , workerAfter
        :: Tracer m (WorkerLog key msg) -> Either SomeException () -> m ()
        -- ^ Action to run when the worker exits. It will be run
        --   * when the 'workerMain' action exits (successfully or not)
        --   * if 'workerAcquire' fails
        --   * or if the 'workerBefore' action throws an exception.
    , workerAcquire :: (resource -> m ()) -> m ()
        -- ^ A bracket-style factory to acquire a resource
    }

defaultWorkerAfter
    :: MonadUnliftIO m
    => Tracer m (WorkerLog key msg)
    -> Either SomeException a
    -> m ()
defaultWorkerAfter tr = traceAfterThread (contramap MsgThreadAfter tr)

liftWorker
    :: MonadUnliftIO m
    => MkWorker IO key resource msg ctx
    -> MkWorker m key resource msg ctx
liftWorker MkWorker{..} = MkWorker
    { workerBefore = \ctx -> liftIO . workerBefore ctx
    , workerMain = \ctx -> liftIO . workerMain ctx
    , workerAfter = \tr e -> do
            u <- askRunInIO
            let tr' = Tracer $ \a -> u $ runTracer tr a
            liftIO (workerAfter tr' e)
    , workerAcquire = \action -> do
            u <- askRunInIO
            liftIO (workerAcquire (u . action))
    }

-- | Register a new worker for a given key.
--
-- A worker maintains an acquired resource. It expects a task as an argument
-- and will terminate as soon as its task is over. In practice, we provide a
-- never-ending task that keeps the worker alive forever.
--
-- Any (synchronous) exceptions thrown during 'workerAcquire' setup or
-- 'workerBefore' are rethrown by this function.
--
-- The worker is registered after setup but before the workerMain action, and
-- unregistered after workerMain but before workerAfter. Fixme: there is a race condition because workers can be registered twice while their resource is being acquired.
register
    :: forall m resource ctx key msg.
        ( MonadUnliftIO m
        , Ord key
        , key ~ WorkerKey ctx
        , msg ~ WorkerMsg ctx
        , HasLogger (WorkerLog key msg) ctx
        , HasWorkerCtx resource ctx
        )
    => WorkerRegistry key resource
    -> ctx
    -> key
    -> MkWorker m key resource msg ctx
    -> m (Worker key resource)
register registry ctx k MkWorker{..} = do
    resourceVar <- newEmptyMVar :: m (MVar (Either SomeException resource))
    let work = workerAcquire $ \resource -> do
            let ctx' = hoistResource resource (MsgFromWorker k) ctx
            workerBefore ctx' k
                `withException` (cleanup . Left)
                `catch` (putMVar resourceVar . Left)
            putMVar resourceVar (Right resource)
            workerMain ctx' k
    threadId <- work `forkFinally` \result -> do
        removeWorker
        cleanup result
    takeMVar resourceVar >>= either throwIO (addWorker threadId)
  where
    tr = natTracer liftIO (ctx ^. logger @(WorkerLog key msg))
    addWorker threadId resource = do
        let worker = Worker
                { workerId = k
                , workerThread = threadId
                , workerResource = resource
                }
        registry `insert` worker
        return worker
    removeWorker = void $ registry `delete` k
    cleanup = workerAfter tr


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
traceAfterThread tr = traceWith tr . \case
    Right _ -> MsgThreadFinished
    Left e -> if isSyncException e
        then MsgUnhandledException $ LoggedException e
        else MsgThreadCancelled
