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

      -- * Context
    , HasWorkerCtx (..)

      -- * Logging
    , WorkerLog (..)
    ) where

import Prelude hiding
    ( log, lookup )

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Wallet
    ( HasLogger, logger )
import Control.Concurrent
    ( ThreadId, forkFinally, killThread )
import Control.Exception.Base
    ( AsyncException (..), asyncExceptionFromException )
import Control.Monad
    ( void )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Tracer
    ( Tracer, traceWith )
import Data.Foldable
    ( traverse_ )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Labels
    ()
import Data.Generics.Product.Typed
    ( HasType )
import Data.Map.Strict
    ( Map )
import qualified Data.Map.Strict as Map
import Data.Text
    ( Text )
import qualified Data.Text as T
import Data.Text.Class
    ( ToText (..) )
import Fmt
    ( pretty )
import GHC.Generics
    ( Generic )
import UnliftIO.Exception
    ( SomeException, finally )
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

{-------------------------------------------------------------------------------
                                Worker Context
-------------------------------------------------------------------------------}

-- | A class to link an existing context to a worker context.
class HasType resource (WorkerCtx ctx) => HasWorkerCtx resource ctx where
    type WorkerCtx ctx :: *
    type WorkerMsg ctx :: *
    type WorkerKey ctx :: *
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

-- | Get all registered keys in the registry
keys
    :: WorkerRegistry key resource
    -> IO [key]
keys (WorkerRegistry mvar) =
    Map.keys <$> readMVar mvar

-- | Register a new worker
insert
    :: Ord key
    => WorkerRegistry key resource
    -> Worker key resource
    -> IO ()
insert (WorkerRegistry mvar) wrk =
    modifyMVar_ mvar (pure . Map.insert (workerId wrk) wrk)

-- | Delete a worker from the registry, but don't cancel the running task.
--
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
--
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

-- | A worker which holds and manipulate a paticular acquired resource. That
-- resource can be, for example, a handle to a database connection.
data Worker key resource = Worker
    { workerId :: key
    , workerThread :: ThreadId
    , workerResource :: resource
    } deriving (Generic)

-- | See 'register'
data MkWorker key resource msg ctx = MkWorker
    { workerBefore :: WorkerCtx ctx -> key -> IO ()
        -- ^ A task to execute before the main worker's task. When creating a
        -- worker, this task is guaranteed to have terminated once 'register'
        -- returns.
    , workerMain :: WorkerCtx ctx -> key -> IO ()
        -- ^ A task for the worker, possibly infinite
    , workerAfter
        :: Tracer IO (WorkerLog key msg) -> Either SomeException () -> IO ()
        -- ^ Action to run when the worker exits
    , workerAcquire :: (resource -> IO ()) -> IO ()
        -- ^ A bracket-style factory to acquire a resource
    }

defaultWorkerAfter
    :: Tracer IO (WorkerLog key msg)
    -> Either SomeException a
    -> IO ()
defaultWorkerAfter tr = traceWith tr . \case
    Right _ -> MsgFinished
    Left e -> case asyncExceptionFromException e of
        Just ThreadKilled -> MsgThreadKilled
        Just UserInterrupt -> MsgUserInterrupt
        _ -> MsgUnhandledException $ pretty $ show e

-- | Register a new worker for a given key.
--
-- A worker maintains an acquired resource. It expects a task as an argument
-- and will terminate as soon as its task is over. In practice, we provide a
-- never-ending task that keeps the worker alive forever.
--
-- Returns 'Nothing' if the worker fails to acquire the necessary resource or
-- terminates unexpectedly before entering its 'main' action.
--
register
    :: forall resource ctx key msg.
        ( Ord key
        , key ~ WorkerKey ctx
        , msg ~ WorkerMsg ctx
        , HasLogger (WorkerLog key msg) ctx
        , HasWorkerCtx resource ctx
        )
    => WorkerRegistry key resource
    -> ctx
    -> key
    -> MkWorker key resource msg ctx
    -> IO (Maybe (Worker key resource))
register registry ctx k (MkWorker before main after acquire) = do
    mvar <- newEmptyMVar
    let io = acquire $ \resource -> do
            let ctx' = hoistResource resource (MsgFromWorker k) ctx
            before ctx' k `finally` putMVar mvar (Just resource)
            main ctx' k
    threadId <- forkFinally io (cleanup mvar)
    takeMVar mvar >>= traverse (create threadId)
  where
    tr = ctx ^. logger @(WorkerLog key msg)
    create threadId resource = do
        let worker = Worker
                { workerId = k
                , workerThread = threadId
                , workerResource = resource
                }
        registry `insert` worker
        return worker
    cleanup mvar e = do
        void $ registry `delete` k
        void $ tryPutMVar mvar Nothing
        after tr e

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data WorkerLog key msg
    = MsgFinished
    | MsgThreadKilled
    | MsgUserInterrupt
    | MsgUnhandledException Text
    | MsgFromWorker key msg
    deriving (Show, Eq)

instance (ToText key, ToText msg) => ToText (WorkerLog key msg) where
    toText = \case
        MsgFinished ->
            "Worker has exited: main action is over."
        MsgThreadKilled ->
            "Worker has exited: killed by parent."
        MsgUserInterrupt ->
            "Worker has exited: killed by user."
        MsgUnhandledException err ->
            "Worker has exited unexpectedly: " <> err
        MsgFromWorker key msg
            | toText key == mempty -> toText msg
            | otherwise -> T.take 8 (toText key) <> ": " <> toText msg

instance HasPrivacyAnnotation (WorkerLog key msg)
instance HasSeverityAnnotation msg => HasSeverityAnnotation (WorkerLog key msg) where
    getSeverityAnnotation = \case
        MsgFinished -> Notice
        MsgThreadKilled -> Notice
        MsgUserInterrupt -> Notice
        MsgUnhandledException _ -> Error
        MsgFromWorker _ msg -> getSeverityAnnotation msg
