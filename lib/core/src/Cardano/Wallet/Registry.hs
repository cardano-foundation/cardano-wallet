{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Registry
    ( -- * Worker Registry
      WorkerRegistry
    , empty
    , insert
    , keys
    , lookup
    , remove

      -- * Worker
    , Worker
    , MkWorker(..)
    , newWorker
    , workerThread
    , workerId
    , workerResource

      -- * Context
    , HasWorkerCtx (..)

      -- * Logging
    , WithWorkerKey (..)
    ) where

import Prelude hiding
    ( log, lookup )

import Cardano.BM.Trace
    ( Trace, appendName )
import Cardano.Wallet
    ( HasLogger, logger )
import Control.Concurrent
    ( ThreadId, forkFinally, killThread )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar_
    , newEmptyMVar
    , newMVar
    , putMVar
    , readMVar
    , takeMVar
    , tryPutMVar
    )
import Control.Exception
    ( SomeException, finally )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Tracer
    ( contramap )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( (.~), (^.) )
import Data.Generics.Labels
    ()
import Data.Generics.Product.Typed
    ( HasType )
import Data.Map.Strict
    ( Map )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import GHC.Generics
    ( Generic )

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

{-------------------------------------------------------------------------------
                                Worker Context
-------------------------------------------------------------------------------}

-- | A class to link an existing context to a worker context.
class HasType resource (WorkerCtx ctx) => HasWorkerCtx resource ctx where
    type WorkerCtx ctx :: *
    hoistResource :: resource -> ctx -> WorkerCtx ctx

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

-- | Remove a worker from the registry (and cancel any running task)
remove
    :: Ord key
    => WorkerRegistry key resource
    -> key
    -> IO ()
remove (WorkerRegistry mvar) k =
    modifyMVar_ mvar (Map.alterF alterF k)
  where
    alterF = \case
        Nothing -> pure Nothing
        Just wrk -> do
            -- NOTE: It is safe to kill a thread that is already dead.
            killThread (workerThread wrk)
            return Nothing

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

-- | See 'newWorker'
data MkWorker key resource ctx = MkWorker
    { workerBefore :: WorkerCtx ctx -> key -> IO ()
        -- ^ A task to execute before the main worker's task. When creating a
        -- worker, this task is guaranteed to have terminated once 'newWorker'
        -- returns.
    , workerMain :: WorkerCtx ctx -> key -> IO ()
        -- ^ A task for the worker, possibly infinite
    , workerAfter :: Trace IO Text -> Either SomeException () -> IO ()
        -- ^ Action to run when the worker exits
    , workerAcquire :: (resource -> IO ()) -> IO ()
        -- ^ A bracket-style factory to acquire a resource
    }

-- | Create a new worker for a given key. Workers maintain an acquired resource.
-- They expect a task as argument and will terminate as soon as their task
-- is over; so in practice, we provide a never-ending task that keeps the worker
-- alive forever.
--
-- Returns 'Nothing' if the worker fails to acquire the necessary resource or
-- terminate unexpectedly before entering its 'main' action.
--
-- >>> newWorker ctx k withDBLayer restoreWallet
-- worker<thread#1234>
newWorker
    :: forall key resource ctx.
        ( HasLogger ctx
        , HasWorkerCtx resource ctx
        , ToText key
        )
    => ctx
    -> key
    -> MkWorker key resource ctx
    -> IO (Maybe (Worker key resource))
newWorker ctx k (MkWorker before main after acquire) = do
    mvar <- newEmptyMVar
    let io = acquire $ \resource -> do
            let ctx' = hoistResource resource (ctx & logger .~ tr')
            before ctx' k `finally` putMVar mvar (Just resource)
            main ctx' k
    threadId <- forkFinally io (cleanup mvar)
    takeMVar mvar >>= \case
        Nothing -> return Nothing
        Just resource -> return $ Just Worker
            { workerId = k
            , workerThread = threadId
            , workerResource = resource
            }
  where
    tr = ctx ^. logger
    tr' = contramap (fmap (toText . WithWorkerKey k)) $ appendName "worker" tr
    cleanup mvar e = tryPutMVar mvar Nothing *> after tr' e

-- | A worker log event includes the key (i.e. wallet ID) as context.
data WithWorkerKey key = WithWorkerKey key Text
    deriving (Eq, Show)

instance ToText key => ToText (WithWorkerKey key) where
    toText (WithWorkerKey k msg) = T.take 8 (toText k) <> ": " <> msg
