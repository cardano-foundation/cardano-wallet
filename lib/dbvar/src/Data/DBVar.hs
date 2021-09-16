{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Data.DBVar (
    -- * Synopsis
    -- | 'DBVar' represents a mutable variable whose value is kept in memory,
    -- but which is written to the hard drive on every update.
    -- This provides a convient interface for persisting
    -- values across program runs.
    -- For efficient updates, delta encodings are used, see "Data.Delta".
    --
    -- 'Store' represent a storage facility to which the 'DBVar'
    -- is written.

    -- * DBVar
      DBVar
    , readDBVar, updateDBVar, modifyDBVar
    , initDBVar, loadDBVar

    -- * Store
    , Store (..), newStore
    , embedStore, pairStores
    ) where

import Prelude

import Control.Monad ( void )
import Control.Applicative
    ( liftA2 )
import Control.Monad.Class.MonadSTM
    ( MonadSTM
    -- , TVar
    , atomically
    , modifyTVar'
    , newTVarIO
    , readTVar
    , retry
    , writeTVar
    )
import Data.Delta
    ( Delta (..)
    , Embedding' (..)
    , Embedding
    , inject
    , project
    , Machine (..)
    )

{-------------------------------------------------------------------------------
    DBVar
-------------------------------------------------------------------------------}
-- | A 'DBVar'@ m delta@ is a mutable reference to a value of type @a@.
-- The type @delta@ is a delta encoding for this value type @a@, 
-- that is we have @a ~ @'Base'@ delta@.
--
-- The value is kept in-memory.
-- However, whenever the value is updated, a copy of will be written
-- to persistent storage like a file or database on the hard disk.
-- For efficient updates, the delta encoding @delta@ is used.
--
-- Concurrency:
--
-- * Updates are atomic and will block other updates.
-- * Reads will /not/ be blocked during (most of) an update.
data DBVar m delta = DBVar
    { readDBVar_   :: m (Base delta)
    , updateDBVar_ :: delta -> m ()
    }

-- | Read the current value of the 'DBVar'.
readDBVar :: (Delta da, a ~ Base da) => DBVar m da -> m a
readDBVar = readDBVar_

-- | Update the value of the 'DBVar' using a delta encoding.
updateDBVar :: Delta da => DBVar m da -> da -> m ()
updateDBVar = updateDBVar_

-- | Modify the value in a 'DBVar'.
modifyDBVar
    :: (Delta da, Monad m, a ~ Base da)
    => DBVar m da -> (a -> (da, b)) -> m b
modifyDBVar var f = do
    a <- readDBVar var
    let (delta, b) = f a
    updateDBVar var delta
    pure b

-- | Initialize a new 'DBVar' for a given 'Store'.
initDBVar
    :: (MonadSTM m, Delta da, a ~ Base da)
    => Store m da -- ^ 'Store' for writing.
    -> a -- ^ Initial value.
    -> m (DBVar m da)
initDBVar store v = do
    writeS store v
    newWithCache (updateS store) v

-- | Create a 'DBVar' by loading its value from an existing 'Store' (if successful).
loadDBVar
    :: (MonadSTM m, Delta da)
    => Store m da -- ^ 'Store' for writing and for reading the initial value.
    -> m (Maybe (DBVar m da))
loadDBVar store =
    loadS store >>= \case
        Nothing -> pure Nothing
        Just a  -> Just <$> newWithCache (updateS store) a

-- | Create 'DBVar' from an initial value and an update function
-- using a 'TVar' as in-memory cache.
newWithCache
    :: (MonadSTM m, Delta da, a ~ Base da)
    => (a -> da -> m ()) -> a -> m (DBVar m da)
newWithCache update a = do
    cache  <- newTVarIO a
    locked <- newTVarIO False  -- lock for updating the cache
    pure $ DBVar
        { readDBVar_   = atomically $ readTVar cache
        , updateDBVar_ = \delta -> do
            old <- atomically $ do
                readTVar locked >>= \case
                    True  -> retry
                    False -> do
                        writeTVar locked True
                        readTVar cache
            let new = apply delta old
            update old delta
            atomically $ do
                writeTVar cache new
                writeTVar locked False
        }

{-------------------------------------------------------------------------------
    Store
-------------------------------------------------------------------------------}
-- | A 'Store' is an on-disk storage facility for values
-- of type @a ~@'Base'@ da@.
-- The store need not contain a properly formatted value at first.
--
-- The operations for a 'Store' are expected to satisfy several invariants.
-- For example, reading and writing should be inverse to each other.
--
-- > writeS s a >> loadS s = pure (Just a)
--
-- Also, the delta encoding should satisfy
--
-- > updateS s old delta = writeS s (apply delta old)
--
-- It is expected that the functions 'loadS', 'updateS', 'writeS'
-- do not throw synchronous exceptions. In the worst case,
-- 'loadS' should return 'Nothing' after reading or writing
-- to the store was unsuccesful.
data Store m da = Store
    { loadS   :: m (Maybe (Base da))
    , writeS  :: Base da -> m ()
    , updateS
        :: Base da -- old value
        -> da -- delta to new value
        -> m () -- write new value
    }

-- | An in-memory 'Store' from a mutable variable ('TVar') for testing.
newStore :: (Delta da, MonadSTM m) => m (Store m da)
newStore = do
    ref <- newTVarIO Nothing
    pure $ Store
        { loadS   = atomically $ readTVar ref
        , writeS  = atomically . writeTVar ref . Just
        , updateS = \_ -> atomically . modifyTVar' ref . fmap . apply
        }

-- | Add a caching layer to a 'Store'.
--
-- Access to the underlying 'Store' is enforced to be sequential,
-- but the cache can be accessed in parallel.
--
-- FIXME: Safety with respect to asynchronous exceptions?
cachedStore
    :: forall m da. (MonadSTM m, Delta da)
    => Store m da -> m (Store m da)
cachedStore Store{loadS,writeS,updateS} = do
    -- Lock that puts loadS, writeS and updateS into sequence
    islocked <- newTVarIO False
    let withLock :: forall b. m b -> m b
        withLock action = do
            atomically $ readTVar islocked >>= \case
                True  -> retry
                False -> writeTVar islocked True
            a <- action
            atomically $ writeTVar islocked False
            pure a

    -- Cache that need not be filled in the beginning
    iscached <- newTVarIO False
    cache    <- newTVarIO (Nothing :: Maybe (Base da))
    let writeCache ma = writeTVar cache ma >> writeTVar iscached True

    -- Load the value from the Store only if it is not cached and
    -- nobody else is writing to the store.
    let load :: m (Maybe (Base da))
        load = do
            action <- atomically $
                readTVar iscached >>= \case
                    True  -> do
                        ma <- readTVar cache  -- read from cache
                        pure $ pure ma
                    False -> readTVar islocked >>= \case
                        True  -> retry  -- somebody is writing
                        False -> pure $ withLock $ do
                            ma <- loadS
                            atomically $ writeCache ma
                            pure ma
            action

    pure $ Store
        { loadS = load
        , writeS = \a -> withLock $ do
            atomically $ writeCache (Just a)
            writeS a
        , updateS = \old delta -> withLock $ do
            atomically $ writeCache $ Just $ apply delta old
            updateS old delta
        }

embedStore :: (MonadSTM m, Delta da, Delta db)
    => Embedding da db -> Store m db -> m (Store m da)
embedStore embed bstore = do
    machine <- newTVarIO Nothing
    let readMachine  = atomically $ readTVar machine
        writeMachine = atomically . writeTVar machine . Just
    let load = loadS bstore >>= \mb -> case project embed =<< mb of
                Nothing      -> pure Nothing
                Just (a,mab) -> do
                    writeMachine mab
                    pure $ Just a
        write a = do
            let mab = inject embed a
            writeMachine mab
            writeS bstore (state_ mab)
        update a da = do
            readMachine >>= \case
                Nothing   -> do -- we were missing the inital write
                    write (apply da a)
                Just mab1 -> do -- advance the machine by one step
                    let (db, mab2) = step_ mab1 (a,da)
                    updateS bstore (state_ mab2) db
                    writeMachine mab2

    pure $ Store {loadS=load,writeS=write,updateS=update}


-- | Obtain a 'Store' for one type @a1@ from a 'Store' for another type @a2@
-- via an 'Embedding' of the first type into the second type.
embedStore'
    :: (Monad m)
    => Embedding' da db -> Store m db -> Store m da
embedStore' Embedding'{load,write,update} Store{loadS,writeS,updateS} = Store
    { loadS   = (load =<<) <$> loadS
    , writeS  = writeS . write
    , updateS = \a da -> do
        mb <- loadS
        case mb of
            Nothing -> pure ()
            Just b  -> updateS b (update a b da)
    }

-- | Combine two 'Stores' into a store for pairs.
pairStores :: (Monad m, Delta da, Delta db)
    => Store m da -> Store m db -> Store m (da, db)
pairStores sa sb = Store
    { loadS = liftA2 (,) <$> loadS sa <*> loadS sb
    , writeS = \(a,b) -> writeS sa a >> writeS sb b
    , updateS = \(a,b) (da,db) -> updateS sa a da >> updateS sb b db
    }
