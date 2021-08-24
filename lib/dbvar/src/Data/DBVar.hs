{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
    , Table, newTable
    ) where

import Prelude

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
    ( Delta (..), Embedding (..), DeltaSet (..) )
import Data.Set
    ( Set )

{-------------------------------------------------------------------------------
    DBVar
-------------------------------------------------------------------------------}
-- | A 'DBVar'@ m delta a@ is a mutable reference to a value of type 'a'.
--
-- The value is kept in-memory.
-- However, whenever the value is updated, a copy of will be written
-- to persistent storage like a file or database on the hard disk.
-- For efficient updates, a delta encoding @delta@ is used.
--
-- Concurrency:
--
-- * Updates are atomic and will block other updates.
-- * Reads will /not/ be blocked during (most of) an update.
data DBVar m delta a = DBVar
    { readDBVar   :: m a
    -- ^ Read the current value of the 'DBVar'.
    , updateDBVar :: delta -> m ()
    -- ^ Update the value of the 'DBVar' using a delta encoding.
    }

-- | Modify the value in a 'DBVar'.
modifyDBVar :: Monad m => DBVar m delta a -> (a -> (delta, b)) -> m b
modifyDBVar var f = do
    a <- readDBVar var
    let (delta, b) = f a
    updateDBVar var delta
    pure b

-- | Initialize a new 'DBVar' for a given 'Store'.
initDBVar
    :: (MonadSTM m, Delta delta, v ~ Base delta)
    => Store m delta v -- ^ 'Store' for writing.
    -> v -- ^ Initial value.
    -> m (DBVar m delta v)
initDBVar store v = do
    writeS store v
    newWithCache (updateS store) v

-- | Create a 'DBVar' by loading its value from an existing 'Store' (if successful).
loadDBVar
    :: (MonadSTM m, Delta delta, v ~ Base delta)
    => Store m delta v -- ^ 'Store' for writing and for reading the initial value.
    -> m (Maybe (DBVar m delta v))
loadDBVar store =
    loadS store >>= \case
        Nothing -> pure Nothing
        Just a  -> Just <$> newWithCache (updateS store) a

-- | Create 'DBVar' from an initial value and an update function
-- using a 'TVar' as in-memory cache.
newWithCache
    :: (MonadSTM m, Delta delta, v ~ Base delta)
    => (v -> delta -> m ()) -> v -> m (DBVar m delta v)
newWithCache update v = do
    cache  <- newTVarIO v
    locked <- newTVarIO False  -- lock for updating the cache
    pure $ DBVar
        { readDBVar   = atomically $ readTVar cache
        , updateDBVar = \delta -> do
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
-- | A 'Store' is an on-disk storage facility for values of type 'a'.
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
data Store m delta a = Store
    { loadS   :: m (Maybe a)
    , writeS  :: a -> m ()
    , updateS
        :: a -- old value
        -> delta -- delta to new value
        -> m () -- write new value
    }

-- | An in-memory 'Store' from a mutable variable ('TVar') for testing.
newStore
    :: (Delta delta, a ~ Base delta, MonadSTM m)
    => m (Store m delta a)
newStore = do
    ref <- newTVarIO Nothing
    pure $ Store
        { loadS   = atomically $ readTVar ref
        , writeS  = atomically . writeTVar ref . Just
        , updateS = \_ -> atomically . modifyTVar' ref . fmap . apply
        }

-- | A database table is a 'Store' for sets with
-- corresponding delta encoding.
type Table m a = Store m [DeltaSet a] (Set a)

-- | Create an in-memory database table for testing.
newTable :: (MonadSTM m, Ord a) => m (Table m a)
newTable = newStore


-- | Obtain a 'Store' for one type @a1@ from a 'Store' for another type @a2@
-- via an 'Embedding' of the first type into the second type.
embedStore :: Functor m => Embedding a1 d1 a2 d2 -> Store m d2 a2 -> Store m d1 a1
embedStore Embedding{load,write,update} Store{loadS,writeS,updateS} = Store
    { loadS   = (load =<<) <$> loadS
    , writeS  = writeS . write
    , updateS = \a1 delta1 -> updateS (write a1) (update a1 delta1)
    }

-- | Combine two 'Stores' into a store for pairs.
pairStores :: Monad m => Store m d1 a1 -> Store m d2 a2 -> Store m (d1, d2) (a1, a2)
pairStores s1 s2 = Store
    { loadS = liftA2 (,) <$> loadS s1 <*> loadS s2
    , writeS = \(a1,a2) -> writeS s1 a1 >> writeS s2 a2
    , updateS = \(a1,a2) (d1,d2) -> updateS s1 a1 d1 >> updateS s2 a2 d2
    }
