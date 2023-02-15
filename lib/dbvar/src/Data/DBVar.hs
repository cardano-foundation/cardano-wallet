{-# OPTIONS_GHC -Wno-redundant-constraints#-}
-- We intentionally specify more constraints than necessary for some exports.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Data.DBVar (
    -- * Synopsis
    -- | 'DBVar' represents a mutable variable whose value is kept in memory,
    -- but which is written to the hard drive on every update.
    -- This provides a convenient interface for persisting
    -- values across program runs.
    -- For efficient updates, delta encodings are used, see "Data.Delta".
    --
    -- 'Store' represent a storage facility to which the 'DBVar'
    -- is written.

    -- * DBVar
      DBVar
    , readDBVar, updateDBVar, modifyDBVar, modifyDBMaybe
    , initDBVar, loadDBVar

    -- * Store
    , Store (..)
    , newStore
    , NotInitialized (..)
    -- $EitherSomeException
    , embedStore, pairStores

    -- * Testing
    , embedStore'
    , updateLoad
    , newCachedStore
    ) where

import Prelude

import Control.Applicative
    ( liftA2 )
import Control.Exception
    ( Exception, SomeException, toException )
import Control.Monad
    ( join )
import Control.Monad.Class.MonadSTM
    ( MonadSTM
    , atomically
    , modifyTVar'
    , newTVarIO
    , readTVar
    , readTVarIO
    , retry
    , writeTVar
    )
import Control.Exception
    ( Exception, SomeException, toException )
import Control.Monad.Class.MonadThrow
    ( MonadEvaluate
    , MonadMask
    , MonadThrow
    , bracket
    , evaluate
    , finally
    , mask
    , throwIO
    )
import Data.Delta
    ( Delta (..), Embedding, Embedding' (..), Machine (..), inject, project )

{-------------------------------------------------------------------------------
    DBVar
-------------------------------------------------------------------------------}
-- | A 'DBVar'@ m delta@ is a mutable reference to a Haskell value of type @a@.
-- The type @delta@ is a delta encoding for this value type @a@,
-- that is we have @a ~ @'Base'@ delta@.
--
-- The Haskell value is cached in memory, in weak head normal form (WHNF).
-- However, whenever the value is updated, a copy of will be written
-- to persistent storage like a file or database on the hard disk;
-- any particular storage is specified by the 'Store' type.
-- For efficient updates, the delta encoding @delta@ is used in the update.
--
-- Concurrency:
--
-- * Updates are atomic and will block other updates.
-- * Reads will /not/ be blocked during an update
--   (except for a small moment where the new value atomically
--    replaces the old one).
data DBVar m delta = DBVar
    { readDBVar_     :: m (Base delta)
    , modifyDBMaybe_ :: forall b. (Base delta -> (Maybe delta, b)) -> m b
    }

-- | Read the current value of the 'DBVar'.
readDBVar :: (Delta da, a ~ Base da) => DBVar m da -> m a
readDBVar = readDBVar_

-- | Update the value of the 'DBVar' using a delta encoding.
--
-- The new value will be evaluated to weak head normal form.
updateDBVar :: (Delta da, Monad m) => DBVar m da -> da -> m ()
updateDBVar var delta = modifyDBMaybe var $ \_ -> (Just delta,())

-- | Modify the value in a 'DBVar'.
--
-- The new value will be evaluated to weak head normal form.
modifyDBVar
    :: (Delta da, Monad m, a ~ Base da)
    => DBVar m da -> (a -> (da, b)) -> m b
modifyDBVar var f = modifyDBMaybe var $ \a -> let (da,b) = f a in (Just da,b)

-- | Maybe modify the value in a 'DBVar'
--
-- If updated, the new value will be evaluated to weak head normal form.
modifyDBMaybe
    :: (Delta da, Monad m, a ~ Base da)
    => DBVar m da -> (a -> (Maybe da, b)) -> m b
modifyDBMaybe = modifyDBMaybe_

-- | Initialize a new 'DBVar' for a given 'Store'.
initDBVar
    ::  ( MonadSTM m, MonadThrow m, MonadEvaluate m, MonadMask m
        , Delta da, a ~ Base da
        )
    => Store m da -- ^ 'Store' for writing.
    -> a -- ^ Initial value.
    -> m (DBVar m da)
initDBVar store v = do
    writeS store v
    newWithCache (updateS store . Just) v

-- | Create a 'DBVar' by loading its value from an existing 'Store'
-- Throws an exception if the value cannot be loaded.
loadDBVar
    ::  ( MonadSTM m, MonadThrow m, MonadEvaluate m, MonadMask m
        , Delta da
        )
    => Store m da -- ^ 'Store' for writing and for reading the initial value.
    -> m (DBVar m da)
loadDBVar store =
    loadS store >>= \case
        Left  e -> throwIO e
        Right a -> newWithCache (updateS store . Just) a

-- | Create 'DBVar' from an initial value and an update function
-- using a 'TVar' as in-memory cache.
--
-- Space: The value in the 'TVar' will be evaluated to weak head normal form.
--
-- Concurrency: The update function needs to be atomic even in the presence
-- of asynchronous exceptions.
newWithCache
    ::  ( MonadSTM m, MonadThrow m, MonadMask m, MonadEvaluate m
        , Delta da, a ~ Base da
        )
    => (a -> da -> m ()) -> a -> m (DBVar m da)
newWithCache update a = do
    cache  <- newTVarIO a
    locked <- newTVarIO False   -- lock for updating the cache
    pure $ DBVar
        { readDBVar_     = readTVarIO cache
        , modifyDBMaybe_ = \f -> do
            let before = atomically $ do
                    readTVar locked >>= \case
                        True  -> retry
                        False -> do
                            writeTVar locked True
                            readTVar cache
                after _ = atomically $ writeTVar locked False
                action old = do
                    let (mdelta, b) = f old
                    case mdelta of
                        Nothing    -> pure ()
                        Just delta -> do
                            new <- evaluate $ apply delta old
                            mask $ \restore -> do
                                -- We mask asynchronous exceptions here
                                -- to ensure that the TVar will be updated
                                -- whenever @update@ succeeds without exception.
                                restore $ update old delta
                                atomically $ writeTVar cache new
                    pure b
            bracket before after action
        }

{-------------------------------------------------------------------------------
    Store
-------------------------------------------------------------------------------}
{- |
A 'Store' is a storage facility for Haskell values of type @a ~@'Base'@ da@.
Typical use cases are a file or a database on the hard disk.

A 'Store' has many similarities with an 'Embedding'.
The main difference is that storing value in a 'Store' has side effects.
A 'Store' is described by three action:

* 'writeS' writes a value to the store.
* 'loadS' loads a value from the store.
* 'updateS' uses a delta encoding of type @da@ to efficiently update
    the store.
    In order to avoid performing an expensive 'loadS' operation,
    the action 'updateS' expects the value described by the store
    as an argument, but no check is performed whether the provided
    value matches the contents of the store.
    Also, not every store inspects this argument.

A 'Store' is characterized by the following properties:

* The store __need not contain__ a properly formatted __value__:
    Loading a value from the store may fail, and this is why 'loadS'
    has an 'Either' result.
    For example, if the 'Store' represents
    a file on disk, then the file may corrupted or in an incompatible
    file format when first opened.
    In such a case of failure, the result 'Left'@ (e :: @'SomeException'@)@
    is returned, where the exception @e@ gives more information
    about the failure.

    However, loading a value after writing it should always succeed,
    we have

        > writeS s a >> loadS s  =  pure (Right a)

* The store is __redundant__:
    Two stores with different contents may describe
    the same value of type @a@.
    For example, two files with different whitespace
    may describe the same JSON value.
    In general, we have

        > loadS s >>= either (const $ pure ()) (writeS s) ≠  pure ()

* Updating a store __commutes with 'apply'__:
    We have

        > updateS s a da >> loadS s  =  pure $ Right $ apply a da

    However, since the store is redundant, we often have

        > updateS s a da  ≠  writeS s (apply a da)

* __Exceptions__:
    It is expected that the functions 'loadS', 'updateS', 'writeS'
    do not throw synchronous exceptions. In the worst case,
    'loadS' should return 'Left' after reading or writing
    to the store was unsuccessful.

* __Concurrency__:
    It is expected that the functions 'updateS' and 'writeS'
    are /atomic/: Either they succeed in updating / writing
    the new value in its entirety, or the old value is kept.
    In particular, we expect this even when one of these
    functions receives an asynchronous exception and needs to abort
    normal operation.
-}

data Store m da = Store
    { loadS   :: m (Either SomeException (Base da))
    , writeS  :: Base da -> m ()
    , updateS
        :: Maybe (Base da) -- old value, for performance
        -> da -- delta to new value
        -> m () -- write new value
    }

{- HLINT ignore newStore "Use readTVarIO" -}
-- | An in-memory 'Store' from a mutable variable ('TVar').
-- Useful for testing.
newStore :: (Delta da, MonadSTM m) => m (Store m da)
newStore = do
    ref <- newTVarIO $ Left $ toException NotInitialized
    pure $ Store
        { loadS   = atomically $ readTVar ref
        , writeS  = atomically . writeTVar ref . Right
        , updateS = \_ -> atomically . modifyTVar' ref . fmap . apply
        }

{- | $EitherSomeException

NOTE: [EitherSomeException]

In this version of the library, the error case returned by 'loadS' and 'load'
is the general 'SomeException' type, which is a disjoint sum of all possible
error types (that is, members of the 'Exception' class).

In a future version of this library, this may be replaced by a more specific
error type, but at the price of introducing a new type parameter @e@ in the
'Store' type.

For now, I have opted to explore a region of the design space
where the number of type parameters is kept to a minimum.
I would argue that making errors visible on the type level is not as
useful as one might hope for, because in exchange for making the types noisier,
the amount of type-safety we gain is very small.
Specifically, if we encounter an element of the 'SomeException' type that
we did not expect, it is entirely ok to 'throw' it.
For example, consider the following code:
@
let ea :: Either SomeException ()
    ea = [..]
in
    case ea of
        Right _ -> "everything is ok"
        Left e -> case fromException e of
            Just (AssertionFailed _) -> "bad things happened"
            Nothing -> throw e
@
In this example, using the more specific type @ea :: Either AssertionFailed ()@
would have eliminated the need to handle the 'Nothing' case.
But as we are dealing with exceptions, this case does have a default handler,
and there is less need to exclude it at compile as opposed to, say,
the case of an empty list.
-}

-- | Failure that occurs when calling 'loadS' on a 'newStore' that is empty.
data NotInitialized = NotInitialized deriving (Eq, Show)
instance Exception NotInitialized


-- | Add a caching layer to a 'Store'.
--
-- Access to the underlying 'Store' is enforced to be sequential,
-- but the cache can be accessed in parallel.
-- FIXME: There is still a small race condition where the cache
-- could be written twice before it is filled.
-- In general, think about restricting the monad `m`,
-- as the `Store` operations do not compose very well. 🤔
newCachedStore
    :: forall m da. (Delta da, MonadSTM m, MonadThrow m, MonadEvaluate m)
    => Store m da -> m (Store m da)
newCachedStore Store{loadS,writeS,updateS} = do
    -- Lock that puts loadS, writeS and updateS into sequence
    islocked <- newTVarIO False
    let withLock :: forall b. m b -> m b
        withLock action = do
            atomically $ readTVar islocked >>= \case
                True  -> retry
                False -> writeTVar islocked True
            action `finally` atomically (writeTVar islocked False)

    -- Cache that need not be filled in the beginning
    cache    <- newTVarIO (Nothing :: Maybe (Base da))
    let writeCache ma = writeTVar cache ma

    -- Load the value from the Store only if it is not cached and
    -- nobody else is writing to the store.
    let load :: m (Either SomeException (Base da))
        load = join $ atomically $ do
            ma <- readTVar cache
            case ma of
                Nothing -> readTVar islocked >>= \case
                    True  -> retry  -- somebody is writing
                    False -> pure $ withLock $ do
                        ea <- loadS
                        case ea of
                            Left  e -> pure $ Left e
                            Right a -> do
                                atomically $ writeCache $ Just a
                                pure $ Right a
                Just a -> pure $ pure $ Right a

    pure $ Store
        { loadS = load
        , writeS = \a -> withLock $ do
            atomically $ writeCache (Just a)
            writeS a
        , updateS = updateLoad load throwIO $ \old delta -> withLock $ do
            new <- evaluate $ apply delta old
            atomically $ writeCache $ Just new
            updateS (Just old) delta
        }

embedStore :: (MonadSTM m, MonadMask m, Delta da)
    => Embedding da db -> Store m db -> m (Store m da)
embedStore embed bstore = do
    -- For reasons of efficiency, we have to store the 'Machine'
    -- that is created within the 'Embedding'.
    machine <- newTVarIO Nothing
    let readMachine  = readTVarIO machine
        writeMachine = atomically . writeTVar machine . Just

    -- Operations of the result 'Store'.
    let load = loadS bstore >>= \case
            Left  e -> pure $ Left e
            Right b -> case project embed b of
                Left  e       -> pure $ Left e
                Right (a,mab) -> do
                    writeMachine mab
                    pure $ Right a
        write a = do
            let mab = inject embed a
            mask $ \restore -> do
                restore $ writeS bstore (state_ mab)
                writeMachine mab
        update = updateLoad load throwIO $ \a da -> do
            readMachine >>= \case
                Nothing   -> do -- we were missing the initial write
                    write (apply da a)
                Just mab1 -> do -- advance the machine by one step
                    let (db, mab2) = step_ mab1 (a,da)
                    mask $ \restore -> do
                        restore $ updateS bstore (Just $ state_ mab2) db
                        writeMachine mab2
    pure $ Store {loadS=load,writeS=write,updateS=update}


-- | Obtain a 'Store' for one type @a1@ from a 'Store' for another type @a2@
-- via an 'Embedding'' of the first type into the second type.
--
-- Note: This function is exported for testing and documentation only,
-- use the more efficient 'embedStore' instead.
embedStore'
    :: (Monad m, MonadThrow m)
    => Embedding' da db -> Store m db -> Store m da
embedStore' Embedding'{load,write,update} Store{loadS,writeS,updateS} =
    let
        loadL =  (load =<<) <$> loadS
        updateL = \ma da -> case ma of
            Just a -> loadS >>= \case
                Left  _ -> pure ()
                Right b -> updateS (Just b) (update a b da)
            Nothing -> do
                ea <- loadL
                case ea of
                    Left  e -> throwIO e
                    Right a -> updateL (Just a) da
    in Store
        { loadS   = loadL
        , writeS  = writeS . write
        , updateS = updateL
        }

-- | Combine two 'Stores' into a store for pairs.
--
-- WARNING: The 'updateS' and 'writeS' functions of the result are not atomic
-- in the presence of asynchronous exceptions.
-- For example, the update of the first store may succeed while the update of
-- the second store may fail.
-- In other words, this combinator works for some monads, such as @m = @'STM',
-- but fails for others, such as @m = 'IO'@.
pairStores :: Monad m => Store m da -> Store m db -> Store m (da, db)
pairStores sa sb = Store
    { loadS = liftA2 (,) <$> loadS sa <*> loadS sb
    , writeS = \(a,b) -> writeS sa a >> writeS sb b
    , updateS = \mi (da,db) ->
        case mi of
            Nothing -> updateS sa Nothing da >> updateS sb Nothing db
            Just (a,b) -> updateS sa (Just a) da >> updateS sb (Just b) db
    }

-- | Helper for implementing `updateS`
-- for the case where a value is not yet loaded.
updateLoad :: (Exception e, Monad m)
    => m (Either e t) -- ^ How to load the value.
    -> (e -> m b) -- ^ What to do with the error when loading the value.
    -> (t -> da -> m b) -- ^ What to do with the value.
    -> Maybe t -- ^ Value, maybe loaded, maybe not.
    -> da -- ^ Delta.
    -> m b
updateLoad load handle update' Nothing da = do
    ea <- load
    case ea of
        Left e -> handle e
        Right x -> update' x da
updateLoad _load _  update' (Just x) da = update' x da
