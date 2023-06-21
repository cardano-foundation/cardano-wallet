-- We intentionally specify more constraints than necessary for some exports.
{-# OPTIONS_GHC -Wno-redundant-constraints#-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
module Data.Store (
    -- * Synopsis
    -- | 'Store' represents a facility for storing one value of a given type.
    --
    -- Typically, this facility is useful when we want to store
    -- a value __outside of RAM__, e.g. in a database file on the hard disk,
    -- because otherwise, we can just work with the Haskell value itself.
    -- In such cases, the value is typically a collection,
    -- for example 'Data.Map.Map'@ @'Integer'@ @'String'.

    -- * Store, definition
    -- ** Type
      Store (..)

    -- ** Properties
    -- $Properties

    -- *** Laws: Load and Write
    -- $LoadWriteLaws

    -- *** Laws: Update
    -- $UpdateLaws

    -- *** Laws: Query
    -- $QueryLaws

    -- *** Monad
    -- $StoreMonad

    -- *** updateS, Maybe argument
    -- $updateS

    -- *** loadS, SomeException
    -- $EitherSomeException

    -- * Store, functions
    -- ** Query
    , Query (..)
    , Whole (..)

    -- ** Constructors
    , SimpleStore
    , mkSimpleStore
    , UpdateStore
    , mkUpdateStore
    , mkQueryStore

    -- ** Combinators
    , embedStore
    , pairStores
    , newCachedStore

    -- ** Helpers
    , updateLoad
    , loadWhenNothing

    -- ** Testing
    , embedStore'
    , newStore, NotInitialized (..), updateSequence
    ) where

import Prelude

import Control.Applicative
    ( liftA2 )
import Control.Concurrent.Class.MonadSTM
    ( MonadSTM, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, retry,
    writeTVar )
import Control.Exception
    ( Exception, SomeException (..), toException )
import Control.Monad
    ( foldM_, join )
import Control.Monad.Class.MonadThrow
    ( MonadEvaluate, MonadMask, MonadThrow, evaluate, finally, mask, throwIO )
import Data.Delta
    ( Delta (..), Embedding, Embedding' (..), Machine (..), Replace (..),
    inject, project )
import Data.Kind
    ( Type )
import GHC.Generics
    ( (:+:) (..) )

{-------------------------------------------------------------------------------
    Store
-------------------------------------------------------------------------------}
{- |
A 'Store' is a storage facility for Haskell values of type
@a ~ @'Base'@ da ~ @'World'@ qa@.

Typical use cases are a file or a database on the hard disk.

The purpose of the type parameters is:

* The monad @m@ encapsulates access to the storage space.
* The query type @qa@ represents the specialized queries
  that this store supports.
* The delta type @da@ is used for efficient updates.

If you care about one these aspects, but not the others,
we recommend to use a specialized type synonym
such as 'SimpleStore' or 'UpdateStore'.
-}
data Store m (qa :: Type -> Type) da = Store
    {
      -- | Load the value from the store into memory, or fail.
      --
      -- This operation can be expensive.
      loadS   :: m (Either SomeException (Base da))
      -- | Write a value from memory into the store.
    , writeS  :: Base da -> m ()
      -- | Update the value in the store
      -- efficiently by using a 'Delta' type @da@.
      --
      -- For effiency,
      -- the first argument may supply the current value in-memory.
    , updateS
        :: Maybe (Base da) -- old value, for performance
        -> da -- delta to new value
        -> m () -- write new value
      -- | Run a specialized 'Query' on the value in the store.
      --
      -- This operation can be less expensive than 'loadS',
      -- because the query may not need to load the whole value into memory.
    , queryS  :: forall b. qa b -> m b
    }

{- $Properties
Any implementation of 'Store' is expected to satisfy the __properties__
specified in this section.
We make no attempt at enforcing these properties on the type-level.
However, the module "Test.Store" provides QuickCheck code for these
properties for automated testing.
-}

-- Note [LoadWriteLaws]
{- $LoadWriteLaws

The most fundamental operations on a 'Store' are

* 'loadS' — loads the value contained in the 'Store' into memory.
* 'writeS' — writes a value from memory into the 'Store'.

These two operations are characterized by the following design:

1. The store __need not contain__ a properly formatted __value__.

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

2. The store is __redundant__.

    Two stores with different internal contents may contain
    the same value of type @a@.
    For example, two files with different whitespace
    may describe the same JSON value.
    In general, loading a value and writing it again may change the
    internal store contents, i.e.

        > loadS s >>= either (\_ -> pure ()) (writeS s)  ≠  pure ()
-}

-- Note [UpdateLaws]
{- $UpdateLaws

In order to update the store content without loading all of it into memory,
'Store' supports the operation

* 'updateS' — updates the value contained in the 'Store' using a 'Delta' type.

This operation is characterized by the following law:

* Updating a store __commutes with 'apply'__.

    We have

        > updateS s (Just a) da >> loadS s  =  pure $ Right $ apply a da

    However, since the store is redundant, we often have

        > updateS s (Just a) da  ≠  writeS s (apply a da)

The combination of 'loadS', 'writeS', 'updateS' has many similarities
with an 'Embedding' of delta types. However, the main difference
is that manipulating a 'Store' involves side effects.
-}

-- Note [QueryLaws]
{- $QueryLaws

In order to query parts of the store content
without loading all of it into memory,
'Store' supports the operation

* 'queryS' — run a specialized 'Query' on the value contained in the 'Store'.

This operation is characterized by the following law:

* Querying a store __commutes with 'query'__:

        >  ∀q. query q <$> (loadS s >>= either throw pure)  =  queryS s q
-}

-- Note [updateS argument]
{- $updateS

The function 'updateS' applies a delta to the content of the 'Store'.
Depending on the implementation of the 'Store', this operation may
require large parts of the content to be loaded into memory,
which is expensive.
In some use cases such as 'Data.DBVar.DBVar', the value is already available
in memory and can be used for executing the update.
For these cases, the __first argument__ of 'updateS'
__may__ provide the __in-memory value__.
We expect that the following property holds:

>   updateS s Nothing da
> =
>   loadS s >>= \(Right a) -> updateS s (Just a) da

The helper 'loadWhenNothing' is useful for handling this argument.
-}

{- $StoreMonad

The monad @m@ in 'Store'@ m da@ provides the storage space for the value.
Put differently, we like to think of @m@ as a
'Control.Monad.Trans.State.State' monad whose state contains the value.
However, this monad @m@ could have __additional side effects__
such as exceptions, concurrency, non-determinism, and so on.
We would have to specify how a 'Store' should behave with regards to these
effects, which complicates matters significantly.
(In fact, the equality sign @=@ for the laws above has to be
interpreted "… equal effects as far as the 'Store' is concerned".
A proper approach to a specification would involve Hoare logic.)

For simplicity, we now assume that the monad @m@ only has
the effects __state__ and __exceptions__ —
we make no attempt at specifying how an implementation
should behave for concurrent usage of, say, 'updateS'.
This assumption ensures some composability of the 'Store' abstraction.
However, it also implies that choosing @m ~ @'Control.Monad.STM.STM'
results in specified semantics, whereas choosing @m ~ @'IO' can
result in unspecified behavior.
(TODO: Perhaps create a type class 'MonadSequential' to keep track
of this on the type level?)

More specifically, the interaction between 'Store' functions and
effects are as follows:

* __State__: The laws presented above specify the essentials
of how the store state changes. However, this specification is not complete,
other "expected" rules such as

    > writeS s a >> writeS s b  =  writeS s b

    etc. should also hold.

* __Exceptions__:

    * 'loadS' should not throw a synchronous exception,
      but return 'Left' instead.
    * 'queryS' should throw a synchronous exception iff 'loadS' returns 'Left'.
      Moving the error case into the monad @m@ simplifes the use of this operation.
    * 'writeS' and 'loadS' should not throw synchronous exceptions.
      However, in case they do throw an exception,
      the contents of the 'Store' should be treated as corrupted,
      and 'loadS' should return 'Left' subsequently.

* __Concurrency__: We do not specify behavior under concurrent operation.
    Concurrent access to a 'Store' is a frequent desideratum
    — but you will have to implement it yourself.

    One design pattern is to use a custom monad @m ~ MyMonad@
    that has a way of executing state changes atomically,

    > atomically :: MyMonad a -> IO a

    Specifically, @atomically@ either applies /all/ state changes,
    or /none/ of the state changes.
    For instance, SQL transactions can be used for this,
    see e.g. <https://www.sqlite.org/lang_transaction.html>.
    Then, you can implement a 'Store'@ MyMonad@ by composing smaller 'Store',
    and use @atomically@ in a scope where you want to use the 'Store'
    rather than implement it.

* __Non-determinism__ or other effects: Here be dragons.

-}

-- Note [EitherSomeException]
{- $EitherSomeException

In the __error case__ that the store does not contain a value,
'loadS' returns a 'Left' value of type 'SomeException'.
This type is a disjoint sum of all possible
error types (that is, members of the 'Exception' class).

We could parametrize 'Store' by an additional type parameter @e@ representing
the possible error cases. However, we have opted to explore
a region of the design space where the number of type parameters
is kept to a minimum.

In fact, I would argue that making errors visible on the type level is not
very useful: we add much noise to the type level,
but we gain little type-safety in exchange.
Specifically, if we encounter an element of the 'SomeException' type that
we did not expect, we can always 'throw' it.
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
would have eliminated the 'Nothing' case.
However, this case has the sensible default value:
@throw e@, we rethrow the exception that we did not expect.
Ruling out this case on the type-level adds almost no value.
-}

{-------------------------------------------------------------------------------
    Constructors
-------------------------------------------------------------------------------}
{- HLINT ignore newStore "Use readTVarIO" -}
-- | An in-memory 'Store' from a mutable variable ('TVar').
-- Useful for testing.
newStore
    :: (MonadSTM m, MonadThrow m, Delta da, Query qa, Base da ~ World qa)
    => m (Store m qa da)
newStore = do
    ref <- newTVarIO $ Left $ toException NotInitialized
    let load = atomically (readTVar ref)
    pure $ Store
        { loadS   = load
        , queryS  = \q -> query q <$> (throwLeft =<< load)
        , writeS  = atomically . writeTVar ref . Right
        , updateS = \_ -> atomically . modifyTVar' ref . fmap . apply
        }

-- | Failure that occurs when calling 'loadS' on a 'newStore' that is empty.
data NotInitialized = NotInitialized deriving (Eq, Show)
instance Exception NotInitialized

-- | A 'Store' which supports 'loadS' and 'writeS',
-- but no fancy query or update operations.
type SimpleStore m a = Store m (Whole a) (Replace a)

-- | @mkSimpleStore loadS writeS@ constructs a 'SimpleStore'
-- from the given operations.
mkSimpleStore
    :: forall m a
     . (Monad m, MonadThrow m)
    => m (Either SomeException a)
    -> (a -> m ())
    -> SimpleStore m a
mkSimpleStore loadS writeS =
    mkUpdateStore loadS writeS update'
  where
    update' _ (Replace a) = writeS a

-- | A 'Store' whose focus lies on updating the value rather than querying it.
type UpdateStore m da = Store m (Whole (Base da)) da

-- | @mkUpdateStore loadS writeS updateS@ constructs an 'UpdateStore'
-- from the given operations.
mkUpdateStore
    :: forall m a da
     . (Monad m, MonadThrow m, a ~ Base da, Delta da)
    => m (Either SomeException a)
    -> (a -> m ())
    -> (Maybe a -> da -> m ())
    -> UpdateStore m da
mkUpdateStore loadS writeS updateS =
    Store{loadS, queryS=query', writeS, updateS}
  where
    query' :: forall b. Whole a b -> m b
    query' Whole = loadS >>= throwLeft

-- | @mkQueryStore queryS store@ constructs a 'Store'
-- from a query and an 'UpdateStore'.
mkQueryStore :: forall m qa da
     . (MonadThrow m, Delta da, Query qa, Base da ~ World qa)
    => (forall b. qa b -> m b)
    -> UpdateStore m da
    -> Store m qa da
mkQueryStore queryS Store{loadS,writeS,updateS} =
    Store{queryS,loadS,writeS,updateS}

{-------------------------------------------------------------------------------
    Query
-------------------------------------------------------------------------------}
-- | A /query/ @qa b@ for the type @a ~ World qa@
-- corresponds to a function @a -> b@.
-- Put differently, a query allows us to extract some information of type @b@
-- from the larger type @a@.
class Query qa where
    type family World qa
    query :: qa b -> World qa -> b

-- | The query that retrieves the whole value.
data Whole a b where
    Whole :: Whole a a

instance Query (Whole a) where
    type World (Whole a) = a
    query Whole a = a

{-------------------------------------------------------------------------------
    Combinators
-------------------------------------------------------------------------------}
-- | Add a caching layer to a 'Store'.
--
-- Access to the underlying 'Store' is enforced to be sequential,
-- but the cache can be accessed in parallel.
-- FIXME: There is still a small race condition where the cache
-- could be written twice before it is filled. 🤔
-- TODO: Think about whether it is really necessary to handle concurrency here.
-- I think the answer is "yes", but only because the mutable variables
-- provided by the monad @m@ do not work together with e.g. SQL transactions.
newCachedStore
    :: forall m qa da
      . ( MonadSTM m, MonadThrow m, MonadEvaluate m
        , Delta da, Query qa, Base da ~ World qa
        )
    => Store m qa da -> m (Store m qa da)
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
    let writeCache = writeTVar cache

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
        , queryS = \q -> query q <$> (throwLeft =<< load)
        , writeS = \a -> withLock $ do
            atomically $ writeCache (Just a)
            writeS a
        , updateS = updateLoad load throwIO $ \old delta -> withLock $ do
            new <- evaluate $ apply delta old
            atomically $ writeCache $ Just new
            updateS (Just old) delta
        }

-- | Store one type in the 'Store' of another type by using an 'Embedding'.
embedStore :: (MonadSTM m, MonadMask m, Delta da)
    => Embedding da db -> UpdateStore m db -> m (UpdateStore m da)
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
    pure $ mkUpdateStore load write update

-- | Store one type in the 'Store' of another type by using an 'Embedding'.
--
-- Note: This function is exported for testing and documentation only,
-- use the more efficient 'embedStore' instead.
embedStore'
    :: (Monad m, MonadThrow m)
    => Embedding' da db -> UpdateStore m db -> UpdateStore m da
embedStore' Embedding'{load,write,update} Store{loadS,writeS,updateS} =
    let
        loadL =  (load =<<) <$> loadS
        updateL ma da = case ma of
            Just a -> loadS >>= \case
                Left  _ -> pure ()
                Right b -> updateS (Just b) (update a b da)
            Nothing -> do
                ea <- loadL
                case ea of
                    Left  e -> throwIO e
                    Right a -> updateL (Just a) da
    in  mkUpdateStore loadL (writeS . write) updateL

-- | Combine two 'Stores' into a 'Store' for pairs.
--
-- TODO: Handle the case where 'writeS' or 'updateS' throw an exception
-- and partially break the 'Store'.
pairStores
    :: Monad m
    => Store m qa da
    -> Store m qb db
    -> Store m (qa :+: qb) (da,db)
pairStores sa sb = Store
    { loadS = liftA2 (,) <$> loadS sa <*> loadS sb
    , queryS = \case
        L1 qa -> queryS sa qa
        R1 qb -> queryS sb qb
    , writeS = \(a,b) -> writeS sa a >> writeS sb b
    , updateS = \mi (da,db) ->
        case mi of
            Nothing -> updateS sa Nothing da >> updateS sb Nothing db
            Just (a,b) -> updateS sa (Just a) da >> updateS sb (Just b) db
    }

{-------------------------------------------------------------------------------
    Helpers
-------------------------------------------------------------------------------}
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

-- | Throw 'Left' as an exception in the monad.
throwLeft :: MonadThrow m => Either SomeException b -> m b
throwLeft = \case
    Left (SomeException e) -> throwIO e
    Right a -> pure a

-- | Helper for implementing `updateS`.
-- Call 'loadS' from a 'Store' if the value is not already given in memory.
loadWhenNothing
    :: (Monad m, MonadThrow m, Delta da)
    => Maybe (Base da) -> Store m qa da -> m (Base da)
loadWhenNothing (Just a) _ = pure a
loadWhenNothing Nothing store = loadS store >>= throwLeft

updateSequence
    :: (Monad m, Delta delta)
    => (Base delta -> delta -> m ())
    -> Base delta
    -> [delta]
    -> m ()
updateSequence f s = foldM_ update' s . reverse
  where
    update' s' da = f s' da >> pure (da `apply` s')
