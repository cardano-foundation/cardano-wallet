{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
module Data.QueryStore
    ( QueryStore (..)
    , Query (..)
    , queryStoreProperty
    , untry
    ) where

import Prelude

import Control.Exception
    ( SomeException (..), throwIO )
import Control.Monad.IO.Class
    ( MonadIO (liftIO) )
import Data.Delta
    ( Delta (Base) )
import Data.Store
    ( Store (loadS), UpdateStore )

{-----------------------------------------------------------------------------
    General QueryStore abstraction
------------------------------------------------------------------------------}
{- |
A 'QueryStore' is a storage facility for a Haskell value of type @a ~@'Base'@ da@.
Typical use cases are a file or a database on the hard disk.

In addition, 'QueryStore' also allows reading /parts/ of the data through 'queryS'
— often, it is more efficient to read part of the data from disk
rather than first load the entire data through 'loadDB' into memory,
and then filtering the parts of interest.

The parts of the data are expressed through a type constructor @read@
— typically implemented as a GADT representing different read operations.
We expect that there is a function @query :: read b -> World a -> b@ which
applies the operation to a plain value.
Then, 'queryS' must satisfy

> ∀ qs read.  query read <$> (loadS . store) qs  =  queryS qs read

In other words, loading the value into memory and reading a part
is equivalent to reading it directly.
For notational simplicity, we make no attempt at codifying this expectation
in Haskell.

We stress that all these operations — especially 'updateS' and 'queryS' —
only exist in order to express control over the storage location
and in order to enable an efficient implementation.
Conceptually, a 'QueryStore' is very plain — it stores a single value of type
@a ~@'Base'@ da@, nothing more, nothing less.
(If you want to store multiple values, consider storing a 'Set' or 'Map'.)
-}
data QueryStore m qa da = QueryStore
    { store :: UpdateStore m da
    , queryS :: forall b. qa b -> m b
    }

class Query qa where
    type family World qa
    query :: qa b -> World qa -> b

queryStoreProperty
    :: (Eq b, Query qa, MonadFail m, Base da ~ World qa)
    => QueryStore m qa da
    -> qa b
    -> m Bool
queryStoreProperty QueryStore{store, queryS} r = do
    Right z <- loadS store
    (query r z ==) <$> queryS r

-- | Helper function to retry the exception reported by 'loadS'.
untry :: MonadIO m => m (Either SomeException a) -> m a
untry action = action >>= liftIO . \case
    Left (SomeException e) -> throwIO e
    Right a -> pure a
