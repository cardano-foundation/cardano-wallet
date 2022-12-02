{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Database.Persist.Delta (
    -- * Synopsis
    -- | Manipulating SQL database tables using delta encodings
    -- via the "persistent" package.

    -- * Store
    newEntityStore, newSqlStore
    ) where

import Prelude hiding
    ( all )

import Control.Monad
    ( forM_, void, when )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Data.Bifunctor
    ( first )
import Data.DBVar
    ( Store (..) )
import Data.Delta
    ( Delta (..) )
import Data.Proxy
    ( Proxy (..) )
import Data.Table
    ( DeltaDB (..), Pile (..), Table (..) )
import Database.Persist
    ( Filter, Key, PersistRecordBackend, ToBackendKey )
import Database.Persist.Sql
    ( SqlBackend, SqlPersistM, fromSqlKey, toSqlKey )
import Database.Schema
    ( (:.) (..), Col (..), IsRow, Primary (..) )
import Say
    ( say, sayShow )

-- FIXME: Replace with IOSim stuff later.
import Data.IORef
    ( newIORef, readIORef, writeIORef )

import qualified Data.Table as Table
import qualified Database.Persist as Persist
import qualified Database.Schema as Sql

{-------------------------------------------------------------------------------
    Database operations
-------------------------------------------------------------------------------}
-- | Helper abstraction for a Database backend
data Database m key row = Database
    { selectAll   :: m [(key, row)]
    , deleteAll   :: m ()
    , repsertMany :: [(key, row)] -> m ()
    , deleteOne   :: key -> m ()
    , updateOne   :: (key, row) -> m ()
    }

-- | Database table for 'Entity'.
persistDB
    :: forall row. ( PersistRecordBackend row SqlBackend
    , ToBackendKey SqlBackend row )
    => Database SqlPersistM Int row
persistDB = Database
    { selectAll = map toPair <$> Persist.selectList all []
    , deleteAll = Persist.deleteWhere all
    , repsertMany = Persist.repsertMany . map (first toKey)
    , deleteOne = Persist.delete . toKey
    , updateOne = \(key,val) -> Persist.replace (toKey key) val
    }
  where
    all = [] :: [Filter row]

    toPair (Persist.Entity key val) = (fromKey key, val)

    fromKey = fromIntegral . fromSqlKey
    toKey :: Int -> Key row
    toKey = toSqlKey . fromIntegral

-- | SQL database backend
sqlDB
    :: forall row. (IsRow row, IsRow (row :. Col "id" Primary))
    => Database SqlPersistM Int row
sqlDB = Database
    { selectAll = map toPair <$> Sql.callSql Sql.selectAll
    , deleteAll = Sql.runSql $ Sql.deleteAll proxy
    , repsertMany = \zs -> forM_ zs $
        Sql.runSql . Sql.repsertOne . fromPair
    , deleteOne = Sql.runSql . Sql.deleteOne proxy . Col . Primary
    , updateOne = Sql.runSql . Sql.updateOne . fromPair
    }
  where
    proxy = Proxy :: Proxy row

    fromPair :: (Int,row) -> (row :. Col "id" Primary)
    fromPair (key,row) = row :. (Col (Primary key) :: Col "id" Primary)

    toPair :: (row :. Col "id" Primary) -> (Int,row)
    toPair (row :. Col (Primary key)) = (key,row)

{-------------------------------------------------------------------------------
    Database operations
-------------------------------------------------------------------------------}
-- | Construct a 'Store' from an SQL table.
--
-- The unique IDs will be stored in a column "id" at the end of
-- each row in the database table.
newSqlStore
    :: (MonadIO m, IsRow row, IsRow (row :. Col "id" Primary), Show row)
    => m (Store SqlPersistM [DeltaDB Int row])
newSqlStore = newDatabaseStore sqlDB

-- | Construct a 'Store' for 'Entity'.
--
-- FIXME: This function should also do \"migrations\", i.e.
-- create the database table in the first place.
newEntityStore
    :: forall row m.
    ( PersistRecordBackend row SqlBackend
    , ToBackendKey SqlBackend row, Show row
    , MonadIO m )
    => m (Store SqlPersistM [DeltaDB Int row])
newEntityStore = newDatabaseStore persistDB

-- | Helper function to create a 'Store' using a 'Database' backend.
newDatabaseStore
    :: forall m n row. (MonadIO m, MonadIO n, Show row)
    => Database m Int row
    -> n (Store m [DeltaDB Int row])
newDatabaseStore db = do
    ref <- liftIO $ newIORef Nothing
    let rememberSupply table = liftIO $ writeIORef ref $ Just $ uids table
    pure $ Store
        { loadS   = do
            debug $ do
                say "\n** loadS"
                liftIO . print =<< selectAll db
            -- read database table, preserve keys
            table <- Table.fromRows <$> selectAll db
            -- but use our own unique ID supply
            liftIO (readIORef ref) >>= \case
                Just supply  -> pure $ Right table{uids = supply}
                Nothing      -> do
                    rememberSupply table
                    pure $ Right table
        , writeS  = \table -> void $ do
            deleteAll db -- delete any old data in the table first
            repsertMany db $ getPile $ Table.toRows table
            rememberSupply table
        , updateS = \table ds -> do
            debug $ do
                say "\n** updateS table deltas"
                sayShow table
                sayShow ds
            mapM_ (update1 table) ds
            rememberSupply (apply ds table) -- need to use updated supply
        }
  where
    debug = when False

    update1 _ (InsertManyDB zs) = void $ repsertMany db zs
    update1 _ (DeleteManyDB ks) = forM_ ks $ deleteOne db
    update1 _ (UpdateManyDB zs) = forM_ zs $ updateOne db

{- Note [Unique ID supply in newDBStore]

We expect that updating the store and loading the value
is the same as first loading the value and then apply the delta,
i.e. we expect that the two actions

    loadS >>= \a -> updateS a da >>= loadS
    loadS >>= \a -> pure $ apply da a

are operationally equivalent.
However, this is only the case if we keep track of the supply
of unique IDs for the table! Otherwise, loading the table
from the database again can mess up the supply.
-}
-- FIXME: For clarity, we may want to implement this in terms
-- of a product of stores ("semidirect product").
