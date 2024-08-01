{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

Monad for atomic SQLite operations.
-}
module Database.Table.SQLite.Simple.Monad
    ( -- * SQL Connection
      Connection
    , open
    , close
    , withConnection

    -- * SQL monad
    , SqlM
    , runSqlM
    , rawSqlite
    , SqlException (..)
    ) where

import Prelude

import Control.Concurrent.MVar
    ( MVar
    , newMVar
    , withMVar
    )
import Control.Exception
    ( Exception (..)
    , SomeException (..)
    )
import Control.Monad.Class.MonadThrow
    ( MonadCatch (..)
    , MonadThrow (..)
    )
import Control.Monad.Trans.Reader
    ( ReaderT (..)
    )

import qualified Database.SQLite.Simple as Sqlite

{-------------------------------------------------------------------------------
    SQL Connection
-------------------------------------------------------------------------------}
-- | A connection to an SQLite
data Connection = Connection
    { filepath :: FilePath
    , connection :: Sqlite.Connection
    , lock :: MVar ()
    }

{-| Equivalent of 'Sqlite.open'.

Open a database connection to a given file.
Will throw an exception if it cannot connect.

Every 'open' must be closed with a call to 'close'.

If you specify ":memory:" or an empty string as the input filename,
then a private, temporary in-memory database is created for the connection.
This database will vanish when you close the connection.
-}
open :: FilePath -> IO Connection
open filepath = do
    connection <- Sqlite.open filepath
    lock <- newMVar ()
    pure $ Connection{filepath, connection, lock}

-- | Equivalent of 'Sqlite.close'.
-- Close a database connection.
close :: Connection -> IO ()
close Connection{connection} = Sqlite.close connection

-- | Equivalent of 'Sqlite.withConnection'.
--
-- Opens a database connection, executes an action using this connection,
-- and closes the connection, even in the presence of exceptions.
withConnection :: FilePath -> (Connection -> IO a) -> IO a
withConnection s = bracket (open s) close

{-------------------------------------------------------------------------------
    SQL monad
-------------------------------------------------------------------------------}
{- | Monad for database operations.
--
-- This monad includes the following effects:
--
-- * mutable state (of the database)
-- * exceptions
-}
newtype SqlM a = SqlM (ReaderT Sqlite.Connection IO a)
    deriving newtype (Functor, Applicative, Monad)
    deriving newtype (MonadThrow, MonadCatch)

{- | Atomically run a computation from the 'SqlM' monad.

* exceptions — The change to the database state performed by 'SqlM'
  completes either fully or not at all.
  If the computation throws an exception, then any intermediate
  state change is rolled back.
  (Essentially, the computation is wrapped in 'Sqlite.withTransaction'.)
* concurrency — A lock ensures that multiple calls to 'runSqlM'
  are run in sequence.
-}
runSqlM :: SqlM a -> Connection -> IO a
runSqlM (SqlM action) Connection{lock,connection} =
    withMVar lock $ \_ ->
        Sqlite.withTransaction connection $
            runReaderT action connection

-- | Wrap a function from "Database.SQLite.Simple" in 'SqlM'.
rawSqlite :: (Sqlite.Connection -> IO a) -> SqlM a
rawSqlite = SqlM . ReaderT

-- | Union of exceptions that can occur with "Database.SQLite.Simple".
data SqlException
    = SqlFormatError Sqlite.FormatError
    | SqlResultError Sqlite.ResultError
    | SqlSQLError Sqlite.SQLError
    deriving (Eq, Show)

-- | When converting to and from 'SomeException',
-- the constructors of the 'SqlException' type are stripped.
-- In other words, the type 'SqlException' represents a structural union,
-- not a nominal sum of the individual exceptions.
-- This makes it easier to catch the individual exceptions as a union.
--
-- Example:
--
-- > throw (Sqlite.SQLError Sqlite.ErrorIOData "" "")
-- >     `catch` \e -> print (e :: SqlException)
instance Exception SqlException where
    toException (SqlFormatError e) = SomeException e
    toException (SqlResultError e) = SomeException e
    toException (SqlSQLError e) = SomeException e
    fromException e0
        | Just e <- fromException e0 = Just $ SqlFormatError e
        | Just e <- fromException e0 = Just $ SqlResultError e
        | Just e <- fromException e0 = Just $ SqlSQLError e
        | otherwise = Nothing
