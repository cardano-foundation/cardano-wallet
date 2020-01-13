{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant flip" #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- An implementation of the DBLayer which uses Persistent and SQLite.

module Cardano.DB.Sqlite
    ( SqliteContext (..)
    , MigrationError (..)
    , DBLog (..)
    , chunkSize
    , dbChunked
    , destroyDBLayer
    , handleConstraint
    , startSqliteBackend
    , unsafeRunQuery
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( DefinePrivacyAnnotation (..), DefineSeverity (..) )
import Control.Concurrent.MVar
    ( newMVar, withMVar )
import Control.Exception
    ( Exception, bracket_, tryJust )
import Control.Monad
    ( mapM_ )
import Control.Monad.Catch
    ( Handler (..), MonadCatch (..), handleIf, handleJust )
import Control.Monad.Logger
    ( LogLevel (..) )
import Control.Retry
    ( constantDelay, limitRetriesByCumulativeDelay, recovering )
import Control.Tracer
    ( Tracer, traceWith )
import Data.Aeson
    ( ToJSON )
import Data.Function
    ( (&) )
import Data.List
    ( isInfixOf )
import Data.List.Split
    ( chunksOf )
import Data.Maybe
    ( fromMaybe )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Database.Persist.Sql
    ( LogFunc
    , Migration
    , PersistException
    , close'
    , runMigrationQuiet
    , runSqlConn
    )
import Database.Persist.Sqlite
    ( SqlBackend, SqlPersistT, mkSqliteConnectionInfo, wrapConnectionInfo )
import Database.Sqlite
    ( Error (ErrorConstraint), SqliteException (SqliteException) )
import Fmt
    ( fmt, (+|), (+||), (|+), (||+) )
import GHC.Generics
    ( Generic )
import System.Log.FastLogger
    ( fromLogStr )

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.Sqlite as Sqlite

{-------------------------------------------------------------------------------
                            Sqlite connection set up
-------------------------------------------------------------------------------}

-- | Context for the SQLite 'DBLayer'.
data SqliteContext = SqliteContext
    { getSqlBackend :: SqlBackend
    -- ^ A handle to the Persistent SQL backend.
    , runQuery :: forall a. SqlPersistT IO a -> IO a
    -- ^ 'safely' run a query with logging and lock-protection
    , dbFile :: Maybe FilePath
    -- ^ The actual database file, if any. If none, runs in-memory
    , trace :: Tracer IO DBLog
    -- ^ A 'Tracer' for logging
    }

-- | Error type for when migrations go wrong after opening a database.
newtype MigrationError = MigrationError
    { getMigrationErrorMessage :: Text }
    deriving (Show, Eq, Generic, ToJSON)

instance Exception MigrationError

-- | Run a raw query from the outside using an instantiate DB layer. This is
-- completely unsafe because it breaks the abstraction boundary and can have
-- disastrous results on the database consistency.
unsafeRunQuery :: SqliteContext -> SqlPersistT IO a -> IO a
unsafeRunQuery = runQuery

queryLogFunc :: Tracer IO DBLog -> LogFunc
queryLogFunc tr _loc _source level str = traceWith tr (MsgQuery msg sev)
  where
    -- Filter out parameters which appear after the statement semicolon.
    -- They will contain sensitive material that we don't want in the log.
    stmt = B8.takeWhile (/= ';') $ fromLogStr str
    msg = T.decodeUtf8 stmt
    sev = case level of
        LevelDebug -> Debug
        LevelInfo -> Info
        LevelWarn -> Warning
        LevelError -> Error
        LevelOther _ -> Warning

-- | Run an action, and convert any Sqlite constraints exception into the given
-- error result. No other exceptions are handled.
handleConstraint :: MonadCatch m => e -> m a -> m (Either e a)
handleConstraint e = handleJust select handler . fmap Right
  where
      select (SqliteException ErrorConstraint _ _) = Just ()
      select _ = Nothing
      handler = const . pure  . Left $ e

-- | Finalize database statements and close the database connection.
--
-- If the database connection is still in use, it will retry for up to a minute,
-- to let other threads finish up.
--
-- This function is idempotent: if the database connection has already been
-- closed, calling this function will exit without doing anything.
--
destroyDBLayer :: SqliteContext -> IO ()
destroyDBLayer (SqliteContext {getSqlBackend, trace, dbFile}) = do
    traceWith trace (MsgClosing dbFile)
    handleIf
        isAlreadyClosed
        (const $ pure ())
        (recovering pol [const $ Handler isBusy] (const $ close' getSqlBackend))
  where
    isAlreadyClosed = \case
        -- Thrown when an attempt is made to close a connection that is already
        -- in the closed state:
        Sqlite.SqliteException Sqlite.ErrorMisuse _ _ -> True
        Sqlite.SqliteException {}                     -> False
    isBusy (SqliteException name _ _) = pure (name == Sqlite.ErrorBusy)
    pol = limitRetriesByCumulativeDelay (60000*ms) $ constantDelay (25*ms)
    ms = 1000 -- microseconds in a millisecond

{-------------------------------------------------------------------------------
                           Internal / Database Setup
-------------------------------------------------------------------------------}

-- | Opens the SQLite database connection, sets up query logging and timing,
-- runs schema migrations if necessary.
startSqliteBackend
    :: Migration
    -> Tracer IO DBLog
    -> Maybe FilePath
    -> IO (Either MigrationError SqliteContext)
startSqliteBackend migrateAll trace fp = do
    backend <- createSqliteBackend trace fp (queryLogFunc trace)
    lock <- newMVar ()
    let traceRun = traceWith trace . MsgRun
    let observe :: IO a -> IO a
        observe = bracket_ (traceRun False) (traceRun True)
    let runQuery :: SqlPersistT IO a -> IO a
        runQuery cmd = withMVar lock $ const $ observe $ runSqlConn cmd backend
    migrations <- runQuery (runMigrationQuiet migrateAll)
        & tryJust (isMigrationError @PersistException)
        & tryJust (isMigrationError @SqliteException)
    traceWith trace $ MsgMigrations (fmap length migrations)
    let ctx = SqliteContext backend runQuery fp trace
    case migrations of
        Left e -> do
            destroyDBLayer ctx
            pure $ Left e
        Right _ -> pure $ Right ctx

class Exception e => IsMigrationError e where
    -- | Exception predicate for migration errors.
    isMigrationError :: e -> Maybe MigrationError

instance IsMigrationError PersistException where
    isMigrationError e
        | mark `isInfixOf` msg = Just $ MigrationError $ T.pack msg
        | otherwise = Nothing
      where
        msg = show e
        mark = "Database migration: manual intervention required."

instance IsMigrationError SqliteException where
    isMigrationError (SqliteException ErrorConstraint _ msg) =
        Just $ MigrationError msg
    isMigrationError _ =
        Nothing

createSqliteBackend
    :: Tracer IO DBLog
    -> Maybe FilePath
    -> LogFunc
    -> IO SqlBackend
createSqliteBackend trace fp logFunc = do
    let connStr = sqliteConnStr fp
    traceWith trace $ MsgConnStr connStr
    conn <- Sqlite.open connStr
    wrapConnectionInfo (mkSqliteConnectionInfo connStr) conn logFunc

sqliteConnStr :: Maybe FilePath -> Text
sqliteConnStr = maybe ":memory:" T.pack

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data DBLog
    = MsgMigrations (Either MigrationError Int)
    | MsgQuery Text Severity
    | MsgRun Bool
    | MsgConnStr Text
    | MsgClosing (Maybe FilePath)
    | MsgDatabaseReset
    deriving (Generic, Show, Eq, ToJSON)

instance DefinePrivacyAnnotation DBLog
instance DefineSeverity DBLog where
    defineSeverity ev = case ev of
        MsgMigrations (Right 0) -> Debug
        MsgMigrations (Right _) -> Notice
        MsgMigrations (Left _) -> Error
        MsgQuery _ sev -> sev
        MsgRun _ -> Debug
        MsgConnStr _ -> Debug
        MsgClosing _ -> Debug
        MsgDatabaseReset -> Notice

instance ToText DBLog where
    toText msg = case msg of
        MsgMigrations (Right 0) ->
            "No database migrations were necessary."
        MsgMigrations (Right n) ->
            fmt $ ""+||n||+" migrations were applied to the database."
        MsgMigrations (Left err) ->
            "Failed to migrate the database: " <> getMigrationErrorMessage err
        MsgQuery stmt _ -> stmt
        MsgRun False -> "Running database action - Start"
        MsgRun True -> "Running database action - Finish"
        MsgConnStr connStr -> "Using connection string: " <> connStr
        MsgClosing fp -> "Closing database ("+|fromMaybe "in-memory" fp|+")"
        MsgDatabaseReset ->
            "Non backward compatible database found. Removing old database \
            \and re-creating it from scratch. Ignore the previous error."

{-------------------------------------------------------------------------------
                               Extra DB Helpers
-------------------------------------------------------------------------------}

-- | Convert a single DB "updateMany" (or similar) query into multiple
-- updateMany queries with smaller lists of values.
--
-- This is to prevent too many variables appearing in the SQL statement.
-- SQLITE_MAX_VARIABLE_NUMBER is 999 by default, and we will get a
-- "too many SQL variables" exception if that is exceeded.
--
-- We choose a conservative value 'chunkSize' << 999 because there can be
-- multiple variables per row updated.
dbChunked :: ([a] -> SqlPersistT IO b) -> [a] -> SqlPersistT IO ()
dbChunked = chunkedM chunkSize

-- | Given an action which takes a list of items, and a list of items, run that
-- action multiple times with the input list cut into chunks.
chunkedM
    :: Monad m
    => Int -- ^ Chunk size
    -> ([a] -> m b) -- ^ Action to run on values
    -> [a] -- ^ The values
    -> m ()
chunkedM n f = mapM_ f . chunksOf n

-- | Size of chunks when inserting, updating or deleting many rows at once. We
-- only act on `chunkSize` values at a time. See also 'dbChunked'.
chunkSize :: Int
chunkSize = 100
