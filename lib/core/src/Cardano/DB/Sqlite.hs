{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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
    , chunkSize
    , dbChunked
    , destroyDBLayer
    , handleConstraint
    , startSqliteBackend
    , unsafeRunQuery

    -- * Manual Migration
    , ManualMigration (..)
    , MigrationError (..)
    , DBField(..)
    , tableName
    , fieldName
    , fieldType

    -- * Logging
    , DBLog (..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( DefinePrivacyAnnotation (..), DefineSeverity (..) )
import Cardano.DB.Sqlite.Delete
    ( DeleteSqliteDatabaseLog )
import Control.Concurrent.MVar
    ( newMVar, withMVar )
import Control.Exception
    ( Exception, bracket_, tryJust )
import Control.Monad
    ( join, mapM_, when )
import Control.Monad.Catch
    ( Handler (..), MonadCatch (..), handleIf, handleJust )
import Control.Monad.Logger
    ( LogLevel (..) )
import Control.Retry
    ( constantDelay, limitRetriesByCumulativeDelay, recovering )
import Control.Tracer
    ( Tracer, traceWith )
import Data.Aeson
    ( ToJSON (..) )
import Data.Function
    ( (&) )
import Data.List
    ( isInfixOf )
import Data.List.Split
    ( chunksOf )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Database.Persist.Sql
    ( DBName (..)
    , EntityField
    , LogFunc
    , Migration
    , PersistEntity (..)
    , PersistException
    , SqlType (..)
    , close'
    , entityDB
    , fieldDB
    , fieldSqlType
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

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.Persist.Sql as Persist
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
    recovering pol [const $ Handler isBusy] (const $ close' getSqlBackend)
        & handleIf isAlreadyClosed
            (traceWith trace . MsgIsAlreadyClosed . showT)
        & handleIf statementAlreadyFinalized
            (traceWith trace . MsgStatementAlreadyFinalized . showT)
  where
    isAlreadyClosed = \case
        -- Thrown when an attempt is made to close a connection that is already
        -- in the closed state:
        Sqlite.SqliteException Sqlite.ErrorMisuse _ _ -> True
        Sqlite.SqliteException {}                     -> False

    statementAlreadyFinalized = \case
        -- Thrown
        Persist.StatementAlreadyFinalized{} -> True
        Persist.Couldn'tGetSQLConnection{}  -> False

    showT :: Show a => a -> Text
    showT = T.pack . show

    isBusy (SqliteException name _ _) = pure (name == Sqlite.ErrorBusy)
    pol = limitRetriesByCumulativeDelay (60000*ms) $ constantDelay (25*ms)
    ms = 1000 -- microseconds in a millisecond

{-------------------------------------------------------------------------------
                           Internal / Database Setup
-------------------------------------------------------------------------------}

-- | Opens the SQLite database connection, sets up query logging and timing,
-- runs schema migrations if necessary.
startSqliteBackend
    :: ManualMigration
    -> Migration
    -> Tracer IO DBLog
    -> Maybe FilePath
    -> IO (Either MigrationError SqliteContext)
startSqliteBackend manualMigration autoMigration trace fp = do
    (backend, connection) <-
        createSqliteBackend trace fp manualMigration (queryLogFunc trace)
    lock <- newMVar ()
    let traceRun = traceWith trace . MsgRun
    let observe :: IO a -> IO a
        observe = bracket_ (traceRun False) (traceRun True)
    let runQuery :: SqlPersistT IO a -> IO a
        runQuery cmd = withMVar lock $ const $ observe $ runSqlConn cmd backend
    autoMigrationResult <-
        withForeignKeysDisabled trace connection
            $ runQuery (runMigrationQuiet autoMigration)
            & tryJust (matchMigrationError @PersistException)
            & tryJust (matchMigrationError @SqliteException)
            & fmap join
    traceWith trace $ MsgMigrations $ fmap length autoMigrationResult
    let ctx = SqliteContext backend runQuery fp trace
    case autoMigrationResult of
        Left e -> do
            destroyDBLayer ctx
            pure $ Left e
        Right _ -> pure $ Right ctx

-- | Run the given task in a context where foreign key constraints are
--   /temporarily disabled/, before re-enabling them.
--
withForeignKeysDisabled
    :: Tracer IO DBLog
    -> Sqlite.Connection
    -> IO a
    -> IO a
withForeignKeysDisabled t c =
    bracket_
        (updateForeignKeysSetting t c ForeignKeysDisabled)
        (updateForeignKeysSetting t c ForeignKeysEnabled)

-- | Specifies whether or not foreign key constraints are enabled, equivalent
--   to the Sqlite 'foreign_keys' setting.
--
-- When foreign key constraints are /enabled/, the database will enforce
-- referential integrity, and cascading deletes are enabled.
--
-- When foreign keys constraints are /disabled/, the database will not enforce
-- referential integrity, and cascading deletes are disabled.
--
-- See the following resource for more information:
-- https://www.sqlite.org/foreignkeys.html#fk_enable
--
data ForeignKeysSetting
    = ForeignKeysEnabled
        -- ^ Foreign key constraints are /enabled/.
    | ForeignKeysDisabled
        -- ^ Foreign key constraints are /disabled/.
    deriving (Eq, Generic, ToJSON, Show)

-- | Read the current value of the Sqlite 'foreign_keys' setting.
--
readForeignKeysSetting :: Sqlite.Connection -> IO ForeignKeysSetting
readForeignKeysSetting connection = do
    query <- Sqlite.prepare connection "PRAGMA foreign_keys"
    state <- Sqlite.step query >> Sqlite.columns query
    Sqlite.finalize query
    case state of
        [Persist.PersistInt64 0] -> pure ForeignKeysDisabled
        [Persist.PersistInt64 1] -> pure ForeignKeysEnabled
        unexpectedValue -> error $ mconcat
            [ "Unexpected result when querying the current value of "
            , "the Sqlite 'foreign_keys' setting: "
            , show unexpectedValue
            , "."
            ]

-- | Update the current value of the Sqlite 'foreign_keys' setting.
--
updateForeignKeysSetting
    :: Tracer IO DBLog
    -> Sqlite.Connection
    -> ForeignKeysSetting
    -> IO ()
updateForeignKeysSetting trace connection desiredValue = do
    traceWith trace $ MsgUpdatingForeignKeysSetting desiredValue
    query <- Sqlite.prepare connection $
        "PRAGMA foreign_keys = " <> valueToWrite <> ";"
    _ <- Sqlite.step query
    Sqlite.finalize query
    finalValue <- readForeignKeysSetting connection
    when (desiredValue /= finalValue) $ error $ mconcat
        [ "Unexpected error when updating the value of the Sqlite "
        , "'foreign_keys' setting. Attempted to write the value "
        , show desiredValue
        , " but retrieved the final value "
        , show finalValue
        , "."
        ]
  where
    valueToWrite = case desiredValue of
        ForeignKeysEnabled  -> "ON"
        ForeignKeysDisabled -> "OFF"

class Exception e => MatchMigrationError e where
    -- | Exception predicate for migration errors.
    matchMigrationError :: e -> Maybe MigrationError

instance MatchMigrationError PersistException where
    matchMigrationError e
        | mark `isInfixOf` msg = Just $ MigrationError $ T.pack msg
        | otherwise = Nothing
      where
        msg = show e
        mark = "Database migration: manual intervention required."

instance MatchMigrationError SqliteException where
    matchMigrationError (SqliteException ErrorConstraint _ msg) =
        Just $ MigrationError msg
    matchMigrationError _ =
        Nothing

-- | Encapsulates a manual migration action (or sequence of actions) to be
--   performed immediately after an SQL connection is initiated.
--
newtype ManualMigration = ManualMigration
    { executeManualMigration :: Sqlite.Connection -> IO () }

createSqliteBackend
    :: Tracer IO DBLog
    -> Maybe FilePath
    -> ManualMigration
    -> LogFunc
    -> IO (SqlBackend, Sqlite.Connection)
createSqliteBackend trace fp migration logFunc = do
    let connStr = sqliteConnStr fp
    traceWith trace $ MsgConnStr connStr
    conn <- Sqlite.open connStr
    executeManualMigration migration conn
    backend <- wrapConnectionInfo (mkSqliteConnectionInfo connStr) conn logFunc
    pure (backend, conn)

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
    | MsgIsAlreadyClosed Text
    | MsgStatementAlreadyFinalized Text
    | MsgWaitingForDatabase Text (Maybe Int)
    | MsgRemovingInUse Text Int
    | MsgRemoving Text
    | MsgRemovingDatabaseFile Text DeleteSqliteDatabaseLog
    | MsgManualMigrationNeeded DBField Text
    | MsgManualMigrationNotNeeded DBField
    | MsgUpdatingForeignKeysSetting ForeignKeysSetting
    deriving (Generic, Show, Eq, ToJSON)

data DBField where
    DBField
        :: forall record typ. (PersistEntity record)
        => EntityField record typ
        -> DBField

tableName :: DBField -> Text
tableName (DBField (_ :: EntityField record typ)) =
    unDBName $ entityDB $ entityDef (Proxy @record)

fieldName :: DBField -> Text
fieldName (DBField field) =
    unDBName $ fieldDB $ persistFieldDef field

fieldType :: DBField -> Text
fieldType (DBField field) =
    showSqlType $ fieldSqlType $ persistFieldDef field

showSqlType :: SqlType -> Text
showSqlType = \case
    SqlString  -> "VARCHAR"
    SqlInt32   -> "INTEGER"
    SqlInt64   -> "INTEGER"
    SqlReal    -> "REAL"
    SqlDay     -> "DATE"
    SqlTime    -> "TIME"
    SqlDayTime -> "TIMESTAMP"
    SqlBlob    -> "BLOB"
    SqlBool    -> "BOOLEAN"
    SqlOther t -> t
    SqlNumeric precision scale -> T.concat
        [ "NUMERIC("
        , T.pack (show precision)
        , ","
        , T.pack (show scale), ")"
        ]

instance Show DBField where
    show field = T.unpack (tableName field <> "." <> fieldName field)

instance Eq DBField where
    field0 == field1 = show field0 == show field1

instance ToJSON DBField where
    toJSON = Aeson.String . fieldName

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
        MsgIsAlreadyClosed _ -> Warning
        MsgStatementAlreadyFinalized _ -> Warning
        MsgWaitingForDatabase _ _ -> Info
        MsgRemovingInUse _ _ -> Notice
        MsgRemoving _ -> Info
        MsgRemovingDatabaseFile _ msg -> defineSeverity msg
        MsgManualMigrationNeeded{} -> Notice
        MsgManualMigrationNotNeeded{} -> Debug
        MsgUpdatingForeignKeysSetting{} -> Debug

instance ToText DBLog where
    toText = \case
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
        MsgIsAlreadyClosed msg ->
            "Attempted to close an already closed connection: " <> msg
        MsgStatementAlreadyFinalized msg ->
            "Statement already finalized: " <> msg
        MsgWaitingForDatabase wid Nothing ->
            "Database "+|wid|+" is ready to be deleted"
        MsgWaitingForDatabase wid (Just count) ->
            "Waiting for "+|count|+" withDatabase "+|wid|+" call(s) to finish"
        MsgRemovingInUse wid count ->
            "Timed out waiting for "+|count|+" withDatabase "+|wid|+" call(s) to finish. " <>
            "Attempting to remove the database anyway."
        MsgRemoving wid ->
            "Removing wallet's database. Wallet id was " <> wid
        MsgRemovingDatabaseFile wid msg ->
            "Removing " <> wid <> ": " <> toText msg
        MsgManualMigrationNeeded field value -> mconcat
            [ tableName field
            , " table does not contain required field '"
            , fieldName field
            , "'. "
            , "Adding this field with a default value of "
            , value
            , "."
            ]
        MsgManualMigrationNotNeeded field -> mconcat
            [ tableName field
            , " table already contains required field '"
            , fieldName field
            , "'."
            ]
        MsgUpdatingForeignKeysSetting value -> mconcat
            [ "Updating the foreign keys setting to: "
            , T.pack $ show value
            , "."
            ]

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
