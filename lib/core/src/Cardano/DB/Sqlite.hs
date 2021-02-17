{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- HLINT ignore "Redundant flip" -}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- An implementation of the DBLayer which uses Persistent and SQLite.

module Cardano.DB.Sqlite
    ( SqliteContext (..)
    , newSqliteContext
    , newInMemorySqliteContext

    -- * ConnectionPool
    , ConnectionPool
    , withConnectionPool

    -- * Helpers
    , chunkSize
    , dbChunked
    , dbChunked'
    , handleConstraint
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
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.DB.Sqlite.Delete
    ( DeleteSqliteDatabaseLog )
import Cardano.Wallet.Logging
    ( BracketLog, bracketTracer )
import Control.Monad
    ( join, mapM_, void, when )
import Control.Monad.IO.Unlift
    ( MonadUnliftIO (..) )
import Control.Monad.Logger
    ( LogLevel (..) )
import Control.Retry
    ( RetryStatus (..)
    , constantDelay
    , limitRetriesByCumulativeDelay
    , recovering
    )
import Control.Tracer
    ( Tracer, contramap, traceWith )
import Data.Aeson
    ( ToJSON (..) )
import Data.Function
    ( (&) )
import Data.List
    ( isInfixOf )
import Data.List.Split
    ( chunksOf )
import Data.Pool
    ( Pool, createPool, destroyAllResources, withResource )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Time.Clock
    ( NominalDiffTime )
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
    , entityFields
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
    ( fmt, ordinalF, (+|), (+||), (|+), (||+) )
import GHC.Generics
    ( Generic )
import System.Log.FastLogger
    ( fromLogStr )
import UnliftIO.Compat
    ( handleIf, mkRetryHandler )
import UnliftIO.Exception
    ( Exception, bracket, bracket_, handleJust, mask_, tryJust )
import UnliftIO.MVar
    ( newMVar, withMVarMasked )

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
    { runQuery :: forall a. SqlPersistT IO a -> IO a
    -- ^ 'safely' run a query with logging and lock-protection
    , dbFile :: Maybe FilePath
    -- ^ The actual database file, if any. If none, runs in-memory
    }

type ConnectionPool = Pool (SqlBackend, Sqlite.Connection)

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
handleConstraint :: MonadUnliftIO m => e -> m a -> m (Either e a)
handleConstraint e = handleJust select handler . fmap Right
  where
    select (SqliteException ErrorConstraint _ _) = Just ()
    select _ = Nothing
    handler = const . pure  . Left $ e

{-------------------------------------------------------------------------------
                           Internal / Database Setup
-------------------------------------------------------------------------------}

newInMemorySqliteContext
    :: Tracer IO DBLog
    -> [ManualMigration]
    -> Migration
    -> IO SqliteContext
newInMemorySqliteContext tr manualMigrations autoMigration = do
    conn <- Sqlite.open connStr
    mapM_ (`executeManualMigration` conn) manualMigrations
    unsafeBackend <- wrapConnectionInfo info conn (queryLogFunc tr)
    void $ runSqlConn (runMigrationQuiet autoMigration) unsafeBackend

    let observe :: forall a. IO a -> IO a
        observe = bracketTracer (contramap MsgRun tr)

    -- We still use a lock with the in-memory database to protect it from
    -- concurrent accesses and ensure database integrity in case where multiple
    -- threads would be reading/writing from/to it.
    lock <- newMVar unsafeBackend
    let runQuery :: forall a. SqlPersistT IO a -> IO a
        runQuery cmd = withMVarMasked lock (observe . runSqlConn cmd)

    return $ SqliteContext { runQuery, dbFile }
  where
    dbFile = Nothing
    connStr = sqliteConnStr dbFile
    info = mkSqliteConnectionInfo connStr

-- | Sets up query logging and timing, runs schema migrations if necessary and
-- provide a safe 'SqliteContext' for interacting with the database.
newSqliteContext
    :: Tracer IO DBLog
    -> ConnectionPool
    -> [ManualMigration]
    -> Migration
    -> FilePath
    -> IO (Either MigrationError SqliteContext)
newSqliteContext tr pool manualMigrations autoMigration fp = do
    migrationResult <- withResource pool $ \(backend, conn) -> do
        let executeAutoMigration = runSqlConn (runMigrationQuiet autoMigration) backend
        migrationResult <- withForeignKeysDisabled tr conn $ do
            mapM_ (`executeManualMigration` conn) manualMigrations
            executeAutoMigration
                & tryJust (matchMigrationError @PersistException)
                & tryJust (matchMigrationError @SqliteException)
                & fmap join
        traceWith tr $ MsgMigrations $ fmap length migrationResult
        return migrationResult
    return $ case migrationResult of
        Left e  -> Left e
        Right{} ->
            let observe :: IO a -> IO a
                observe = bracketTracer (contramap MsgRun tr)

               -- runSqlConn is guarded with a lock because it's not threadsafe in
               -- general.It is also masked, so that the SqlBackend state is not
               -- corrupted if a thread gets cancelled while running a query.
               -- See: https://github.com/yesodweb/persistent/issues/981
               --
               -- Note that `withResource` does already mask async exception but
               -- only for dealing with the pool resource acquisition. The action
               -- is then ran unmasked with the acquired resource. If an
               -- asynchronous exception occurs (or actually any exception), the
               -- resource is NOT placed back in the pool.
                runQuery :: SqlPersistT IO a -> IO a
                runQuery cmd = withResource pool $
                    mask_ . observe . retryOnBusy tr . runSqlConn cmd . fst

            in Right $ SqliteContext { runQuery, dbFile = Just fp }

-- | Finalize database statements and close the database connection.
--
-- If the database connection is still in use, it will retry for up to a minute,
-- to let other threads finish up.
--
-- This function is idempotent: if the database connection has already been
-- closed, calling this function will exit without doing anything.
destroySqliteBackend
    :: Tracer IO DBLog
    -> SqlBackend
    -> FilePath
    -> IO ()
destroySqliteBackend tr sqlBackend dbFile = do
    traceWith tr (MsgCloseSingleConnection dbFile)
    retryOnBusy tr (close' sqlBackend)
        & handleIf isAlreadyClosed
            (traceWith tr . MsgIsAlreadyClosed . showT)
        & handleIf statementAlreadyFinalized
            (traceWith tr . MsgStatementAlreadyFinalized . showT)
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

-- | Retry an action if the database yields an 'SQLITE_BUSY' error.
--
-- From <https://www.sqlite.org/rescode.html#busy>
--
--     The SQLITE_BUSY result code indicates that the database file could not be
--     written (or in some cases read) because of concurrent activity by some
--     other database connection, usually a database connection in a separate
--     process.
--
--     For example, if process A is in the middle of a large write transaction
--     and at the same time process B attempts to start a new write transaction,
--     process B will get back an SQLITE_BUSY result because SQLite only supports
--     one writer at a time. Process B will need to wait for process A to finish
--     its transaction before starting a new transaction. The sqlite3_busy_timeout()
--     and sqlite3_busy_handler() interfaces and the busy_timeout pragma are
--     available to process B to help it deal with SQLITE_BUSY errors.
--
retryOnBusy :: Tracer IO DBLog -> IO a -> IO a
retryOnBusy tr action =
    recovering policy (mkRetryHandler isBusy) $ \RetryStatus{rsIterNumber} -> do
        when (rsIterNumber > 0) $ traceWith tr (MsgRetryOnBusy rsIterNumber)
        action
  where
    isBusy (SqliteException name _ _) = pure (name == Sqlite.ErrorBusy)
    policy = limitRetriesByCumulativeDelay (60000*ms) $ constantDelay (25*ms)
    ms = 1000 -- microseconds in a millisecond

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

withConnectionPool
    :: Tracer IO DBLog
    -> FilePath
    -> (ConnectionPool -> IO a)
    -> IO a
withConnectionPool tr fp =
    bracket (newConnectionPool tr fp) destroyConnectionPool

newConnectionPool
    :: Tracer IO DBLog
    -> FilePath
    -> IO ConnectionPool
newConnectionPool tr fp = do
    let connStr = sqliteConnStr (Just fp)
    let info = mkSqliteConnectionInfo connStr

    traceWith tr $ MsgWillOpenDB (Just fp)

    let acquireConnection = do
            conn <- Sqlite.open connStr
            (,conn) <$> wrapConnectionInfo info conn (queryLogFunc tr)

    let releaseConnection = \(backend, _) -> do
            destroySqliteBackend tr backend fp

    createPool
        acquireConnection
        releaseConnection
        numberOfStripes
        timeToLive
        maximumConnections
  where
    numberOfStripes = 1
    maximumConnections = 10
    timeToLive = 600 {- 10 minutes -} :: NominalDiffTime

destroyConnectionPool :: Pool a -> IO ()
destroyConnectionPool = destroyAllResources

sqliteConnStr :: Maybe FilePath -> Text
sqliteConnStr = maybe ":memory:" T.pack

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data DBLog
    = MsgMigrations (Either MigrationError Int)
    | MsgQuery Text Severity
    | MsgRun BracketLog
    | MsgCloseSingleConnection FilePath
    | MsgDestroyConnectionPool FilePath
    | MsgWillOpenDB (Maybe FilePath)
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
    | MsgFoundDatabase FilePath Text
    | MsgUnknownDBFile FilePath
    | MsgRetryOnBusy Int
    deriving (Generic, Show, Eq, ToJSON)

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

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

instance HasPrivacyAnnotation DBLog
instance HasSeverityAnnotation DBLog where
    getSeverityAnnotation ev = case ev of
        MsgMigrations (Right 0) -> Debug
        MsgMigrations (Right _) -> Notice
        MsgMigrations (Left _) -> Error
        MsgQuery _ sev -> sev
        MsgRun _ -> Debug
        MsgCloseSingleConnection _ -> Info
        MsgDestroyConnectionPool _ -> Notice
        MsgWillOpenDB _ -> Info
        MsgDatabaseReset -> Notice
        MsgIsAlreadyClosed _ -> Warning
        MsgStatementAlreadyFinalized _ -> Warning
        MsgWaitingForDatabase _ _ -> Info
        MsgRemovingInUse _ _ -> Notice
        MsgRemoving _ -> Info
        MsgRemovingDatabaseFile _ msg -> getSeverityAnnotation msg
        MsgManualMigrationNeeded{} -> Notice
        MsgManualMigrationNotNeeded{} -> Debug
        MsgUpdatingForeignKeysSetting{} -> Debug
        MsgFoundDatabase _ _ -> Info
        MsgUnknownDBFile _ -> Notice
        MsgRetryOnBusy n | n <= 1 -> Debug
        MsgRetryOnBusy n | n <= 3 -> Notice
        MsgRetryOnBusy _ -> Warning

instance ToText DBLog where
    toText = \case
        MsgMigrations (Right 0) ->
            "No database migrations were necessary."
        MsgMigrations (Right n) ->
            fmt $ ""+||n||+" migrations were applied to the database."
        MsgMigrations (Left err) ->
            "Failed to migrate the database: " <> getMigrationErrorMessage err
        MsgQuery stmt _ -> stmt
        MsgRun b -> "Running database action - " <> toText b
        MsgWillOpenDB fp -> "Will open db at " <> (maybe "in-memory" T.pack fp)
        MsgDatabaseReset ->
            "Non backward compatible database found. Removing old database \
            \and re-creating it from scratch. Ignore the previous error."
        MsgCloseSingleConnection fp ->
            "Closing single database connection ("+|fp|+")"
        MsgDestroyConnectionPool fp ->
            "Destroy database connection pool ("+|fp|+")"
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
        MsgFoundDatabase _file wid ->
            "Found existing wallet: " <> wid
        MsgUnknownDBFile file -> mconcat
            [ "Found something other than a database file in "
            , "the database folder: ", T.pack file
            ]
        MsgRetryOnBusy n ->
            let nF = ordinalF n in
            "Retrying db query because db was busy for the " +| nF |+ " time."

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
dbChunked
    :: forall record b. PersistEntity record
    => ([record] -> SqlPersistT IO b)
    -> [record]
    -> SqlPersistT IO ()
dbChunked = chunkedM (chunkSizeFor @record)

-- | Like 'dbChunked', but allows bundling elements with a 'Key'. Useful when
-- used with 'repsertMany'.
dbChunked'
    :: forall record b. PersistEntity record
    => ([(Key record, record)] -> SqlPersistT IO b)
    -> [(Key record, record)]
    -> SqlPersistT IO ()
dbChunked' = chunkedM (chunkSizeFor @record)

-- | Given an action which takes a list of items, and a list of items, run that
-- action multiple times with the input list cut into chunks.
chunkedM
    :: Monad m
    => Int -- ^ Chunk size
    -> ([a] -> m b) -- ^ Action to run on values
    -> [a] -- ^ The values
    -> m ()
chunkedM n f = mapM_ f . chunksOf n

-- | Maximum number of variables allowed in a single SQL statement
--
-- See also 'dbChunked'.
chunkSize :: Int
chunkSize = 999


-- | Size of chunks when inserting, updating or deleting many rows at once.
-- Worst-case is when all columns of a particular table gets updated / inserted,
-- thus to be safe we must ensure that we do not act on more than `chunkSize /
-- cols` variables.
--
-- See also 'dbChunked'.
chunkSizeFor :: forall record. PersistEntity record => Int
chunkSizeFor = chunkSize `div` cols
  where
    cols = length $ entityFields $ entityDef (Proxy @record)
