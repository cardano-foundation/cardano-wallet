{-# LANGUAGE DataKinds #-}

module Cardano.Wallet.DB.Sqlite.MigrationNew
    ( runNewStyleMigrations
    , DBHandle(..)) where

import Prelude hiding
    ( id, (.) )

import Cardano.DB.Sqlite
    ( DBLog, withConnectionPool )
import Cardano.Wallet.DB.Migration
    ( Migration
    , MigrationInterface (..)
    , Version (..)
    , hoistMigration
    , runMigrations
    )
import Cardano.Wallet.DB.Sqlite.MigrationOld
    ( getSchemaVersion, putSchemaVersion )
import Control.Category
    ( Category (id), (.) )
import Control.Monad.Reader
    ( withReaderT )
import Control.Monad.Trans.Reader
    ( ReaderT )
import Control.Tracer
    ( Tracer )
import Data.Pool
    ( withResource )
import Database.Persist.Sql
    ( SqlBackend )
import Database.Persist.Sqlite
    ( SqlPersistT )
import Database.Sqlite
    ( Connection )
import System.Directory
    ( copyFile )

import qualified Cardano.Wallet.DB.Sqlite.MigrationOld as MigrateOld
import qualified Database.Sqlite as Sqlite

data DBHandle = DBHandle
    { dbConn :: Connection
    , dbBackend :: SqlBackend
    }

mkDBHandle :: (SqlBackend, Connection) -> DBHandle
mkDBHandle (backend, conn) = DBHandle conn backend

newMigrationInterface
    :: Tracer IO DBLog
    -> MigrationInterface IO DBHandle
newMigrationInterface tr =
    MigrationInterface
        { backupDatabaseFile = \fp v -> do
            let backupFile = fp <> ".v" <> show v <> ".bak"
            copyFile fp backupFile
        , withDatabaseFile = \fp f -> do
            withConnectionPool tr fp $ \pool ->
                withResource pool $ f . mkDBHandle
        , getVersion = getVersionNew . dbConn
        , setVersion = setVersionNew . dbConn
        }

oldToNewSchemaVersion :: MigrateOld.SchemaVersion -> Version
oldToNewSchemaVersion (MigrateOld.SchemaVersion v) = Version v

newToOldSchemaVersion :: Version -> MigrateOld.SchemaVersion
newToOldSchemaVersion (Version v) = MigrateOld.SchemaVersion v

getVersionNew :: Sqlite.Connection -> IO Version
getVersionNew = fmap oldToNewSchemaVersion . getSchemaVersion

setVersionNew :: Sqlite.Connection -> Version -> IO ()
setVersionNew conn = putSchemaVersion conn . newToOldSchemaVersion

noMigrations :: Migration m 2 2
noMigrations = id

_useSqlBackend
    :: Migration (SqlPersistT m) from to
    -> Migration (ReaderT DBHandle m) from to
_useSqlBackend = hoistMigration $ withReaderT dbBackend

runNewStyleMigrations :: Tracer IO DBLog -> FilePath -> IO ()
runNewStyleMigrations tr fp = runMigrations (newMigrationInterface tr) fp
    noMigrations
