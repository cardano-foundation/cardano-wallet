{-# LANGUAGE DataKinds #-}

module Cardano.Wallet.DB.Sqlite.Migration.New
    ( runNewStyleMigrations, newMigrationInterface
    ) where

import Prelude hiding
    ( id, (.) )

import Cardano.DB.Sqlite
    ( DBHandle (..), DBLog, ReadDBHandle, withDBHandle )
import Cardano.Wallet.DB.Migration
    ( Migration, MigrationInterface (..), Version (..), hoistMigration,
    runMigrations )
import Cardano.Wallet.DB.Sqlite.Migration.Old
    ( getSchemaVersion, putSchemaVersion )
import Cardano.Wallet.DB.Store.Delegations.Migration
    ( migrateDelegations )
import Control.Category
    ( Category (id), (.) )
import Control.Monad.Reader
    ( withReaderT )
import Control.Tracer
    ( Tracer )
import Database.Persist.Sqlite
    ( SqlPersistT )
import Database.Sqlite
    ( Connection )
import System.Directory
    ( copyFile )

import qualified Cardano.Wallet.DB.Sqlite.Migration.Old as Old

newMigrationInterface
    :: Tracer IO DBLog
    -> MigrationInterface IO DBHandle
newMigrationInterface tr =
    MigrationInterface
        { backupDatabaseFile = \fp v -> do
            let backupFile = fp <> ".v" <> show v <> ".bak"
            copyFile fp backupFile
        , withDatabaseFile = withDBHandle tr
        , getVersion = getVersionNew . dbConn
        , setVersion = setVersionNew . dbConn
        }

oldToNewSchemaVersion :: Old.SchemaVersion -> Version
oldToNewSchemaVersion (Old.SchemaVersion v) = Version v

newToOldSchemaVersion :: Version -> Old.SchemaVersion
newToOldSchemaVersion (Version v) = Old.SchemaVersion v

getVersionNew :: Connection -> IO Version
getVersionNew = fmap oldToNewSchemaVersion . getSchemaVersion

setVersionNew :: Connection -> Version -> IO ()
setVersionNew conn = putSchemaVersion conn . newToOldSchemaVersion

noMigrations :: Migration m 2 2
noMigrations = id

_useSqlBackend
    :: Migration (SqlPersistT m) from to
    -> Migration (ReadDBHandle m) from to
_useSqlBackend = hoistMigration $ withReaderT dbBackend

runNewStyleMigrations :: Tracer IO DBLog -> FilePath -> IO ()
runNewStyleMigrations tr fp =
    runMigrations (newMigrationInterface tr) fp
        $ migrateDelegations . noMigrations
