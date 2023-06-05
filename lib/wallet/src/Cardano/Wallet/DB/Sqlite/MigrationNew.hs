module Cardano.Wallet.DB.Sqlite.MigrationNew (newMigrationInterface) where

import Prelude

import Cardano.DB.Sqlite
    ( DBLog, withConnectionPool )
import Cardano.Wallet.DB.Migration
    ( MigrationInterface (..), Version )
import Cardano.Wallet.DB.Sqlite.MigrationOld
    ( getSchemaVersion, putSchemaVersion )
import Control.Tracer
    ( Tracer )
import Data.Pool
    ( withResource )
import System.Directory
    ( copyFile )

import qualified Cardano.Wallet.DB.Sqlite.MigrationOld as MigrateOld
import qualified Database.Sqlite as Sqlite

newMigrationInterface
    :: Tracer IO DBLog
    -> MigrationInterface IO Sqlite.Connection
newMigrationInterface tr =
    MigrationInterface
        { backupDatabaseFile = \fp v -> do
            let backupFile = fp <> ".v" <> show v <> ".bak"
            copyFile fp backupFile
        , withDatabaseFile = \fp f -> do
            withConnectionPool tr fp $ \pool ->
                withResource pool $ \(_, conn) -> do
                    f conn
        , getVersion = getVersionNew
        , setVersion = setVersionNew
        }

oldToNewSchemaVersion :: MigrateOld.SchemaVersion -> Version
oldToNewSchemaVersion (MigrateOld.SchemaVersion v) = v

newToOldSchemaVersion :: Version -> MigrateOld.SchemaVersion
newToOldSchemaVersion = MigrateOld.SchemaVersion

getVersionNew :: Sqlite.Connection -> IO Version
getVersionNew = fmap oldToNewSchemaVersion . getSchemaVersion

setVersionNew :: Sqlite.Connection -> Version -> IO ()
setVersionNew conn = putSchemaVersion conn . newToOldSchemaVersion
