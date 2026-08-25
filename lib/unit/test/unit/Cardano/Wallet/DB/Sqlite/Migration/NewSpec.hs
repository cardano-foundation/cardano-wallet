{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Copyright: © 2023 IOHK License: Apache-2.0
--
-- Tests for new database migration sqlite instance.  A module that tests a new
-- database migration sqlite instance.
module Cardano.Wallet.DB.Sqlite.Migration.NewSpec
    ( spec
    ) where

import Cardano.DB.Sqlite
    ( DBHandle (dbBackend)
    )
import Cardano.Wallet.DB.Migration
    ( MigrationInterface (..)
    , Version (..)
    )
import Cardano.Wallet.DB.Sqlite.Migration.New
    ( latestVersion
    , newMigrationInterface
    )
import Control.Tracer
    ( nullTracer
    )
import Data.List
    ( sort
    )
import Data.Text
    ( Text
    )
import System.Directory
    ( listDirectory
    )
import System.IO.Temp
    ( withSystemTempDirectory
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldReturn
    )
import Test.Hspec.Extra
    ( itWithDiagnosticTimeout
    )
import UnliftIO
    ( MonadUnliftIO
    )
import Prelude hiding
    ( (.)
    )

import qualified Database.Persist.Sqlite as Sqlite

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
spec :: Spec
spec = do
    describe "new migrations" $ do
        it "targets durable-submission schema version six"
            $ latestVersion `shouldBe` Version 6
        itWithDiagnosticTimeout
            60
            "handles backupDatabaseFile and withDatabaseFile"
            $ \publish _ ->
                withSystemTempDirectory "test" $ \dir -> do
                    let interface = newMigrationInterface nullTracer
                    let dbf = dir <> "/db"
                    execute publish interface dbf CreateTable createTable
                    publish $ diagnostic dbf (BackupTo $ Version 1) LockExpected
                    backupDatabaseFile interface dbf $ Version 1
                    publish
                        $ diagnostic dbf (BackupTo $ Version 1) NoConnectionExpected
                    execute publish interface dbf PopulateTable populateTable
                    publish $ diagnostic dbf (BackupTo $ Version 2) LockExpected
                    backupDatabaseFile interface dbf $ Version 2
                    publish
                        $ diagnostic dbf (BackupTo $ Version 2) NoConnectionExpected
                    publish $ diagnostic dbf InspectFiles NoConnectionExpected
                    sort <$> listDirectory dir
                        `shouldReturn` sort ["db", "db.v1.bak", "db.v2.bak"]

execute
    :: MonadUnliftIO m
    => (MigrationDiagnostic -> m ())
    -> MigrationInterface m DBHandle
    -> FilePath
    -> MigrationOperation
    -> Text
    -> m ()
execute publish interface dbf operation t = do
    publish $ diagnostic dbf operation ConnectionOpening
    withDatabaseFile interface dbf $ \handle ->
        do
            publish $ diagnostic dbf operation ConnectionOpen
            Sqlite.runSqlConn
                (Sqlite.rawExecute t [])
                (dbBackend handle)
    publish $ diagnostic dbf operation NoConnectionExpected

data MigrationDiagnostic = MigrationDiagnostic
    { databaseFile :: FilePath
    , currentOperation :: MigrationOperation
    , connectionOrLockExpectation :: ConnectionOrLockExpectation
    }
    deriving (Show)

data MigrationOperation
    = CreateTable
    | BackupTo Version
    | PopulateTable
    | InspectFiles
    deriving (Show)

data ConnectionOrLockExpectation
    = NoConnectionExpected
    | ConnectionOpening
    | ConnectionOpen
    | LockExpected
    deriving (Show)

diagnostic
    :: FilePath
    -> MigrationOperation
    -> ConnectionOrLockExpectation
    -> MigrationDiagnostic
diagnostic databaseFile currentOperation connectionOrLockExpectation =
    MigrationDiagnostic
        { databaseFile
        , currentOperation
        , connectionOrLockExpectation
        }

createTable :: Text
createTable =
    "CREATE TABLE IF NOT EXISTS test \
    \(id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL)"

populateTable :: Text
populateTable =
    "INSERT INTO test (name) VALUES ('hello')"
