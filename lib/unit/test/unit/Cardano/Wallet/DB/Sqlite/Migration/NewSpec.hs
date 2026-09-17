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
    ( newMigrationInterface
    , runNewStyleMigrations
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
    ( copyFile
    , listDirectory
    )
import System.IO.Temp
    ( withSystemTempDirectory
    )
import Test.Hspec
    ( Spec
    , anyException
    , describe
    , it
    , shouldBe
    , shouldReturn
    , shouldThrow
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

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Database.Persist.Sqlite as Sqlite

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
spec :: Spec
spec = do
    describe "new migrations" $ do
        it "backs up V5 and migrates shared spending/collateral claims"
            $ withSystemTempDirectory "test"
            $ \dir -> do
                let dbf = dir <> "/wallet.sqlite"
                createV5Database dbf False
                v5 <- BS.readFile dbf
                runNewStyleMigrations nullTracer dbf
                schemaVersion dbf `shouldReturn` 6
                claims <-
                    Sqlite.runSqlite (T.pack dbf)
                        $ Sqlite.rawSql
                            "SELECT source_tx_id, source_index FROM dapp_submission_input WHERE active = 1"
                            []
                claims
                    `shouldBe` [(Sqlite.Single $ T.replicate 32 "11", Sqlite.Single (0 :: Int))]
                BS.readFile (dbf <> ".v5.bak") `shouldReturn` v5
        it
            "rolls back malformed V5 submissions and leaves a restorable backup"
            $ withSystemTempDirectory "test"
            $ \dir -> do
                let dbf = dir <> "/wallet.sqlite"
                createV5Database dbf True
                v5 <- BS.readFile dbf
                runNewStyleMigrations nullTracer dbf `shouldThrow` anyException
                schemaVersion dbf `shouldReturn` 5
                durableSubmissionTableCount dbf `shouldReturn` 0
                BS.readFile (dbf <> ".v5.bak") `shouldReturn` v5
                copyFile (dbf <> ".v5.bak") dbf
                schemaVersion dbf `shouldReturn` 5
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

createV5Database :: FilePath -> Bool -> IO ()
createV5Database dbf malformedLiveSubmission =
    Sqlite.runSqlite (T.pack dbf) $ do
        Sqlite.rawExecute
            "CREATE TABLE database_schema_version (name TEXT PRIMARY KEY, version INTEGER NOT NULL)"
            []
        Sqlite.rawExecute
            "INSERT INTO database_schema_version (name, version) VALUES ('schema', 5)"
            []
        Sqlite.rawExecute
            "CREATE TABLE wallet (wallet_id TEXT PRIMARY KEY)"
            []
        Sqlite.rawExecute
            "CREATE TABLE submissions (wallet_id TEXT NOT NULL, tx_id TEXT NOT NULL, tx BLOB NOT NULL, expiration INTEGER NULL, status INTEGER NOT NULL, acceptance INTEGER NULL)"
            []
        Sqlite.rawExecute "INSERT INTO wallet (wallet_id) VALUES ('00')" []
        if malformedLiveSubmission
            then
                Sqlite.rawExecute
                    "INSERT INTO submissions (wallet_id, tx_id, tx, expiration, status, acceptance) VALUES ('00', '00', X'00', NULL, 0, NULL)"
                    []
            else
                Sqlite.rawExecute
                    "INSERT INTO submissions (wallet_id, tx_id, tx, expiration, status, acceptance) VALUES ('00', '309bcb04da03b16436e65709c2e5ad2d75a1ebfec3c0b328e0a7a7e10b398aae', X'84a40081825820111111111111111111111111111111111111111111111111111111111111111100018182581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1a000f424002000d81825820111111111111111111111111111111111111111111111111111111111111111100a0f5f6', NULL, 0, NULL)"
                    []

schemaVersion :: FilePath -> IO Int
schemaVersion dbf = do
    [Sqlite.Single version] <-
        Sqlite.runSqlite (T.pack dbf)
            $ Sqlite.rawSql
                "SELECT version FROM database_schema_version WHERE name = 'schema'"
                []
    pure version

durableSubmissionTableCount :: FilePath -> IO Int
durableSubmissionTableCount dbf = do
    [Sqlite.Single count] <-
        Sqlite.runSqlite (T.pack dbf)
            $ Sqlite.rawSql
                "SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = 'dapp_submission'"
                []
    pure count
