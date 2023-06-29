{-# LANGUAGE DataKinds #-}
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
    , shouldReturn
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
        it "handles backupDatabaseFile and withDatabaseFile" $ do
            withSystemTempDirectory "test" $ \dir -> do
                let interface = newMigrationInterface nullTracer
                let dbf = dir <> "/db"
                execute interface dbf createTable
                backupDatabaseFile interface dbf $ Version 1
                execute interface dbf populateTable
                backupDatabaseFile interface dbf $ Version 2
                sort <$> listDirectory dir
                    `shouldReturn` sort ["db", "db.v1.bak", "db.v2.bak"]

execute
    :: MonadUnliftIO m
    => MigrationInterface m DBHandle
    -> FilePath
    -> Text
    -> m ()
execute interface dbf t =
    withDatabaseFile interface dbf $ \handle ->
        Sqlite.runSqlConn
            (Sqlite.rawExecute t [])
            (dbBackend handle)

createTable :: Text
createTable =
    "CREATE TABLE IF NOT EXISTS test \
    \(id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL)"

populateTable :: Text
populateTable =
    "INSERT INTO test (name) VALUES ('hello')"
