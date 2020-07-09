{-# LANGUAGE TemplateHaskell #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- DBLayer tests for SQLite implementation.


module Cardano.Pool.DB.SqliteSpec
    ( spec
    ) where

import Prelude

import Cardano.DB.Sqlite
    ( DBLog (..) )
import Cardano.Pool.DB.Properties
    ( newMemoryDBLayer, properties, withDB )
import Cardano.Pool.DB.Sqlite
    ( withDBLayer )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyTimeInterpreter )
import System.Directory
    ( copyFile )
import System.FilePath
    ( (</>) )
import System.IO.Temp
    ( withSystemTempDirectory )
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.Utils.Paths
    ( getTestData )
import Test.Utils.Trace
    ( captureLogging )

spec :: Spec
spec = do
    withDB newMemoryDBLayer $ do
        describe "Sqlite" properties

    describe "Migration Regressions" $ do
        test_migrationFromv20191216

test_migrationFromv20191216 :: Spec
test_migrationFromv20191216 =
    it "'migrate' an existing database from v2019-12-16 by\
       \ creating it from scratch again. But only once." $ do
        let orig = $(getTestData) </> "stake-pools-db" </> "v2019-12-16.sqlite"
        withSystemTempDirectory "stake-pools-db" $ \dir -> do
            let path = dir </> "stake-pools.sqlite"
            copyFile orig path
            let ti = dummyTimeInterpreter
            (logs, _) <- captureLogging $ \tr -> do
                withDBLayer tr (Just path) ti $ \_ -> pure ()
                withDBLayer tr (Just path) ti $ \_ -> pure ()

            let databaseConnMsg  = filter isMsgConnStr logs
            let databaseResetMsg = filter (== MsgDatabaseReset) logs
            let migrationErrMsg  = filter isMsgMigrationError logs

            length databaseConnMsg  `shouldBe` 3
            length databaseResetMsg `shouldBe` 1
            length migrationErrMsg  `shouldBe` 1


isMsgConnStr :: DBLog -> Bool
isMsgConnStr (MsgConnStr _) = True
isMsgConnStr _ = False

isMsgMigrationError :: DBLog -> Bool
isMsgMigrationError (MsgMigrations (Left _)) = True
isMsgMigrationError _ = False
