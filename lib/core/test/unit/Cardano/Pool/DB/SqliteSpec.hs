{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- DBLayer tests for SQLite implementation.

module Cardano.Pool.DB.SqliteSpec
    ( spec
    ) where

import Prelude

import Cardano.BM.Trace
    ( traceInTVarIO )
import Cardano.DB.Sqlite
    ( DBLog (..), SqliteContext )
import Cardano.Pool.DB
    ( DBLayer (..) )
import Cardano.Pool.DB.Log
    ( PoolDbLog (..) )
import Cardano.Pool.DB.Properties
    ( properties )
import Cardano.Pool.DB.Sqlite
    ( newDBLayer, withDBLayer )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyTimeInterpreter )
import System.Directory
    ( copyFile )
import System.FilePath
    ( (</>) )
import Test.Hspec
    ( Spec, before, describe, it, parallel, shouldBe )
import Test.Utils.Paths
    ( getTestData )
import Test.Utils.Trace
    ( captureLogging )
import UnliftIO.STM
    ( TVar, newTVarIO )
import UnliftIO.Temporary
    ( withSystemTempDirectory )

-- | Set up a DBLayer for testing, with the command context, and the logging
-- variable.
newMemoryDBLayer :: IO (DBLayer IO)
newMemoryDBLayer = snd . snd <$> newMemoryDBLayer'

newMemoryDBLayer' :: IO (TVar [PoolDbLog], (SqliteContext, DBLayer IO))
newMemoryDBLayer' = do
    logVar <- newTVarIO []
    (logVar, ) <$> newDBLayer (traceInTVarIO logVar) Nothing ti
  where
    ti = dummyTimeInterpreter

spec :: Spec
spec = parallel $ do
    before newMemoryDBLayer $ do
        parallel $ describe "Sqlite" properties

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
            let databaseResetMsg = filter (== MsgGeneric MsgDatabaseReset) logs
            let migrationErrMsg  = filter isMsgMigrationError logs

            length databaseConnMsg  `shouldBe` 3
            length databaseResetMsg `shouldBe` 1
            length migrationErrMsg  `shouldBe` 1

isMsgConnStr :: PoolDbLog -> Bool
isMsgConnStr (MsgGeneric (MsgConnStr _)) = True
isMsgConnStr _ = False

isMsgMigrationError :: PoolDbLog -> Bool
isMsgMigrationError (MsgGeneric (MsgMigrations (Left _))) = True
isMsgMigrationError _ = False
