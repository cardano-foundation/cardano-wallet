{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- DBLayer tests for SQLite implementation.

module Cardano.Pool.DB.LayerSpec
    ( spec
    ) where

import Prelude

import Cardano.BM.Trace
    ( nullTracer
    )
import Cardano.Pool.DB
    ( DBLayer (..)
    )
import Cardano.Pool.DB.Layer
    ( withDBLayer
    )
import Cardano.Pool.DB.Log
    ( PoolDbLog (..)
    )
import Cardano.Pool.DB.Properties
    ( properties
    )
import Cardano.Pool.DB.Sqlite
    ( DBLog (..)
    )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyTimeInterpreter
    )
import System.Directory
    ( copyFile
    )
import System.FilePath
    ( (</>)
    )
import Test.Hspec
    ( Spec
    , around
    , describe
    , it
    , shouldBe
    )
import Test.Utils.Paths
    ( getTestData
    )
import Test.Utils.Trace
    ( captureLogging
    )
import UnliftIO.Temporary
    ( withSystemTempDirectory
    )

withMemoryDBLayer
    :: (DBLayer IO -> IO a)
    -> IO a
withMemoryDBLayer = withDBLayer nullTracer Nothing dummyTimeInterpreter

spec :: Spec
spec = do
    around withMemoryDBLayer $ do
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

            let databaseConnMsg  = filter isMsgOpenDB logs
            let databaseResetMsg = filter (== MsgGeneric MsgDatabaseReset) logs
            let migrationErrMsg  = filter isMsgMigrationError logs

            length databaseConnMsg  `shouldBe` 8
            length databaseResetMsg `shouldBe` 1
            length migrationErrMsg  `shouldBe` 1

isMsgOpenDB :: PoolDbLog -> Bool
isMsgOpenDB (MsgGeneric (MsgOpenSingleConnection _)) = True
isMsgOpenDB _ = False

isMsgMigrationError :: PoolDbLog -> Bool
isMsgMigrationError (MsgGeneric (MsgMigrations (Left _))) = True
isMsgMigrationError _ = False
