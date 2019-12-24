{-# LANGUAGE TemplateHaskell #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- DBLayer tests for SQLite implementation.


module Cardano.Pool.DB.SqliteSpec
    ( spec
    ) where

import Prelude

import Cardano.BM.Configuration.Static
    ( defaultConfigTesting )
import Cardano.BM.Data.MonitoringEval
    ( MEvAction (..), MEvExpr (..), Operand (..), Operator (..) )
import Cardano.BM.Data.Observable
    ( ObservableInstance (..) )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.Pool.DB.Properties
    ( newMemoryDBLayer, properties, withDB )
import Cardano.Pool.DB.Sqlite
    ( withDBLayer )
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

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.BM.Data.Aggregated as CM
import qualified Cardano.BM.Data.AggregatedKind as CM
import qualified Cardano.BM.Data.Backend as CM
import qualified Cardano.BM.Data.SubTrace as CM
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

spec :: Spec
spec = do
    withDB (newMemoryDBLayer testingLogConfig) $ do
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
            cfg <- defaultConfigTesting
            (logs, _) <- captureLogging $ \tr -> do
                withDBLayer cfg tr (Just path) $ \_ -> pure ()
                withDBLayer cfg tr (Just path) $ \_ -> pure ()

            let databaseConnMsg  = filter
                    (T.isInfixOf "Using connection string")
                    logs

            let databaseResetMsg = filter
                    (T.isInfixOf "Non backward compatible database found")
                    logs

            let migrationErrMsg  = filter
                    (T.isInfixOf "PersistError")
                    logs

            length databaseConnMsg  `shouldBe` 3
            length databaseResetMsg `shouldBe` 1
            length migrationErrMsg  `shouldBe` 1

testingLogConfig :: IO CM.Configuration
testingLogConfig = do
    logConfig <- defaultConfigTesting
    CM.setMinSeverity logConfig Debug
    CM.setSetupBackends logConfig [CM.KatipBK, CM.AggregationBK, CM.MonitoringBK]

    CM.setSubTrace logConfig "query"
        (Just $ CM.ObservableTraceSelf [MonotonicClock])

    CM.setBackends logConfig
        "query"
        (Just [CM.AggregationBK])
    CM.setAggregatedKind logConfig
        "query"
        (Just CM.StatsAK) -- statistics AgreggatedKind
    CM.setBackends logConfig
        "#aggregation.query"
        (Just [CM.KatipBK])

    -- This monitor should always trigger.
    CM.setMonitors logConfig $ HM.singleton
        "query.diff"
        ( Nothing
        , Compare "query.diff.timestamp" (GE, (OpMeasurable (CM.Seconds 0)))
        , [CreateMessage Info "runQuery monitor works"]
        )

    CM.setBackends logConfig
        "query"
        (Just [CM.AggregationBK, CM.KatipBK, CM.MonitoringBK])

    pure logConfig
