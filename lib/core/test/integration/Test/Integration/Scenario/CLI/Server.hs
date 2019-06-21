module Test.Integration.Scenario.CLI.Server
    ( spec
    ) where

import Prelude

import Control.Concurrent
    ( threadDelay )
import System.IO.Temp
    ( withSystemTempDirectory )
import System.Process
    ( terminateProcess, withCreateProcess )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Integration.Framework.DSL
    ( Context (..), expectPathEventuallyExist, proc' )

spec :: SpecWith (Context t)
spec = do
    describe "SERVER - cardano-wallet serve" $ do
        it "SERVER - Can start cardano-wallet serve --database" $ \_ -> do
            withTempDir $ \d -> do
                let db = d ++ "/db-file"
                let args = ["serve", "--database", db]
                let process = proc' "cardano-wallet" args
                withCreateProcess process $ \_ _ _ ph -> do
                    expectPathEventuallyExist db
                    expectPathEventuallyExist (db <> "-shm")
                    expectPathEventuallyExist (db <> "-wal")
                    terminateProcess ph
            threadDelay oneSecond

oneSecond :: Int
oneSecond = 1000000

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "integration-state"
