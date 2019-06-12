module Test.Integration.Scenario.CLI.Server
    ( spec
    ) where

import Prelude

import Control.Concurrent
    ( threadDelay )
import System.Directory
    ( listDirectory, removeDirectory )
import System.Exit
    ( ExitCode (..) )
import System.IO.Temp
    ( withSystemTempDirectory )
import System.Process
    ( CreateProcess, proc, withCreateProcess )
import Test.Hspec
    ( Spec, describe, it, shouldContain )

spec :: Spec
spec = do
    describe "Launcher should start the server with a database" $ do
        it "should create the database file" $ withTempDir $ \d -> do
            removeDirectory d
            launcher d
            ls <- listDirectory d
            ls `shouldContain` ["wallet.db"]

        it "should work with empty state directory" $ withTempDir $ \d -> do
            launcher d
            ls <- listDirectory d
            ls `shouldContain` ["wallet.db"]

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "integration-state"

waitForStartup :: IO ()
waitForStartup = threadDelay (2 * 1000 * 1000)

launcher :: FilePath -> IO ()
launcher stateDir = withCreateProcess cmd $ \_ _ _ ph -> do
    waitForStartup
    terminateProcess ph
  where
    cmd = proc' "cardano-wallet-launcher" ["--state-dir", stateDir]

-- There is a dependency cycle in the packages.
--
-- cardano-wallet-launcher depends on cardano-wallet-http-bridge so that it can
-- import the HttpBridge module.
--
-- This package (cardano-wallet-http-bridge) should have
-- build-tool-depends: cardano-wallet:cardano-wallet-launcher so that it can
-- run launcher in the tests. But that dependency can't be expressed in the
-- cabal file, because otherwise there would be a cycle.
--
-- So one hacky way to work around it is by running programs under "stack exec".
proc' :: FilePath -> [String] -> CreateProcess
proc' cmd args = proc "stack" (["exec", cmd, "--"] ++ args)
