module Test.Integration.Scenario.CLI.Server
    ( spec
    ) where

import Prelude

import System.Directory
    ( listDirectory, removeDirectory )
import System.Exit
    ( ExitCode (..) )
import System.IO.Temp
    ( withSystemTempDirectory )
import System.Process
    ( CreateProcess (..)
    , StdStream (..)
    , createProcess
    , proc
    , terminateProcess
    , waitForProcess
    , withCreateProcess
    )
import Test.Hspec
    ( Spec, describe, it, shouldContain, shouldReturn )

import qualified Data.Text.IO as TIO

spec :: Spec
spec = do
    describe "Launcher should start the server with a database" $ do
        it "should create the database file" $ withTempDir $ \d -> do
            launcher d
            ls <- listDirectory d
            ls `shouldContain` ["wallet.db"]

        it "should work with empty state directory" $ withTempDir $ \d -> do
            removeDirectory d
            launcher d
            ls <- listDirectory d
            ls `shouldContain` ["wallet.db"]

    describe "DaedalusIPC" $ do
        it "should reply with the port when asked" $ do
            (_, _, _, ph) <-
                createProcess (proc "test/integration/js/mock-daedalus.js" [])
            waitForProcess ph `shouldReturn` ExitSuccess

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "integration-state"

launcher :: FilePath -> IO ()
launcher stateDir = withCreateProcess cmd $ \_ _ (Just stderr) ph -> do
    TIO.hGetContents stderr >>= TIO.putStrLn
    terminateProcess ph
  where
    cmd = proc' "cardano-wallet" ["launch", "--state-dir", stateDir]

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
proc' cmd args = (proc "stack" (["exec", "--", cmd] ++ args))
    { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
