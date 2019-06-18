module Cardano.LauncherSpec
    ( spec
    ) where

import Prelude

import Cardano.Launcher
    ( Command (..), StdStream (..) )
import System.Directory
    ( doesDirectoryExist, doesFileExist, removeDirectory )
import System.Exit
    ( ExitCode (..) )
import System.IO.Temp
    ( withSystemTempDirectory )
import System.Process
    ( createProcess, proc, waitForProcess )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Expectations.Lifted
    ( shouldReturn )
import Test.Integration.Framework.DSL
    ( expectCmdStarts )

spec :: Spec
spec = do
    describe "LAUNCH - cardano-wallet launch" $ do
        it "LAUNCH - Can start launcher against testnet" $ withTempDir $ \d -> do
            removeDirectory d
            let cardanoWalletLauncher = Command "stack"
                    [ "exec", "--", "cardano-wallet", "launch"
                    , "--network", "testnet"
                    ] (return ())
                    Inherit
            expectCmdStarts cardanoWalletLauncher
            doesDirectoryExist d `shouldReturn` False

        it "LAUNCH - Can start launcher with --state-dir" $ withTempDir $ \d -> do
            let cardanoWalletLauncher = Command "stack"
                    [ "exec", "--", "cardano-wallet", "launch"
                    , "--state-dir", d
                    ] (return ())
                    Inherit
            expectCmdStarts cardanoWalletLauncher
            expectStateDirExists d

        it "LAUNCH - Can start launcher with --state-dir <emptydir>"
            $ withTempDir $ \d -> do
            removeDirectory d
            let cardanoWalletLauncher = Command "stack"
                    [ "exec", "--", "cardano-wallet", "launch"
                    , "--state-dir", d
                    ] (return ())
                    Inherit
            expectCmdStarts cardanoWalletLauncher
            expectStateDirExists d

    describe "DaedalusIPC" $ do
        it "should reply with the port when asked" $ do
            (_, _, _, ph) <-
             createProcess (proc "test/integration/js/mock-daedalus.js" [])
            waitForProcess ph `shouldReturn` ExitSuccess

expectStateDirExists :: FilePath -> IO ()
expectStateDirExists dir = do
    doesDirectoryExist dir `shouldReturn` True
    doesDirectoryExist (dir ++ "/testnet") `shouldReturn` True
    doesFileExist (dir ++ "/wallet.db") `shouldReturn` True
    doesFileExist (dir ++ "/wallet.db-shm") `shouldReturn` True
    doesFileExist (dir ++ "/wallet.db-wal") `shouldReturn` True

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "integration-state"
