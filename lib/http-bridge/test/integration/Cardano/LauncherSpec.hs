module Cardano.LauncherSpec
    ( spec
    ) where

import Prelude

import Cardano.Launcher
    ( Command (..), StdStream (..) )
import System.Directory
    ( createDirectory, doesDirectoryExist, doesFileExist, removePathForcibly )
import System.Exit
    ( ExitCode (..) )
import System.Process
    ( createProcess, proc, waitForProcess )
import Test.Hspec
    ( Spec, after_, describe, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldReturn )
import Test.Integration.Framework.DSL
    ( expectCmdStarts )

spec :: Spec
spec = after_ tearDown $ do
    describe "LAUNCH - cardano-wallet launch" $ do
        it "LAUNCH - Can start launcher against testnet" $ do
            let cardanoWalletLauncher = Command "stack"
                    [ "exec", "--", "cardano-wallet", "launch"
                    , "--network", "testnet"
                    ] (return ())
                    Inherit
            expectCmdStarts cardanoWalletLauncher
            d1 <- doesDirectoryExist stateDir
            d1 `shouldBe` False

        it "LAUNCH - Can start launcher with --state-dir" $ do
            let cardanoWalletLauncher = Command "stack"
                    [ "exec", "--", "cardano-wallet", "launch"
                    , "--state-dir", stateDir
                    ] (return ())
                    Inherit
            expectCmdStarts cardanoWalletLauncher
            expectStateDirExists stateDir

        it "LAUNCH - Can start launcher with --state-dir <emptydir>" $ do
            createDirectory stateDir
            let cardanoWalletLauncher = Command "stack"
                    [ "exec", "--", "cardano-wallet", "launch"
                    , "--state-dir", stateDir
                    ] (return ())
                    Inherit
            expectCmdStarts cardanoWalletLauncher
            expectStateDirExists stateDir

        describe "DaedalusIPC" $ do
            it "should reply with the port when asked" $ do
                (_, _, _, ph) <-
                 createProcess (proc "test/integration/js/mock-daedalus.js" [])
                waitForProcess ph `shouldReturn` ExitSuccess

 where
     stateDir = "./test/data/tmpStateDir"
     tearDown = do
         removePathForcibly stateDir

     expectStateDirExists dir = do
         doesDirectoryExist dir `shouldReturn` True
         doesDirectoryExist (dir ++ "/testnet") `shouldReturn` True
         doesFileExist (dir ++ "/wallet.db") `shouldReturn` True
         doesFileExist (dir ++ "/wallet.db-shm") `shouldReturn` True
         doesFileExist (dir ++ "/wallet.db-wal") `shouldReturn` True
