module Cardano.LauncherSpec
    ( spec
    ) where

import Prelude

import Cardano.Launcher
    ( Command (..), StdStream (..) )
import System.Directory
    ( doesDirectoryExist, doesFileExist, removePathForcibly )
import Test.Hspec
    ( Spec, after_, describe, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe )
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

        it "LAUNCH - Can start launcher against testnet with --state-dir" $ do
            let cardanoWalletLauncher = Command "stack"
                    [ "exec", "--", "cardano-wallet", "launch"
                    , "--state-dir", stateDir
                    ] (return ())
                    Inherit
            expectCmdStarts cardanoWalletLauncher

            d1 <- doesDirectoryExist stateDir
            d2 <- doesDirectoryExist (stateDir ++ "/testnet")
            w1 <- doesFileExist (stateDir ++ "/wallet.db")
            w2 <- doesFileExist (stateDir ++ "/wallet.db-shm")
            w3 <- doesFileExist (stateDir ++ "/wallet.db-wal")
            d1 `shouldBe` True
            d2 `shouldBe` True
            w1 `shouldBe` True
            w2 `shouldBe` True
            w3 `shouldBe` True
 where
     stateDir = "./test/data/tmpStateDir"
     tearDown = do
         removePathForcibly stateDir
