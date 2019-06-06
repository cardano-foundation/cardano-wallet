{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Test.Integration.Scenario.CLI.Mnemonics
    ( spec
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Data.List
    ( length )
import Data.Version
    ( showVersion )
import Paths_cardano_wallet_http_bridge
    ( version )
import System.Command
    ( Exit (..), Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain )
import Test.Integration.Framework.DSL
    ( cardanoWalletCLI, cardanoWalletLauncherCLI, generateMnemonicsViaCLI )

import qualified Data.List as L

spec :: SpecWith ()
spec = do
    it "CLI_VERSION - cardano-wallet shows version" $  do
        (Exit c, Stdout out) <- cardanoWalletCLI ["--version"]
        let v = L.dropWhileEnd (== '\n') out
        v `shouldBe` (showVersion version)
        c `shouldBe` ExitSuccess

    it "CLI_VERSION - cardano-wallet-launcher shows version" $  do
        (Exit c, Stdout out) <- cardanoWalletLauncherCLI ["--version"]
        let v = L.dropWhileEnd (== '\n') out
        v `shouldBe` (showVersion version)
        c `shouldBe` ExitSuccess

    it "CLI_HELP - cardano-wallet-launcher shows help on bad argument" $  do
        (Exit c, Stdout out) <- cardanoWalletLauncherCLI ["--bad arg"]
        out `shouldContain` "cardano-wallet-launcher"
        c `shouldBe` ExitFailure 1

    describe "CLI_HELP - cardano-wallet-launcher shows help with" $  do
        let test option = it option $ do
                (Exit c, Stdout out) <- cardanoWalletLauncherCLI [option]
                out `shouldContain` "cardano-wallet-launcher"
                c `shouldBe` ExitSuccess
        forM_ ["-h", "--help"] test

    it "CLI_HELP - cardano-wallet shows help on bad argument" $  do
        (Exit c, Stdout out) <- cardanoWalletCLI ["--bad arg"]
        out `shouldContain` "Cardano Wallet CLI"
        c `shouldBe` ExitFailure 1

    describe "CLI_HELP - cardano-wallet shows help with" $  do
        let test option = it option $ do
                (Exit c, Stdout out) <- cardanoWalletCLI [option]
                out `shouldContain` "Cardano Wallet CLI"
                c `shouldBe` ExitSuccess
        forM_ ["-h", "--help"] test

    it "CLI_MNEMONICS_01 - Can generate mnemonics with default size" $  do
        (Exit c, Stdout out) <- generateMnemonicsViaCLI []
        length (words out) `shouldBe` 15
        c `shouldBe` ExitSuccess

    describe "CLI_MNEMONICS_01 - Can generate mnemonics with different sizes" $ do
        let test size = it ("--size=" <> show size) $ do
                (Exit c, Stdout out) <-
                    generateMnemonicsViaCLI ["--size", show size]
                length (words out) `shouldBe` size
                c `shouldBe` ExitSuccess
        forM_ [9, 12, 15, 18, 21, 24] test

    describe "CLI_MNEMONICS_02 - It can't generate mnemonics with an invalid size" $ do
        let sizes =
                ["15.5", "3", "6", "14", "abc", "👌", "0", "~!@#%" , "-1000", "1000"]
        forM_ sizes $ \(size) -> it ("--size=" <> size) $ do
            (Exit c, Stdout out, Stderr err) <-
                generateMnemonicsViaCLI ["--size", size]
            c `shouldBe` ExitFailure 1
            err `shouldBe`
                "Invalid mnemonic size. Expected one of: 9,12,15,18,21,24\n"
            out `shouldBe` mempty
