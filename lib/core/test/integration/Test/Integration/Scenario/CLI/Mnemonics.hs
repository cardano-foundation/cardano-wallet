{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Test.Integration.Scenario.CLI.Mnemonics
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Version
    ( showVersion, version )
import Control.Monad
    ( forM_ )
import Data.List
    ( length )
import System.Command
    ( Exit (..), Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain )
import Test.Integration.Framework.DSL
    ( cardanoWalletCLI, generateMnemonicsViaCLI )

import qualified Data.List as L

spec :: SpecWith ()
spec = do
    it "CLI_VERSION - cardano-wallet shows version" $  do
        (Exit c, Stdout out) <- cardanoWalletCLI ["version"]
        let v = L.dropWhileEnd (== '\n') out
        v `shouldBe` (showVersion version)
        c `shouldBe` ExitSuccess

    describe "CLI_HELP - cardano-wallet shows help on bad arg or param" $ do
        let wid = replicate 40 '1'
        let badArgs =
                [ "--bad param"
                , "bad arg"
                -- serve
                , "serve --network testnet --random-por --quiet"
                , "serve --network testnet --random-port --bridge-port 666 --vebrose"
                , "serve --network"
                , "server --verbose"
                , "serve --database"
                --launch
                , "launch --quit"
                , "launch --network mainnet --state-dir"
                , "launch --randomport 666 --state-dir tempdir"
                , "launcher --verbose"
                --mnemonic
                , "mnemonic generate --size"
                , "mnemnic generate"
                , "mnemonic --size 15"
                -- wallet list
                , "wallet list --port"
                -- wallet create
                , "wallet create"
                , "wallet"
                , "create"
                , "wallet crate name"
                , "wallet create --port name"
                , "wallet create name --address-pool-gap"
                -- wallet get
                , "wallet get"
                , "get"
                , "get " ++ wid
                , "wallet get " ++ wid ++ " --port"
                -- wallet update
                , "wallet update"
                , "update"
                , "update " ++ wid
                , "wallet update " ++ wid ++ " --port"
                -- wallet delete
                , "wallet delete"
                , "delete"
                , "delete " ++ wid
                , "wallet delete " ++ wid ++ " --port"
                -- transaction create
                , "transaction create " ++ wid ++ " --payment"
                , "transaction create " ++ wid ++ " --payment 22@2cWKMJemoBaiPcjZZKQzTHzjRkaAee5dx246Ren8U5KcDGt9QX6FZQPskzykhYL1AW62U --payment"
                , "transaction create --port " ++ wid ++ " --payment 22@ --payment"
                , "transaction create"
                -- address list
                , "address"
                , "list"
                , "address list"
                , "address list " ++ wid ++ " --port"
                ]
        forM_ badArgs $ \args -> it args $ \_ -> do
            (Exit c, Stdout o, Stderr e) <- cardanoWalletCLI $ words args
            c `shouldBe` ExitFailure 1
            o `shouldBe` ""
            e `shouldContain` "Usage:"

    describe "CLI_HELP - cardano-wallet shows help with" $  do
        let test option = it option $ do
                (Exit c, Stdout o, Stderr e) <- cardanoWalletCLI [option]
                e `shouldBe` ""
                o `shouldContain` "Usage:"
                o `shouldContain` "Available options:"
                o `shouldContain` "Available commands:"
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
