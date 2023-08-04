{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.CLI.Miscellaneous
  ( spec
  )
where

import Cardano.Wallet.Version
  ( showVersionAsDate
  , version
  )
import Control.Monad
  ( forM_
  )
import Data.List qualified as L
import System.Command
  ( Exit (..)
  , Stderr (..)
  , Stdout (..)
  )
import System.Exit
  ( ExitCode (..)
  )
import Test.Hspec
  ( SpecWith
  , describe
  , it
  )
import Test.Hspec.Expectations.Lifted
  ( shouldBe
  , shouldContain
  )
import Test.Integration.Framework.DSL
  ( cardanoWalletCLI
  )
import Prelude

spec :: SpecWith ()
spec = describe "COMMON_CLI_MISC" $ do
  it "CLI_VERSION - cardano-wallet shows version" $ do
    (Exit c, Stdout out) <- cardanoWalletCLI @_ @IO ["version"]
    let
      v = L.dropWhileEnd (== '\n') out
    v `shouldContain` (showVersionAsDate version <> " (git revision: ")
    c `shouldBe` ExitSuccess

  describe "CLI_HELP - cardano-wallet shows help on bad arg or param" $ do
    let
      wid = replicate 40 '1'
    let
      badArgs =
        [ "--bad param"
        , "bad arg"
        , -- serve
          "serve --network testnet --random-por --quiet"
        , "serve --network testnet --random-port --bridge-port 666 --vebrose"
        , "serve --network"
        , "server --verbose"
        , "serve --database"
        , -- launch
          "launch --quit"
        , "launch --network mainnet --state-dir"
        , "launch --randomport 666 --state-dir tempdir"
        , "launcher --verbose"
        , -- mnemonic
          "mnemonic generate --size"
        , "mnemnic generate"
        , "mnemonic --size 15"
        , -- wallet list
          "wallet list --port"
        , -- wallet create
          "wallet create"
        , "wallet"
        , "create"
        , "wallet crate name"
        , "wallet create --port name"
        , "wallet create name --address-pool-gap"
        , -- wallet get
          "wallet get"
        , "get"
        , "get " ++ wid
        , "wallet get " ++ wid ++ " --port"
        , -- wallet update
          "wallet update"
        , "update"
        , "update " ++ wid
        , "wallet update " ++ wid ++ " --port"
        , -- wallet delete
          "wallet delete"
        , "delete"
        , "delete " ++ wid
        , "wallet delete " ++ wid ++ " --port"
        , -- wallet UTxO
          "wallet utxo"
        , "utxo"
        , "utxo " ++ wid
        , "wallet utxo " ++ wid ++ " --port"
        , -- transaction create
          "transaction create " ++ wid ++ " --payment"
        , "transaction create "
            ++ wid
            ++ " --payment 22@2cWKMJemoBaiPcjZZKQzTHzjRkaAee5dx246Ren8U5KcDGt9QX6FZQPskzykhYL1AW62U --payment"
        , "transaction create --port " ++ wid ++ " --payment 22@ --payment"
        , "transaction create"
        , -- address list
          "address"
        , "list"
        , "address list"
        , "address list " ++ wid ++ " --port"
        , -- network information
          "network"
        , "network info"
        , "networ information"
        , "network information --port"
        ]
    forM_ badArgs $ \args -> it args $ \_ -> do
      (Exit c, Stdout o, Stderr e) <- cardanoWalletCLI @_ @IO $ words args
      c `shouldBe` ExitFailure 1
      o `shouldBe` ""
      e `shouldContain` "Usage:"

  describe "CLI_HELP - cardano-wallet shows help with" $ do
    let
      test option = it option $ do
        (Exit c, Stdout o, Stderr e) <- cardanoWalletCLI @_ @IO [option]
        e `shouldBe` ""
        o `shouldContain` "Usage:"
        o `shouldContain` "Available options:"
        o `shouldContain` "Available commands:"
        c `shouldBe` ExitSuccess
    forM_ ["-h", "--help"] test
