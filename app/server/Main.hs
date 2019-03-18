{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- This module parses command line arguments and starts the wallet.
--
-- This should be the only module that has a notion of command line arguments.
-- Although, for development purposes, we may extend the docopt specification
-- here and call `getArgs` where it is needed.
module Main where

import Prelude

import Cardano.CLI
    ( Network, decode, encode, getArg )
import Cardano.Wallet
    ( WalletName (..) )
import Cardano.Wallet.Mnemonic
    ( Mnemonic )
import Cardano.WalletLayer
    ( NewWallet (..), WalletLayer (..), mkWalletLayer )
import Control.Monad
    ( when )
import System.Console.Docopt
    ( Docopt
    , argument
    , docopt
    , exitWithUsage
    , isPresent
    , longOption
    , parseArgsOrExit
    )
import System.Environment
    ( getArgs )

import qualified Cardano.DBLayer.MVar as MVar
import qualified Cardano.NetworkLayer.HttpBridge as HttpBridge
import qualified Data.Text as T

-- | Command-Line Interface specification. See http://docopt.org/
cli :: Docopt
cli = [docopt|
cardano-wallet-server

Start the cardano wallet server.

Usage:
  cardano-wallet-server [options] <mnemonic>...
  cardano-wallet-server --help

Options:
  --network <NETWORK>          mainnet or testnet [default: mainnet]
  --wallet-server-port <PORT>  port used for serving the wallet API [default: 8090]
  --http-bridge-port <PORT>    port used for communicating with the http-bridge [default: 8080]
|]

main :: IO ()
main = do
    args <- parseArgsOrExit cli =<< getArgs
    when (args `isPresent` (longOption "help")) $ exitWithUsage cli
    networkName <-
        getArg @String args cli (longOption "network") (decode @_ @Network)
    bridgePort <-
        getArg @String args cli (longOption "http-bridge-port") (decode @_ @Int)
    mnemonicSentence <-
        getArg @[String] args cli (argument "mnemonic") (decode @_ @(Mnemonic 15))

    network <- HttpBridge.newNetworkLayer (T.pack . encode $ networkName) bridgePort
    db <- MVar.newDBLayer
    let wallet = mkWalletLayer db network
    wid <- createWallet wallet NewWallet
        { mnemonic = mnemonicSentence
        , mnemonic2ndFactor = mempty
        , name = WalletName "My Wallet"
        , passphrase = mempty
        , gap = minBound
        }
    watchWallet wallet wid
