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
import Control.Concurrent
    ( threadDelay )
import Control.Monad
    ( forever, when )
import Say
    ( sayErr )
import System.Console.Docopt
    ( Docopt, docopt, exitWithUsage, isPresent, longOption, parseArgsOrExit )
import System.Environment
    ( getArgs )

import qualified Data.Text as T

-- | Command-Line Interface specification. See http://docopt.org/
cli :: Docopt
cli = [docopt|
cardano-wallet-server

Start the cardano wallet server.

Usage:
  cardano-wallet-server [options]
  cardano-wallet-server --help

Options:
  --network <NETWORK>          mainnet, testnet or staging [default: mainnet]
  --wallet-server-port <PORT>  port used for serving the wallet API [default: 8090]
  --http-bridge-port <PORT>    port used for communicating with the http-bridge [default: 8080]
|]

main :: IO ()
main = do
    args <- parseArgsOrExit cli =<< getArgs
    when (args `isPresent` (longOption "help")) $ exitWithUsage cli
    networkName <-
        getArg @String args cli (longOption "network") (decode @_ @Network)
    walletPort <-
        getArg @String args cli (longOption "wallet-server-port") (decode @_ @Int)
    bridgePort <-
        getArg @String args cli (longOption "http-bridge-port") (decode @_ @Int)
    forever $ do
        sayErr
            $ "Wallet Backend NOT listening on " <> T.pack (show walletPort)
            <> ", connected to cardano-http-bridge on " <> T.pack (show bridgePort)
            <> "@" <> T.pack (encode networkName)
        threadDelay 1000000
