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
    ( Network, Port, decode, encode, getArg )
import Cardano.NetworkLayer
    ( listen )
import Cardano.Wallet.Primitive
    ( Block )
import Control.Monad
    ( when )
import Fmt
    ( build, fmt )
import Say
    ( say )
import System.Console.Docopt
    ( Docopt, docopt, exitWithUsage, isPresent, longOption, parseArgsOrExit )
import System.Environment
    ( getArgs )

import qualified Cardano.NetworkLayer.HttpBridge as HttpBridge
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
  --network <NETWORK>          mainnet or testnet [default: mainnet]
  --wallet-server-port <PORT>  port used for serving the wallet API [default: 8090]
  --http-bridge-port <PORT>    port used for communicating with the http-bridge [default: 8080]
|]

main :: IO ()
main = do
    args <- parseArgsOrExit cli =<< getArgs
    when (args `isPresent` (longOption "help")) $ exitWithUsage cli

    networkName <- getArg args cli (longOption "network") (decode @Network)
    bridgePort <- getArg args cli (longOption "http-bridge-port") decode
    _ <- getArg args cli (longOption "wallet-server-port") (decode @(Port "wallet"))

    network <- HttpBridge.newNetworkLayer (T.pack . encode $ networkName) bridgePort
    listen network logBlock
  where
    logBlock :: Block -> IO ()
    logBlock = say . fmt . build
