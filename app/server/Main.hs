{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-
   This module parses command line arguments and starts the wallet.

   This should be the only module that has a notion of command line arguments.
   Although, for development purposes, we may extend the docopt specification
   here and call `getArgs` where it is needed.
-}
module Main where

import Prelude

import Cardano.NetworkLayer
    ( listen )
import Cardano.Wallet.Primitive
    ( Block )
import CLI
import Control.Monad
    ( when )
import Fmt
    ( build, fmt )
import System.Console.Docopt
    ( Docopt, docopt, exitWithUsage, isPresent, longOption, parseArgsOrExit )
import System.Environment
    ( getArgs )

import qualified Cardano.NetworkLayer.HttpBridge as HttpBridge
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | Command-Line Interface specification. See http://docopt.org/
cli :: Docopt
cli = [docopt|
cardano-wallet-server

Start the cardano wallet server.

Usage:
  cardano-wallet-server [options]
  cardano-wallet-server --help

Options:
  --wallet-server-port <PORT>  port used for serving the wallet API [default: 8090]
  --network <NETWORK>          mainnet or testnet [default: mainnet]
  --node-port <PORT>           port used for node-wallet communication [default: 8080]
|]



main :: IO ()
main = do
    args <- parseArgsOrExit cli =<< getArgs
    when (args `isPresent` (longOption "help")) $ exitWithUsage cli
    let getArg' = getArg args cli

    networkName <- getArg' (longOption "network") (decode @Network)
    nodePort <- getArg' (longOption "node-port") decode

    --_ <- getArg args (longOption "wallet-server-port") decode

    network <- HttpBridge.newNetworkLayer (T.pack . encode $ networkName) nodePort
    listen network logBlock
  where
    logBlock :: Block -> IO ()
    logBlock = T.putStrLn . fmt . build
