{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Prelude

import Cardano.CLI
    ( Network, Port, decode, encode, getArg )
import Cardano.Launcher
    ( Command (Command), ProcessHasExited (ProcessHasExited), launch )
import Control.Concurrent
    ( threadDelay )
import Control.Monad
    ( when )
import Fmt
    ( blockListF, fmt )
import Say
    ( sayErr )
import System.Console.Docopt
    ( Docopt, docopt, exitWithUsage, isPresent, longOption, parseArgsOrExit )
import System.Environment
    ( getArgs )
import System.Exit
    ( exitWith )

import qualified Data.Text as T


-- | Command-Line Interface specification. See http://docopt.org/
cli :: Docopt
cli = [docopt|
cardano-wallet-launcher

Start the cardano wallet along with its API and underlying node.

Requires cardano-http-bridge. To install, follow instructions at
https://github.com/input-output-hk/cardano-http-bridge, and run
  cargo install --path .
in the directory.

Usage:
  cardano-wallet-launcher [options]
  cardano-wallet-launcher --help

Options:
  --network <NETWORK>          mainnet or testnet [default: mainnet]
  --wallet-server-port <PORT>  port used for serving the wallet API [default: 8090]
  --http-bridge-port <PORT>    port used for communicating with the http-bridge [default: 8080]
|]

main :: IO ()
main = do
    args <- parseArgsOrExit cli =<< getArgs
    when (args `isPresent` (longOption "help")) $ exitWithUsage cli

    bridgePort <- getArg args cli (longOption "http-bridge-port") decode
    walletPort <- getArg args cli (longOption "wallet-server-port") decode
    network <- getArg args cli (longOption "network") decode

    sayErr "Starting..."
    let commands =
            [ nodeHttpBridgeOn bridgePort
            , walletOn walletPort bridgePort network
            ]
    sayErr $ fmt $ blockListF commands
    (ProcessHasExited name code) <- launch commands
    sayErr $ T.pack name <> " exited with code " <> T.pack (show code) 
    exitWith code

nodeHttpBridgeOn :: Port "Node" -> Command
nodeHttpBridgeOn port = Command
    "cardano-http-bridge"
    [ "start"
    , "--port", encode port
    ]
    (return ())

walletOn :: Port "Wallet" -> Port "Node" -> Network -> Command
walletOn wp np net = Command
    "cardano-wallet-server"
    [ "--wallet-server-port", encode wp
    , "--http-bridge-port", encode np
    , "--network", encode net
    ]
    (threadDelay oneSecond)
  where
    oneSecond = 1000000
