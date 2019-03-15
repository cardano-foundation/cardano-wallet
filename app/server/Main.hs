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
    ( build, fmt, (+||), (||+), (+|), (|+) )
import System.Console.Docopt
    ( Docopt, docopt, exitWithUsage, isPresent, longOption, parseArgsOrExit )
import System.Environment
    ( getArgs )
import System.Process (withCreateProcess, waitForProcess, proc, StdStream(..), CreateProcess(..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Say
    ( say, sayErr, sayString )

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

    let
        httpBridgeExe = "cardano-http-bridge"
        httpBridgeArgs = ["start", "--template", encode networkName
                         , "--port", show nodePort]
        httpBridgeProc =
            (proc httpBridgeExe httpBridgeArgs)
            { std_in = NoStream, std_out = Inherit, std_err = Inherit }

        listenThread = do
            threadDelay 1000000  -- wait 1sec for socket to appear
            network <- HttpBridge.newNetworkLayer (T.pack . encode $ networkName) nodePort
            listen network logBlock

    sayString $ "Starting " ++ httpBridgeExe ++ " " ++ unwords httpBridgeArgs
    withCreateProcess httpBridgeProc $ \_ _ _ ph -> do
        race_ listenThread $ do
            status <- waitForProcess ph
            sayErr . fmt $ ""+|httpBridgeExe|+" exited with "+||status||+""
        say "bye bye"

  where
    logBlock :: Block -> IO ()
    logBlock = say . fmt . build
