{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Prelude

import Cardano.CLI
    ( Port, parseArgWith )
import Cardano.Environment.HttpBridge
    ( Network, network )
import Cardano.Launcher
    ( Command (Command)
    , ProcessHasExited (ProcessHasExited)
    , StdStream (..)
    , installSignalHandlers
    , launch
    )
import Control.Concurrent
    ( threadDelay )
import Control.Monad
    ( when )
import Data.Text.Class
    ( FromText (..), ToText (..) )
import Fmt
    ( blockListF, fmt )
import Say
    ( sayErr )
import System.Console.Docopt
    ( Arguments
    , Docopt
    , Option
    , docopt
    , exitWithUsage
    , isPresent
    , longOption
    , parseArgsOrExit
    )
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
  --wallet-server-port <PORT>  port used for serving the wallet API [default: 8090]
  --http-bridge-port <PORT>    port used for communicating with the http-bridge [default: 8080]
|]

main :: IO ()
main = do
    args <- parseArgsOrExit cli =<< getArgs
    when (args `isPresent` (longOption "help")) $ exitWithUsage cli

    bridgePort <- args `parseArg` longOption "http-bridge-port"
    walletPort <- args `parseArg` longOption "wallet-server-port"

    sayErr "Starting..."
    installSignalHandlers
    let commands =
            [ nodeHttpBridgeOn bridgePort network
            , walletOn walletPort bridgePort
            ]
    sayErr $ fmt $ blockListF commands
    (ProcessHasExited name code) <- launch commands
    sayErr $ T.pack name <> " exited with code " <> T.pack (show code)
    exitWith code
  where
    parseArg :: FromText a => Arguments -> Option -> IO a
    parseArg = parseArgWith cli

nodeHttpBridgeOn :: Port "Node" -> Network -> Command
nodeHttpBridgeOn port net = Command
    "cardano-http-bridge"
    [ "start"
    , "--port", T.unpack (toText port)
    , "--template", T.unpack (toText net)
    ]
    (return ())
    Inherit

walletOn :: Port "Wallet" -> Port "Node" -> Command
walletOn wp np = Command
    "cardano-wallet"
    [ "server"
    , "--port", T.unpack (toText wp)
    , "--bridge-port", T.unpack (toText np)
    ]
    (threadDelay oneSecond)
    Inherit
  where
    oneSecond = 1000000
