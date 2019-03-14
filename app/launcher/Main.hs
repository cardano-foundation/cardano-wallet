{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import CLI
    ( Decodable (decode), Encodable (encode), Network, Port (..), getArg )
import Control.Monad
    ( forM_, when )
import Launcher
    ( Command (Command)
    , ProcessHasExited (ProcessHasExited)
    , kill
    , launch
    , monitor
    )
import Prelude
import System.Console.Docopt
    ( Docopt, docopt, exitWithUsage, isPresent, longOption, parseArgsOrExit )
import System.Environment
    ( getArgs )
import System.Exit
    ( exitWith )

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
  --node-port <PORT>           port used for node-wallet communication [default: 8080]
  --network <NETWORK>          mainnet or testnet [default: mainnet]
|]



main :: IO ()
main = do
    args <- parseArgsOrExit cli =<< getArgs
    when (args `isPresent` (longOption "help")) $ exitWithUsage cli
    let getArg' = getArg args cli

    nodePort <- getArg' (longOption "node-port") decode
    walletPort <- getArg' (longOption "wallet-server-port") decode
    network <- getArg' (longOption "network") decode

    putStrLn $
        "Starting wallet on port " ++ (encode walletPort) ++
        ",\n         connecting to node on port " ++ (encode nodePort)

    running <- launch
        [ nodeHttpBridgeOn nodePort network
        , walletOn walletPort nodePort network
        , Command "./app/launcher/mock/node-exit-0.sh" []
        ]

    (ProcessHasExited name code) <- monitor running
    putStrLn $ name <> " exited with code " <> show code 
    forM_ running kill
    exitWith code

nodeHttpBridgeOn :: Port "Node" -> Network -> Command
nodeHttpBridgeOn port _network = Command
    "cardano-http-bridge"
    ["start", "--port", encode port]


walletOn :: Port "Wallet" -> Port "Node" -> Network -> Command
walletOn wp np net = Command
    "cardano-wallet-server"
    ["--wallet-server-port", encode wp,
    "--node-port", encode np,
    "--network", encode net]
