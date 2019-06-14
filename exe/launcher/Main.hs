{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Prelude

import Cardano.CLI
    ( Port, help, parseArgWith )
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
import Data.Maybe
    ( fromMaybe )
import Data.Text.Class
    ( FromText (..), ToText (..) )
import Data.Version
    ( showVersion )
import Fmt
    ( blockListF, fmt )
import Paths_cardano_wallet
    ( version )
import Say
    ( sayErr, sayString )
import System.Console.Docopt
    ( Arguments
    , Docopt
    , Option
    , docopt
    , getArg
    , isPresent
    , longOption
    , parseArgsOrExit
    , shortOption
    )
import System.Directory
    ( createDirectory, doesDirectoryExist )
import System.Environment
    ( getArgs )
import System.Exit
    ( exitSuccess, exitWith )
import System.FilePath
    ( (</>) )

import qualified Cardano.Wallet.Api.Server as ApiServer
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
  cardano-wallet-launcher -h | --help
  cardano-wallet-launcher --version

Options:
  --api-server-port <INT>      port used for serving the wallet API [default: 8090]
  --api-server-random-port     serve wallet API on any available port (conflicts with --api-server-port)
  --network <STRING>           testnet, mainnet, or local [default: testnet]
  --http-bridge-port <PORT>    port used for communicating with the http-bridge [default: 8080]
  --state-dir <DIR>            write wallet state (blockchain and database) to this directory
|]

main :: IO ()
main = do
    args <- parseArgsOrExit cli =<< getArgs
    when (args `isPresent` (longOption "help")) $ help cli
    when (args `isPresent` (shortOption 'h')) $ help cli
    when (args `isPresent` (longOption "version")) $ do
        putStrLn (showVersion version)
        exitSuccess

    let stateDir = args `getArg` (longOption "state-dir")
    let network = fromMaybe "testnet" $ args `getArg` (longOption "network")

    apiServerPort <- args `parseArg` longOption "api-server-port"
    let apiServerListen =
            if args `isPresent` longOption "api-server-random-port"
            then ApiServer.ListenOnRandomPort
            else ApiServer.ListenOnPort apiServerPort

    bridgePort <- args `parseArg` longOption "http-bridge-port"

    sayErr "Starting..."
    installSignalHandlers
    maybe (pure ()) setupStateDir stateDir
    let commands =
            [ nodeHttpBridgeOn stateDir bridgePort network
            , walletOn stateDir apiServerListen bridgePort network
            ]
    sayErr $ fmt $ blockListF commands
    (ProcessHasExited name code) <- launch commands
    sayErr $ T.pack name <> " exited with code " <> T.pack (show code)
    exitWith code
  where
    parseArg :: FromText a => Arguments -> Option -> IO a
    parseArg = parseArgWith cli

nodeHttpBridgeOn
    :: Maybe FilePath
    -> Port "Node"
    -> String
    -> Command
nodeHttpBridgeOn stateDir port net =
    Command "cardano-http-bridge" args (return ()) Inherit
  where
    args =
        [ "start"
        , "--port", T.unpack (toText port)
        , "--template", net
        ] ++ networkArg
    networkArg = maybe [] (\d -> ["--networks-dir", d]) stateDir

walletOn
    :: Maybe FilePath
    -> ApiServer.Listen
    -> Port "Node"
    -> String
    -> Command
walletOn stateDir apiListen np net =
    Command "cardano-wallet" args (threadDelay oneSecond) Inherit
  where
    oneSecond = 1000000
    args = mconcat
        [ [ "server" ]
        , [ "--network", if net == "local" then "testnet" else net ]
        , apiServerPortArgument
        , [ "--bridge-port", showT np ]
        , maybe [] (\d -> ["--database", d </> "wallet.db"]) stateDir
        ]
    apiServerPortArgument = case apiListen of
        ApiServer.ListenOnPort p ->
            [ "--port", showT p ]
        ApiServer.ListenOnRandomPort ->
            [ "--random-port" ]
    showT :: ToText a => a -> String
    showT = T.unpack . toText

setupStateDir :: FilePath -> IO ()
setupStateDir dir = doesDirectoryExist dir >>= \case
    True -> sayString $ "Using state directory: " ++ dir
    False -> do
        sayString $ "Creating state directory: " ++ dir
        createDirectory dir
