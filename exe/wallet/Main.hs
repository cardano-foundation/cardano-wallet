{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- This module parses command line arguments for the wallet and executes
-- corresponding commands.
--
-- In essence, it's a proxy to the wallet server, which is required for most
-- commands. Commands are turned into corresponding API calls, and submitted
-- to an up-and-running server. Some commands do not require an active server
-- and can be run "offline".

module Main where

import Prelude hiding
    ( getLine )

import Cardano.BM.Trace
    ( Trace, appendName, logAlert, logInfo )
import Cardano.CLI
    ( Environment (..)
    , Port (..)
    , Verbosity (..)
    , initTracer
    , makeCli
    , minSeverityFromArgs
    , runCli
    , runCliCommand
    , showT
    , verbosityFromArgs
    , verbosityToArgs
    )
import Cardano.Launcher
    ( Command (Command)
    , ProcessHasExited (ProcessHasExited)
    , StdStream (..)
    , installSignalHandlers
    , launch
    )
import Cardano.Wallet
    ( newWalletLayer )
import Cardano.Wallet.Api.Server
    ( Listen (..) )
import Cardano.Wallet.DaedalusIPC
    ( daedalusIPC )
import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge, byronFeePolicy )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Jormungandr.Network
    ( getBlock )
import Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx (..) )
import Cardano.Wallet.Network
    ( NetworkLayer (..), defaultRetryPolicy, waitForConnection )
import Cardano.Wallet.Primitive.AddressDerivation
    ( KeyToAddress )
import Cardano.Wallet.Primitive.Fee
    ( FeePolicy )
import Cardano.Wallet.Primitive.Types
    ( Block (..), Hash (..) )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Cardano.Wallet.Version
    ( showVersion, version )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( race_ )
import Data.Coerce
    ( coerce )
import Data.Function
    ( (&) )
import Data.Maybe
    ( fromJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Fmt
    ( blockListF, fmt, nameF )
import Network.HTTP.Client
    ( Manager, defaultManagerSettings, newManager )
import Network.Wai.Handler.Warp
    ( setBeforeMainLoop )
import Servant.Client
    ( BaseUrl (..), Scheme (..) )
import System.Console.Docopt
    ( Docopt
    , getArg
    , longOption
    )
import System.Directory
    ( createDirectory, doesDirectoryExist )
import System.Exit
    ( exitWith )
import System.FilePath
    ( (</>) )
import Text.Heredoc
    ( here )

import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Cardano.Wallet.HttpBridge.Compatibility as HttpBridge
import qualified Cardano.Wallet.HttpBridge.Environment as HttpBridge
import qualified Cardano.Wallet.HttpBridge.Network as HttpBridge
import qualified Cardano.Wallet.HttpBridge.Transaction as HttpBridge
import qualified Cardano.Wallet.Jormungandr.Environment as Jormungandr
import qualified Cardano.Wallet.Jormungandr.Network as Jormungandr
import qualified Cardano.Wallet.Jormungandr.Transaction as Jormungandr
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp

{-------------------------------------------------------------------------------
                              Main entry point
-------------------------------------------------------------------------------}

main :: IO ()
main = runCli withHttpBridge cliHttpBridge

withHttpBridge :: Manager -> Environment -> IO ()
withHttpBridge manager env@Environment {..} =
    parseArg (longOption "network") >>= \case
        HttpBridge.Testnet ->
            runCliCommand env manager
                (execServeHttpBridge @'HttpBridge.Testnet env)
                (execLaunchHttpBridge env)
        HttpBridge.Mainnet ->
            runCliCommand env manager
                (execServeHttpBridge @'HttpBridge.Mainnet env)
                (execLaunchHttpBridge env)

withJormungandr :: Manager -> Environment -> IO ()
withJormungandr manager env@Environment {..} =
    parseArg (longOption "network") >>= \case
        Jormungandr.Testnet ->
            runCliCommand env manager
                (execServeJormungandr @'Jormungandr.Testnet env)
                (execLaunchJormungandr env)
        Jormungandr.Mainnet ->
            runCliCommand env manager
                (execServeJormungandr @'Jormungandr.Mainnet env)
                (execLaunchJormungandr env)

{-------------------------------------------------------------------------------
                               CLI definitions
-------------------------------------------------------------------------------}

cliHttpBridge :: Docopt
cliHttpBridge = makeCli
    cliCommandsHttpBridge
    cliOptionsHttpBridge
    cliExamplesHttpBridge

cliJormungandr :: Docopt
cliJormungandr = makeCli
    cliCommandsJormungandr
    cliOptionsJormungandr
    cliExamplesJormungandr

cliCommandsHttpBridge :: String
cliCommandsHttpBridge = [here|
  cardano-wallet launch [--network=STRING] [(--port=INT | --random-port)] [--backend-port=INT] [--state-dir=DIR] [(--quiet | --verbose )]
  cardano-wallet serve  [--network=STRING] [(--port=INT | --random-port)] [--backend-port=INT] [--database=FILE] [(--quiet | --verbose )]
|]

cliCommandsJormungandr :: String
cliCommandsJormungandr = [here|
  cardano-wallet launch [--network=STRING] [(--port=INT | --random-port)] [--backend-port=INT] [--state-dir=DIR] [(--quiet | --verbose )] --genesis-hash=HASH --genesis-block=PATH --node-config=PATH --node-secret=PATH
  cardano-wallet serve  [--network=STRING] [(--port=INT | --random-port)] [--backend-port=INT] [--database=FILE] [(--quiet | --verbose )] --genesis-hash=HASH
|]

cliOptionsHttpBridge :: String
cliOptionsHttpBridge = [here|
  --backend-port <INT>      port used for communicating with the HTTP bridge [default: 8080]
|]

cliOptionsJormungandr :: String
cliOptionsJormungandr = [here|
  --backend-port <INT>      port used for communicating with the backend node [default: 8081]
  --genesis-block <FILE>    path to genesis block
  --genesis-hash <HASH>     hash of genesis block
  --node-config <FILE>      path to node configuration (general)
  --node-secret <FILE>      path to node configuration (secret)
|]

cliExamplesHttpBridge :: String
cliExamplesHttpBridge = [here|
  # Launch and monitor a wallet server and its associated chain producer
  cardano-wallet launch --network mainnet --random-port --state-dir .state-dir

  # Start only a wallet server and connect it to an already existing chain producer
  cardano-wallet serve --backend-port 8080
|]

cliExamplesJormungandr :: String
cliExamplesJormungandr = [here|
  # Launch and monitor a wallet server and its associated chain producer
  cardano-wallet launch \
    --genesis-hash=dba597bee5f0987efbf56f6bd7f44c38158a7770d0cb28a26b5eca40613a7ebd \
    --genesis-block=block0.bin \
    --node-config config.yaml \
    --backend-port 8081 \
    --node-secret secret.yaml

  # Start only a wallet server and connect it to an already existing chain producer
  cardano-wallet serve --backend-port 8081
|]

{-------------------------------------------------------------------------------
                                Launching
-------------------------------------------------------------------------------}

execLaunchHttpBridge :: Environment -> IO ()
execLaunchHttpBridge env@Environment {..} = do
    -- NOTE: 'fromJust' is safe because the network value has a default.
    let network = fromJust $ args `getArg` longOption "network"
    let stateDir = args `getArg` longOption "state-dir"
    let verbosity = verbosityFromArgs args
    backendPort <- parseArg $ longOption "backend-port"
    listen <- parseWalletListen env
    tracer <- initTracer (minSeverityFromArgs args) "launch"
    execLaunchCommands tracer stateDir
        [ commandHttpBridge
              backendPort stateDir network verbosity
        , commandWalletServe
              listen backendPort stateDir network verbosity Nothing
        ]

execLaunchJormungandr :: Environment -> IO ()
execLaunchJormungandr env@Environment {..} = do
    -- NOTE: 'fromJust' is safe because the network value has a default.
    let network = fromJust $ args `getArg` longOption "network"
    let stateDir = args `getArg` longOption "state-dir"
    let verbosity = verbosityFromArgs args
    genesisBlockPath <- parseArg $ longOption "genesis-block"
    genesisHash <- Just <$> parseArg (longOption "genesis-hash")
    listen <- parseWalletListen env
    nodeConfigPath <- parseArg $ longOption "node-config"
    backendPort <- parseArg $ longOption "backend-port"
    nodeSecretPath <- parseArg $ longOption "node-secret"
    tracer <- initTracer (minSeverityFromArgs args) "launch"
    execLaunchCommands tracer stateDir
        [ commandJormungandr
            genesisBlockPath nodeConfigPath nodeSecretPath
        , commandWalletServe
            listen backendPort stateDir network verbosity genesisHash
        ]

-- | Execute 'launch' commands. This differs from the 'serve' command as it
-- takes care of also starting a node backend in two separate processes and
-- monitors both processes: if one terminates, then the other one is cancelled.
execLaunchCommands :: Trace IO Text -> Maybe FilePath -> [Command] -> IO ()
execLaunchCommands tracer stateDir commands = do
    installSignalHandlers
    maybe (pure ()) (setupStateDir tracer) stateDir
    logInfo tracer $ fmt $ nameF "launch" $ blockListF commands
    (ProcessHasExited pName code) <- launch commands
    logAlert tracer $ T.pack pName <> " exited with code " <> T.pack (show code)
    exitWith code

commandHttpBridge
    :: Port "Node" -> Maybe FilePath -> String -> Verbosity -> Command
commandHttpBridge backendPort stateDir network verbosity =
    Command "cardano-http-bridge" args (return ()) Inherit
  where
    args = mconcat
        [ [ "start" ]
        , [ "--port", showT backendPort ]
        , [ "--template", network ]
        , maybe [] (\d -> ["--networks-dir", d]) stateDir
        , verbosityToArgs verbosity
        ]

commandJormungandr :: FilePath -> FilePath -> FilePath -> Command
commandJormungandr genesisBlockPath nodeConfigPath nodeSecretPath =
    Command "jormungandr" args (return ()) Inherit
  where
    args = mconcat
      [ [ "--genesis-block", genesisBlockPath ]
      , [ "--config", nodeConfigPath ]
      , [ "--secret", nodeSecretPath ]
      ]

commandWalletServe
    :: Listen -> Port "Node" -> Maybe FilePath -> String -> Verbosity
    -> Maybe (Hash "Genesis") -> Command
commandWalletServe listen backendPort stateDir network verbosity mGenesisHash =
    Command "cardano-wallet" args (threadDelay oneSecond) Inherit
  where
    oneSecond = 1000000
    args = mconcat
        [ [ "serve" ]
        , [ "--network", if network == "local" then "testnet" else network ]
        , case listen of
            ListenOnRandomPort -> ["--random-port"]
            ListenOnPort port  -> ["--port", showT port]
        , [ "--backend-port", showT backendPort ]
        , maybe [] (\d -> ["--database", d </> "wallet.db"]) stateDir
        , verbosityToArgs verbosity
        , case mGenesisHash of
            Just genesisHash -> [ "--genesis-hash", showT genesisHash ]
            Nothing -> []
        ]

{-------------------------------------------------------------------------------
                                 Serving
-------------------------------------------------------------------------------}

execServeHttpBridge
    :: forall n. (KeyToAddress (HttpBridge n), HttpBridge.KnownNetwork n)
    => Environment -> Proxy (HttpBridge n) -> IO ()
execServeHttpBridge env@Environment {..} _ = do
    tracer <- initTracer (minSeverityFromArgs args) "serve"
    logInfo tracer $ "Wallet backend server starting. "
        <> "Version "
        <> T.pack (showVersion version)
    walletListen <- parseWalletListen env
    backendPort <- parseArg $ longOption "backend-port"
    let dbFile = args `getArg` longOption "database"
    tracerDB <- appendName "DBLayer" tracer
    (_, db) <- Sqlite.newDBLayer tracerDB dbFile
    nw <- HttpBridge.newNetworkLayer @n (getPort backendPort)
    waitForConnection nw defaultRetryPolicy
    let tl = HttpBridge.newTransactionLayer @n
    wallet <- newWalletLayer tracer HttpBridge.block0 byronFeePolicy db nw tl
    Server.withListeningSocket walletListen $ \(port, socket) -> do
        tracerIPC <- appendName "DaedalusIPC" tracer
        tracerApi <- appendName "api" tracer
        let beforeMainLoop = logInfo tracer $
                "Wallet backend server listening on: " <> toText port
        let settings = Warp.defaultSettings
                & setBeforeMainLoop beforeMainLoop
        let ipcServer = daedalusIPC tracerIPC port
        let apiServer = Server.start settings tracerApi socket wallet
        race_ ipcServer apiServer

-- | Start a web-server to serve the wallet backend API on the given port.
execServeJormungandr
    :: forall n . (KeyToAddress (Jormungandr n), Jormungandr.KnownNetwork n)
    => Environment -> Proxy (Jormungandr n) -> IO ()
execServeJormungandr env@Environment {..} _ = do
    tracer <- initTracer (minSeverityFromArgs args) "serve"
    logInfo tracer $ "Wallet backend server starting. "
        <> "Version "
        <> T.pack (showVersion version)
    walletListen <- parseWalletListen env
    backendPort <- parseArg $ longOption "backend-port"
    block0H <- parseArg $ longOption "genesis-hash"
    let dbFile = args `getArg` longOption "database"
    tracerDB <- appendName "DBLayer" tracer
    (_, db) <- Sqlite.newDBLayer tracerDB dbFile
    (nl, block0, feePolicy) <- newNetworkLayer
        (BaseUrl Http "localhost" (getPort backendPort) "/api") block0H
    waitForConnection nl defaultRetryPolicy
    let tl = Jormungandr.newTransactionLayer @n block0H
    wallet <- newWalletLayer tracer block0 feePolicy db nl tl
    Server.withListeningSocket walletListen $ \(port, socket) -> do
        tracerIPC <- appendName "DaedalusIPC" tracer
        tracerApi <- appendName "api" tracer
        let beforeMainLoop = logInfo tracer $
                "Wallet backend server listening on: " <> toText port
        let settings = Warp.defaultSettings
                & setBeforeMainLoop beforeMainLoop
        let ipcServer = daedalusIPC tracerIPC port
        let apiServer = Server.start settings tracerApi socket wallet
        race_ ipcServer apiServer
  where
    newNetworkLayer
        :: BaseUrl
        -> Hash "Genesis"
        -> IO (NetworkLayer (Jormungandr n) IO, Block Tx, FeePolicy)
    newNetworkLayer url block0H = do
        mgr <- newManager defaultManagerSettings
        let jormungandr = Jormungandr.mkJormungandrLayer mgr url
        let nl = Jormungandr.mkNetworkLayer jormungandr
        waitForConnection nl defaultRetryPolicy
        block0 <- unsafeRunExceptT $ getBlock jormungandr (coerce block0H)
        feePolicy <- unsafeRunExceptT $
            Jormungandr.getInitialFeePolicy jormungandr (coerce block0H)
        return (nl, block0, feePolicy)

{-------------------------------------------------------------------------------
                                 Helpers
-------------------------------------------------------------------------------}

-- | Parse and convert the `--port` or `--random-port` option into a 'Listen'
-- data-type.
parseWalletListen :: Environment -> IO Listen
parseWalletListen Environment {..} = do
    let useRandomPort = argPresent $ longOption "random-port"
    walletPort <- parseArg $ longOption "port"
    pure $ case (useRandomPort, walletPort) of
        (True, _) -> ListenOnRandomPort
        (False, port) -> ListenOnPort (getPort port)

-- | Initialize a state directory to store blockchain data such as blocks or
-- the wallet database.
setupStateDir :: Trace IO Text -> FilePath -> IO ()
setupStateDir tracer dir = doesDirectoryExist dir >>= \case
    True -> logInfo tracer $ "Using state directory: " <> T.pack dir
    False -> do
        logInfo tracer $ "Creating state directory: " <> T.pack dir
        createDirectory dir
