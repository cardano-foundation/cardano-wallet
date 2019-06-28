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
    ( Trace, appendName, logInfo )
import Cardano.CLI
    ( Environment (..)
    , Port (..)
    , Verbosity (..)
    , commandWalletServe
    , execLaunchCommands
    , initTracer
    , makeCli
    , parseWalletListen
    , runCli
    , runCliCommand
    , showT
    , verbosityFromArgs
    , verbosityToArgs
    , verbosityToMinSeverity
    )
import Cardano.Launcher
    ( Command (Command), StdStream (..) )
import Cardano.Wallet
    ( WalletLayer )
import Cardano.Wallet.Api.Server
    ( Listen )
import Cardano.Wallet.DaedalusIPC
    ( daedalusIPC )
import Cardano.Wallet.DB
    ( DBLayer )
import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge, Network (..), byronFeePolicy )
import Cardano.Wallet.HttpBridge.Environment
    ( KnownNetwork (..) )
import Cardano.Wallet.HttpBridge.Primitive.Types
    ( Tx )
import Cardano.Wallet.Network
    ( NetworkLayer, defaultRetryPolicy, waitForConnection )
import Cardano.Wallet.Primitive.AddressDerivation
    ( KeyToAddress )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( SeqState )
import Cardano.Wallet.Primitive.Fee
    ( FeePolicy )
import Cardano.Wallet.Primitive.Types
    ( Block )
import Cardano.Wallet.Version
    ( showVersion, version )
import Control.Concurrent.Async
    ( race_ )
import Data.Function
    ( (&) )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Network.HTTP.Client
    ( Manager )
import Network.Wai.Handler.Warp
    ( setBeforeMainLoop )
import System.Console.Docopt
    ( Docopt, longOption )
import Text.Heredoc
    ( here )

import qualified Cardano.Wallet as Wallet
import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Cardano.Wallet.HttpBridge.Compatibility as HttpBridge
import qualified Cardano.Wallet.HttpBridge.Network as HttpBridge
import qualified Cardano.Wallet.HttpBridge.Transaction as HttpBridge
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp

{-------------------------------------------------------------------------------
                              Main entry point
-------------------------------------------------------------------------------}

main :: IO ()
main = runCli withBackend cliDefinition

withBackend :: Manager -> Environment -> IO ()
withBackend manager env@Environment {..} = do
    listen <- parseWalletListen env
    backendPort <- parseArg $ longOption "backend-port"
    dbFile <- parseOptionalArg $ longOption "database"
    stateDir <- parseOptionalArg $ longOption "state-dir"
    let verbosity = verbosityFromArgs args
    -- FIXME
    -- We also need to support the special arg 'local' for the bridge... or not.
    parseArg (longOption "network") >>= \case
        Testnet ->
            runCliCommand env manager
                (execServe @'Testnet verbosity listen backendPort dbFile)
                (execLaunch "testnet" verbosity listen backendPort stateDir)
        Mainnet ->
            runCliCommand env manager
                (execServe @'Mainnet verbosity listen backendPort dbFile)
                (execLaunch "mainnet" verbosity listen backendPort stateDir)

{-------------------------------------------------------------------------------
                               CLI definitions
-------------------------------------------------------------------------------}

cliDefinition :: Docopt
cliDefinition = makeCli
    cliCommands
    cliOptions
    cliExamples

cliCommands :: String
cliCommands = [here|
  cardano-wallet launch [--network=STRING] [(--port=INT | --random-port)] [--backend-port=INT] [--state-dir=DIR] [(--quiet | --verbose )]
  cardano-wallet serve  [--network=STRING] [(--port=INT | --random-port)] [--backend-port=INT] [--database=FILE] [(--quiet | --verbose )]
|]

cliOptions :: String
cliOptions = [here|
  --backend-port <INT>      port used for communicating with the HTTP bridge [default: 8080]
|]

cliExamples :: String
cliExamples = [here|
  # Launch and monitor a wallet server and its associated chain producer
  cardano-wallet launch --network mainnet --random-port --state-dir .state-dir

  # Start only a wallet server and connect it to an already existing chain producer
  cardano-wallet serve --backend-port 8080
|]

{-------------------------------------------------------------------------------
                                Launching
-------------------------------------------------------------------------------}

execLaunch
    :: String
    -> Verbosity
    -> Listen
    -> Port "Node"
    -> Maybe FilePath
    -> IO ()
execLaunch network verbosity listen backendPort stateDir = do
    tracer <- initTracer (verbosityToMinSeverity verbosity) "launch"
    execLaunchCommands tracer stateDir
        [ commandHttpBridge
        , commandWalletServe "cardano-wallet"
              listen backendPort stateDir network verbosity Nothing
        ]
  where
    commandHttpBridge =
        Command "cardano-http-bridge" arguments (return ()) Inherit
      where
        arguments = mconcat
            [ [ "start" ]
            , [ "--port", showT backendPort ]
            , [ "--template", network ]
            , maybe [] (\d -> ["--networks-dir", d]) stateDir
            , verbosityToArgs verbosity
            ]

{-------------------------------------------------------------------------------
                                 Serving
-------------------------------------------------------------------------------}

execServe
    :: forall n t s. (t ~ HttpBridge n, s ~ SeqState t)
    => (KeyToAddress t, KnownNetwork n)
    => Verbosity
    -> Listen
    -> Port "Node"
    -> Maybe FilePath
    -> Proxy t
    -> IO ()
execServe verbosity listen backendPort dbFile _ = do
    tracer <- initTracer (verbosityToMinSeverity verbosity) "serve"
    logInfo tracer "Wallet backend server starting..."
    logInfo tracer $ "Running as v" <> T.pack (showVersion version)
    logInfo tracer $ "Target node is Http-Bridge on " <> toText (networkVal @n)
    newWalletLayer tracer >>= startServer tracer
  where
    startServer
        :: Trace IO Text
        -> WalletLayer s t
        -> IO ()
    startServer tracer wallet = do
        Server.withListeningSocket listen $ \(port, socket) -> do
            tracerIPC <- appendName "daedalus-ipc" tracer
            tracerApi <- appendName "api" tracer
            let beforeMainLoop = logInfo tracer $
                    "Wallet backend server listening on: " <> toText port
            let settings = Warp.defaultSettings
                    & setBeforeMainLoop beforeMainLoop
            let ipcServer = daedalusIPC tracerIPC port
            let apiServer = Server.start settings tracerApi socket wallet
            race_ ipcServer apiServer

    newWalletLayer
        :: Trace IO Text
        -> IO (WalletLayer s t)
    newWalletLayer tracer = do
        (nl, block0, feePolicy) <- newNetworkLayer
        let tl = HttpBridge.newTransactionLayer @n
        db <- newDBLayer tracer
        Wallet.newWalletLayer tracer block0 feePolicy db nl tl

    newNetworkLayer
        :: IO (NetworkLayer t IO, Block Tx, FeePolicy)
    newNetworkLayer = do
        nl <- HttpBridge.newNetworkLayer @n (getPort backendPort)
        waitForConnection nl defaultRetryPolicy
        return (nl, HttpBridge.block0, byronFeePolicy)

    newDBLayer
        :: Trace IO Text
        -> IO (DBLayer IO s t)
    newDBLayer tracer = do
        tracerDB <- appendName "database" tracer
        (_, db) <- Sqlite.newDBLayer tracerDB dbFile
        return db
