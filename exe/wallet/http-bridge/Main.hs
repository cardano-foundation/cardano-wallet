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
    ( appendName, logInfo )
import Cardano.CLI
    ( Environment (..)
    , Port (..)
    , commandWalletServe
    , execLaunchCommands
    , initTracer
    , makeCli
    , minSeverityFromArgs
    , parseWalletListen
    , runCli
    , runCliCommand
    , showT
    , verbosityFromArgs
    , verbosityToArgs
    )
import Cardano.Launcher
    ( Command (Command), StdStream (..) )
import Cardano.Wallet
    ( newWalletLayer )
import Cardano.Wallet.DaedalusIPC
    ( daedalusIPC )
import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge, Network (..), block0, byronFeePolicy )
import Cardano.Wallet.HttpBridge.Environment
    ( KnownNetwork )
import Cardano.Wallet.Network
    ( defaultRetryPolicy, waitForConnection )
import Cardano.Wallet.Primitive.AddressDerivation
    ( KeyToAddress )
import Cardano.Wallet.Version
    ( showVersion, version )
import Control.Concurrent.Async
    ( race_ )
import Data.Function
    ( (&) )
import Data.Proxy
    ( Proxy (..) )
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

import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
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
withBackend manager env@Environment {..} =
    parseArg (longOption "network") >>= \case
        Testnet ->
            runCliCommand env manager
                (execServe @'Testnet env)
                (execLaunch env)
        Mainnet ->
            runCliCommand env manager
                (execServe @'Mainnet env)
                (execLaunch env)

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

execLaunch :: Environment -> IO ()
execLaunch env@Environment {..} = do
    let verbosity = verbosityFromArgs args
    backendPort <- parseArg $ longOption "backend-port"
    listen <- parseWalletListen env
    network <- parseArg $ longOption "network"
    stateDir <- parseOptionalArg $ longOption "state-dir"
    tracer <- initTracer (minSeverityFromArgs args) "launch"
    execLaunchCommands tracer stateDir
        [ commandHttpBridge
              backendPort stateDir network verbosity
        , commandWalletServe "cardano-wallet"
              listen backendPort stateDir network verbosity Nothing
        ]
  where
    commandHttpBridge backendPort stateDir network verbosity =
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
    :: forall n. (KeyToAddress (HttpBridge n), KnownNetwork n)
    => Environment -> Proxy (HttpBridge n) -> IO ()
execServe env@Environment {..} _ = do
    tracer <- initTracer (minSeverityFromArgs args) "serve"
    logInfo tracer $ "Wallet backend server starting. "
        <> "Version "
        <> T.pack (showVersion version)
    walletListen <- parseWalletListen env
    backendPort <- parseArg $ longOption "backend-port"
    dbFile <- parseOptionalArg $ longOption "database"
    tracerDB <- appendName "DBLayer" tracer
    (_, db) <- Sqlite.newDBLayer tracerDB dbFile
    nl <- HttpBridge.newNetworkLayer @n (getPort backendPort)
    waitForConnection nl defaultRetryPolicy
    let tl = HttpBridge.newTransactionLayer @n
    wallet <- newWalletLayer tracer block0 byronFeePolicy db nl tl
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
