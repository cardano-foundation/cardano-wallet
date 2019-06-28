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
    , Verbosity
    , commandWalletServe
    , execLaunchCommands
    , initTracer
    , makeCli
    , parseWalletListen
    , runCli
    , runCliCommand
    , showT
    , verbosityFromArgs
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
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Jormungandr.Environment
    ( KnownNetwork (..), Network (..) )
import Cardano.Wallet.Jormungandr.Network
    ( getBlock )
import Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx (..) )
import Cardano.Wallet.Network
    ( NetworkLayer (..), defaultRetryPolicy, waitForConnection )
import Cardano.Wallet.Primitive.AddressDerivation
    ( KeyToAddress )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( SeqState )
import Cardano.Wallet.Primitive.Fee
    ( FeePolicy )
import Cardano.Wallet.Primitive.Types
    ( Block (..), Hash (..) )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Cardano.Wallet.Version
    ( showVersion, version )
import Control.Concurrent.Async
    ( race_ )
import Data.Coerce
    ( coerce )
import Data.Function
    ( (&) )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Network.HTTP.Client
    ( Manager, defaultManagerSettings, newManager )
import Network.Wai.Handler.Warp
    ( setBeforeMainLoop )
import Servant.Client
    ( BaseUrl (..), Scheme (..) )
import System.Console.Docopt
    ( Docopt, longOption )
import Text.Heredoc
    ( here )

import qualified Cardano.Wallet as Wallet
import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Cardano.Wallet.Jormungandr.Network as Jormungandr
import qualified Cardano.Wallet.Jormungandr.Transaction as Jormungandr
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
    block0H <- parseArg $ longOption "genesis-hash"
    let verbosity = verbosityFromArgs args
    parseArg (longOption "network") >>= \case
        n@Testnet ->
            runCliCommand env manager
                (execServe @'Testnet verbosity listen backendPort dbFile block0H)
                (execLaunch env n verbosity listen backendPort block0H)
        n@Mainnet ->
            runCliCommand env manager
                (execServe @'Mainnet verbosity listen backendPort dbFile block0H)
                (execLaunch env n verbosity listen backendPort block0H)

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
  cardano-wallet launch [--network=STRING] [(--port=INT | --random-port)] [--backend-port=INT] [--state-dir=DIR] [(--quiet | --verbose )] --genesis-hash=HASH --genesis-block=PATH --node-config=PATH --node-secret=PATH
  cardano-wallet serve  [--network=STRING] [(--port=INT | --random-port)] [--backend-port=INT] [--database=FILE] [(--quiet | --verbose )] --genesis-hash=HASH
|]

cliOptions :: String
cliOptions = [here|
  --backend-port <INT>      port used for communicating with the backend node [default: 8081]
  --genesis-block <FILE>    path to genesis block
  --genesis-hash <HASH>     hash of genesis block
  --node-config <FILE>      path to node configuration (general)
  --node-secret <FILE>      path to node configuration (secret)
|]

cliExamples :: String
cliExamples = [here|
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

execLaunch
    :: Environment
    -> Network
    -> Verbosity
    -> Listen
    -> Port "Node"
    -> Hash "Genesis"
    -> IO ()
execLaunch Environment {..} network verbosity listen backendPort block0H = do
    genesisBlockPath <- parseArg $ longOption "genesis-block"
    nodeConfigPath <- parseArg $ longOption "node-config"
    nodeSecretPath <- parseArg $ longOption "node-secret"
    stateDir <- parseOptionalArg $ longOption "state-dir"
    tracer <- initTracer (verbosityToMinSeverity verbosity) "launch"
    execLaunchCommands tracer stateDir
        [ commandJormungandr
            genesisBlockPath nodeConfigPath nodeSecretPath
        , commandWalletServe "cardano-wallet-jormungandr"
            listen backendPort stateDir (showT network) verbosity (Just block0H)
        ]
  where
    commandJormungandr genesisBlockPath nodeConfigPath nodeSecretPath =
        Command "jormungandr" arguments (return ()) Inherit
      where
        arguments = mconcat
          [ [ "--genesis-block", genesisBlockPath ]
          , [ "--config", nodeConfigPath ]
          , [ "--secret", nodeSecretPath ]
          ]

{-------------------------------------------------------------------------------
                                 Serving
-------------------------------------------------------------------------------}

execServe
    :: forall n t s. (t ~ Jormungandr n, s ~ SeqState t)
    => (KeyToAddress t, KnownNetwork n)
    => Verbosity
    -> Listen
    -> Port "Node"
    -> Maybe FilePath
    -> Hash "Genesis"
    -> Proxy t
    -> IO ()
execServe verbosity listen backendPort dbFile block0H _ = do
    tracer <- initTracer (verbosityToMinSeverity verbosity) "serve"
    logInfo tracer "Wallet backend server starting..."
    logInfo tracer $ "Running as v" <> T.pack (showVersion version)
    logInfo tracer $ "Target node is Jörmungandr on " <> toText (networkVal @n)
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
        let tl = Jormungandr.newTransactionLayer @n block0H
        db <- newDBLayer tracer
        Wallet.newWalletLayer tracer block0 feePolicy db nl tl

    newNetworkLayer
        :: IO (NetworkLayer t IO, Block Tx, FeePolicy)
    newNetworkLayer = do
        let url = BaseUrl Http "localhost" (getPort backendPort) "/api"
        mgr <- newManager defaultManagerSettings
        let jormungandr = Jormungandr.mkJormungandrLayer mgr url
        let nl = Jormungandr.mkNetworkLayer jormungandr
        waitForConnection nl defaultRetryPolicy
        block0 <- unsafeRunExceptT $ getBlock jormungandr (coerce block0H)
        feePolicy <- unsafeRunExceptT $
            Jormungandr.getInitialFeePolicy jormungandr (coerce block0H)
        return (nl, block0, feePolicy)

    newDBLayer
        :: Trace IO Text
        -> IO (DBLayer IO s t)
    newDBLayer tracer = do
        tracerDB <- appendName "database" tracer
        (_, db) <- Sqlite.newDBLayer tracerDB dbFile
        return db
