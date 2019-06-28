{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-unused-foralls #-}

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
    ( Port (..)
    , Verbosity (..)
    , cli
    , cmdAddress
    , cmdMnemonic
    , cmdTransaction
    , cmdVersion
    , cmdWallet
    , databaseOption
    , execLaunch
    , initTracer
    , listenOption
    , nodePortOption
    , optionT
    , stateDirOption
    , verbosityOption
    , verbosityToArgs
    , verbosityToMinSeverity
    )
import Cardano.Launcher
    ( Command (Command), StdStream (..) )
import Cardano.Wallet
    ( WalletLayer )
import Cardano.Wallet.Api.Server
    ( Listen (..) )
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
import Control.Applicative
    ( optional, (<|>) )
import Control.Concurrent.Async
    ( race_ )
import Control.Monad
    ( join )
import Data.Function
    ( (&) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..), showT )
import Network.Wai.Handler.Warp
    ( setBeforeMainLoop )
import Options.Applicative
    ( CommandFields
    , Mod
    , Parser
    , command
    , execParser
    , flag'
    , help
    , helper
    , info
    , long
    , metavar
    , progDesc
    , value
    )
import System.FilePath
    ( (</>) )

import qualified Cardano.BM.Configuration.Model as CM
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

main :: forall (n :: Network). IO ()
main = join $ execParser $ cli $ mempty
    <> cmdLaunch
    <> cmdServe
    <> cmdMnemonic
    <> cmdWallet @(HttpBridge n)
    <> cmdTransaction @(HttpBridge n)
    <> cmdAddress @(HttpBridge n)
    <> cmdVersion

{-------------------------------------------------------------------------------
                            Command - 'launch'

  cardano-wallet launch
    [--network=STRING]
    [(--port=INT | --random-port)]
    [--node-port=INT]
    [--state-dir=DIR]
    [(--quiet | --verbose )]
-------------------------------------------------------------------------------}

data LaunchArgs = LaunchArgs
    { _network :: Either Local Network
    , _listen :: Listen
    , _nodePort :: Port "Node"
    , _stateDir :: Maybe FilePath
    , _verbosity :: Verbosity
    }

-- | cardano-wallet launch
cmdLaunch
    :: Mod CommandFields (IO ())
cmdLaunch = command "launch" $ info (helper <*> cmd) $ mempty
    <> progDesc "Launch and monitor a wallet server and its chain producer."
  where
    cmd = fmap exec $ LaunchArgs
        <$> networkOption'
        <*> listenOption
        <*> nodePortOption
        <*> optional stateDirOption
        <*> verbosityOption
    exec (LaunchArgs network listen nodePort stateDir verbosity) = do
        execLaunch verbosity stateDir [commandHttpBridge, commandWalletServe]
      where
        commandHttpBridge =
            Command "cardano-http-bridge" arguments (return ()) Inherit
          where
            arguments = mconcat
                [ [ "start" ]
                , [ "--port", showT nodePort ]
                , [ "--template", case network of
                        Left Local -> "local"
                        Right n -> showT n
                  ]
                , maybe [] (\d -> ["--networks-dir", d]) stateDir
                , verbosityToArgs verbosity
                ]
        commandWalletServe =
            Command "cardano-wallet" arguments (return ()) Inherit
          where
            arguments = mconcat
                [ [ "serve" ]
                , [ "--network", case network of
                        Left Local -> "testnet"
                        Right n -> showT n
                  ]
                , case listen of
                    ListenOnRandomPort -> ["--random-port"]
                    ListenOnPort port  -> ["--port", showT port]
                , [ "--node-port", showT nodePort ]
                , maybe [] (\d -> ["--database", d </> "wallet.db"]) stateDir
                , verbosityToArgs verbosity
                ]

{-------------------------------------------------------------------------------
                            Command - 'serve'

  cardano-wallet serve
    [--network=STRING]
    [(--port=INT | --random-port)]
    [--node-port=INT]
    [--database=FILE]
    [(--quiet | --verbose )]
-------------------------------------------------------------------------------}

-- | Arguments for the 'serve' command
data ServeArgs = ServeArgs
    { _network :: Network
    , _listen :: Listen
    , _nodePort :: Port "Node"
    , _database :: Maybe FilePath
    , _verbosity :: Verbosity
    }

-- | cardano-wallet serve
cmdServe
    :: Mod CommandFields (IO ())
cmdServe = command "serve" $ info (helper <*> cmd) $ mempty
    <> progDesc "serve API that listens for commands/actions."
  where
    cmd = fmap withNetwork $ ServeArgs
        <$> networkOption
        <*> listenOption
        <*> nodePortOption
        <*> optional databaseOption
        <*> verbosityOption
    withNetwork args@(ServeArgs network _ _ _ _) = case network of
        Testnet -> exec @(HttpBridge 'Testnet) args
        Mainnet -> exec @(HttpBridge 'Mainnet) args
    exec
        :: forall t n s. (t ~ HttpBridge n, s ~ SeqState t)
        => (KeyToAddress t, KnownNetwork n)
        => ServeArgs
        -> IO ()
    exec (ServeArgs _ listen nodePort dbFile verbosity) = do
        (logCfg, tracer) <- initTracer (verbosityToMinSeverity verbosity) "serve"
        logInfo tracer "Wallet backend server starting..."
        logInfo tracer $ "Running as v" <> T.pack (showVersion version)
        logInfo tracer $ "Node is Http-Bridge on " <> toText (networkVal @n)
        newWalletLayer logCfg tracer >>= startServer tracer
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
            :: CM.Configuration
            -> Trace IO Text
            -> IO (WalletLayer s t)
        newWalletLayer logCfg tracer = do
            (nl, block0, feePolicy) <- newNetworkLayer
            let tl = HttpBridge.newTransactionLayer @n
            db <- newDBLayer logCfg tracer
            Wallet.newWalletLayer tracer block0 feePolicy db nl tl

        newNetworkLayer
            :: IO (NetworkLayer t IO, Block Tx, FeePolicy)
        newNetworkLayer = do
            nl <- HttpBridge.newNetworkLayer @n (getPort nodePort)
            waitForConnection nl defaultRetryPolicy
            return (nl, HttpBridge.block0, byronFeePolicy)

        newDBLayer
            :: CM.Configuration
            -> Trace IO Text
            -> IO (DBLayer IO s t)
        newDBLayer logCfg tracer = do
            tracerDB <- appendName "database" tracer
            (_, db) <- Sqlite.newDBLayer logCfg tracerDB dbFile
            return db

{-------------------------------------------------------------------------------
                                 Options
-------------------------------------------------------------------------------}

data Local = Local

-- | [--network=STRING], default: testnet
networkOption :: Parser Network
networkOption = optionT $ mempty
    <> long "network"
    <> metavar "STRING"
    <> help "target network to connect to: testnet or mainnet"
    <> value Testnet

-- | [--local-network|--network=STRING], default (Right Testnet)
networkOption' :: Parser (Either Local Network)
networkOption' =
    (Left Local <$ localNetworkOption) <|>  (Right <$> networkOption)
  where
    localNetworkOption = flag' False $ mempty
        <> long "local-network"
        <> help "connect to a local network"
