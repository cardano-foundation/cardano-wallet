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

import Cardano.BM.Backend.Switchboard
    ( Switchboard )
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
    , getDataDir
    , initTracer
    , listenOption
    , nodePortOption
    , optionT
    , runCli
    , stateDirOption
    , verbosityOption
    , verbosityToArgs
    , verbosityToMinSeverity
    , waitForService
    )
import Cardano.Launcher
    ( Command (Command), StdStream (..) )
import Cardano.Wallet
    ( BlockchainParameters (..), WalletLayer )
import Cardano.Wallet.Api.Server
    ( Listen (..) )
import Cardano.Wallet.DaedalusIPC
    ( daedalusIPC )
import Cardano.Wallet.DB
    ( DBLayer )
import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge, Network (..), byronBlockchainParameters )
import Cardano.Wallet.HttpBridge.Environment
    ( KnownNetwork (..) )
import Cardano.Wallet.Network
    ( NetworkLayer, defaultRetryPolicy, waitForConnection )
import Cardano.Wallet.Primitive.AddressDerivation
    ( KeyToAddress )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( SeqKey )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState )
import Cardano.Wallet.Version
    ( showVersion, version )
import Control.Applicative
    ( optional, (<|>) )
import Control.Concurrent.Async
    ( race_ )
import Control.Monad
    ( (>=>) )
import Data.Char
    ( toLower )
import Data.Function
    ( (&) )
import Data.Maybe
    ( fromMaybe )
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
    , flag'
    , help
    , helper
    , info
    , long
    , metavar
    , progDesc
    , value
    )
import System.Environment
    ( getExecutablePath )
import System.FilePath
    ( (</>) )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.Wallet as Wallet
import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Cardano.Wallet.HttpBridge.Network as HttpBridge
import qualified Cardano.Wallet.HttpBridge.Transaction as HttpBridge
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp

{-------------------------------------------------------------------------------
                              Main entry point
-------------------------------------------------------------------------------}

main :: forall (n :: Network). IO ()
main = do
    dataDir <- getDataDir "http-bridge"
    runCli $ cli $ mempty
        <> cmdLaunch dataDir
        <> cmdServe
        <> cmdMnemonic
        <> cmdWallet @(HttpBridge n)
        <> cmdTransaction @(HttpBridge n)
        <> cmdAddress @(HttpBridge n)
        <> cmdVersion

{-------------------------------------------------------------------------------
                            Command - 'launch'
-------------------------------------------------------------------------------}

-- | Arguments for the 'launch' command
data LaunchArgs = LaunchArgs
    { _network :: Either Local Network
    , _listen :: Listen
    , _nodePort :: Port "Node"
    , _stateDir :: Maybe FilePath
    , _verbosity :: Verbosity
    }

cmdLaunch
    :: FilePath
    -> Mod CommandFields (IO ())
cmdLaunch dataDir = command "launch" $ info (helper <*> cmd) $ mempty
    <> progDesc "Launch and monitor a wallet server and its chain producer."
  where
    cmd = fmap withNetwork $ LaunchArgs
        <$> networkOption'
        <*> listenOption
        <*> nodePortOption
        <*> stateDirOption dataDir
        <*> verbosityOption
    withNetwork args@(LaunchArgs network _ _ _ _) = case network of
        Left Local -> exec @(HttpBridge 'Testnet) args
        Right Testnet -> exec @(HttpBridge 'Testnet) args
        Right Mainnet -> exec @(HttpBridge 'Mainnet) args
    exec
        :: forall t n s.
           LaunchArgs
        -> IO ()
    exec (LaunchArgs network listen nodePort mStateDir verbosity) = do
        let withStateDir _ _ = pure ()
        let stateDir = fromMaybe (stateDirForNetwork dataDir network) mStateDir
        cmdName <- getExecutablePath
        execLaunch verbosity stateDir withStateDir
            [ commandHttpBridge stateDir
            , commandWalletServe cmdName stateDir
            ]
      where
        commandHttpBridge stateDir =
            Command "cardano-http-bridge" arguments (return ()) Inherit
          where
            arguments = mconcat
                [ [ "start" ]
                , [ "--port", showT nodePort ]
                , [ "--template", case network of
                        Left Local -> "local"
                        Right n -> showT n
                  ]
                , [ "--networks-dir", stateDir ]
                , verbosityToArgs verbosity
                ]
        commandWalletServe cmdName stateDir =
            Command cmdName arguments (return ()) Inherit
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
                , [ "--database", stateDir </> "wallet.db" ]
                , verbosityToArgs verbosity
                ]

{-------------------------------------------------------------------------------
                            Command - 'serve'
-------------------------------------------------------------------------------}

-- | Arguments for the 'serve' command
data ServeArgs = ServeArgs
    { _network :: Network
    , _listen :: Listen
    , _nodePort :: Port "Node"
    , _database :: Maybe FilePath
    , _verbosity :: Verbosity
    }

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
        => (KeyToAddress t SeqKey, KnownNetwork n)
        => ServeArgs
        -> IO ()
    exec (ServeArgs _ listen nodePort dbFile verbosity) = do
        (cfg, sb, tr) <- initTracer (verbosityToMinSeverity verbosity) "serve"
        logInfo tr "Wallet backend server starting..."
        logInfo tr $ "Running as v" <> T.pack (showVersion version)
        logInfo tr $ "Node is Http-Bridge on " <> toText (networkVal @n)
        withDBLayer cfg tr $ newWalletLayer (sb, tr) >=> startServer tr
      where
        startServer
            :: Trace IO Text
            -> WalletLayer s t SeqKey
            -> IO ()
        startServer tracer wallet = do
            Server.withListeningSocket listen $ \(port, socket) -> do
                let tracerIPC = appendName "daedalus-ipc" tracer
                let tracerApi = appendName "api" tracer
                let beforeMainLoop = logInfo tracer $
                        "Wallet backend server listening on: " <> toText port
                let settings = Warp.defaultSettings
                        & setBeforeMainLoop beforeMainLoop
                let ipcServer = daedalusIPC tracerIPC port
                let apiServer = Server.start settings tracerApi socket wallet
                race_ ipcServer apiServer

        newWalletLayer
            :: (Switchboard Text, Trace IO Text)
            -> DBLayer IO s t SeqKey
            -> IO (WalletLayer s t SeqKey)
        newWalletLayer (sb, tracer) db = do
            (nl, bp) <- newNetworkLayer (sb, tracer)
            let tl = HttpBridge.newTransactionLayer @n
            Wallet.newWalletLayer tracer bp db nl tl

        newNetworkLayer
            :: (Switchboard Text, Trace IO Text)
            -> IO (NetworkLayer t IO, BlockchainParameters t)
        newNetworkLayer (sb, tracer) = do
            nl <- HttpBridge.newNetworkLayer @n (getPort nodePort)
            waitForService "http-bridge" (sb, tracer) nodePort $
                waitForConnection nl defaultRetryPolicy
            return (nl, byronBlockchainParameters)

        withDBLayer
            :: CM.Configuration
            -> Trace IO Text
            -> (DBLayer IO s t SeqKey -> IO a)
            -> IO a
        withDBLayer logCfg tracer action = do
            let tracerDB = appendName "database" tracer
            Sqlite.withDBLayer logCfg tracerDB dbFile action

{-------------------------------------------------------------------------------
                                 Options
-------------------------------------------------------------------------------}

data Local = Local

-- | [--network=STRING], default: testnet
networkOption :: Parser Network
networkOption = optionT $ mempty
    <> long "network"
    <> metavar "STRING"
    <> help "target network to connect to: testnet or mainnet (default: testnet)"
    <> value Testnet

-- | [--local-network|--network=STRING], default (Right Testnet)
networkOption' :: Parser (Either Local Network)
networkOption' =
    (Left Local <$ localNetworkOption) <|>  (Right <$> networkOption)
  where
    localNetworkOption = flag' False $ mempty
        <> long "local-network"
        <> help "connect to a local network (conflicts with --network)"

stateDirForNetwork
    :: FilePath
    -- ^ Backend-specific data directory (result of 'getDataDir')
    -> Either Local Network
    -- ^ The network selected by the user
    -> FilePath
stateDirForNetwork backendDir net = backendDir </> case net of
    Left Local -> "local"
    Right network -> map toLower $ show network
