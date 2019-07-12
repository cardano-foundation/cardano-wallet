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
    , initTracer
    , listenOption
    , nodePortOption
    , optionT
    , resolveHomeDir
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
    ( HttpBridge, Network (..), byronFeePolicy, byronSlotLength )
import Cardano.Wallet.HttpBridge.Environment
    ( KnownNetwork (..) )
import Cardano.Wallet.HttpBridge.Primitive.Types
    ( Tx )
import Cardano.Wallet.Network
    ( ErrNetworkTip, NetworkLayer, defaultRetryPolicy, waitForConnection )
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
    ( (>=>) )
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
    ( getProgName )
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
main = runCli $ cli $ mempty
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
    , _stateDir :: FilePath
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
        <*> stateDirOption
        <*> verbosityOption
    exec (LaunchArgs network listen nodePort stateDirRaw verbosity) = do
        let withStateDir _ _ = pure ()
        stateDir <- resolveHomeDir stateDirRaw
        cmdName <- getProgName
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
        (cfg, sb, tr) <- initTracer (verbosityToMinSeverity verbosity) "serve"
        logInfo tr "Wallet backend server starting..."
        logInfo tr $ "Running as v" <> T.pack (showVersion version)
        logInfo tr $ "Node is Http-Bridge on " <> toText (networkVal @n)
        withDBLayer cfg tr $ newWalletLayer (sb, tr) >=> startServer tr
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
            :: (Switchboard Text, Trace IO Text)
            -> DBLayer IO s t
            -> IO (WalletLayer s t)
        newWalletLayer (sb, tracer) db = do
            (nl, block0, feePolicy) <- newNetworkLayer (sb, tracer)
            let tl = HttpBridge.newTransactionLayer @n
            let bp = BlockchainParameters block0 feePolicy byronSlotLength
            Wallet.newWalletLayer tracer bp db nl tl

        newNetworkLayer
            :: (Switchboard Text, Trace IO Text)
            -> IO (NetworkLayer t IO, Block Tx, FeePolicy)
        newNetworkLayer (sb, tracer) = do
            nl <- HttpBridge.newNetworkLayer @n (getPort nodePort)
            waitForService @ErrNetworkTip "http-bridge" (sb, tracer) nodePort $
                waitForConnection nl defaultRetryPolicy
            return (nl, HttpBridge.block0, byronFeePolicy)

        withDBLayer
            :: CM.Configuration
            -> Trace IO Text
            -> (DBLayer IO s t -> IO a)
            -> IO a
        withDBLayer logCfg tracer action = do
            tracerDB <- appendName "database" tracer
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
