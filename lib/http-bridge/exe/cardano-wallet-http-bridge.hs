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
-- License: Apache-2.0
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
    ( logInfo )
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
    , getDataDir
    , initTracer
    , listenOption
    , nodePortOption
    , optionT
    , runCli
    , setupStateDir
    , stateDirOption
    , verbosityOption
    , verbosityToArgs
    , verbosityToMinSeverity
    )
import Cardano.Launcher
    ( StdStream (..) )
import Cardano.Wallet.Api.Server
    ( Listen (..) )
import Cardano.Wallet.HttpBridge
    ( serveWallet )
import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge, Network (..) )
import Cardano.Wallet.HttpBridge.Environment
    ( KnownNetwork (..), Local (..) )
import Cardano.Wallet.HttpBridge.Network
    ( HttpBridgeBackend (..), HttpBridgeConfig (..) )
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
import Data.Char
    ( toLower )
import Data.Maybe
    ( fromMaybe )
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
import System.Exit
    ( exitWith )
import System.FilePath
    ( (</>) )

import qualified Data.Text as T

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
        :: forall t k n s. (t ~ HttpBridge n, s ~ SeqState t, k ~ SeqKey)
        => (KeyToAddress t k, KnownNetwork n)
        => LaunchArgs
        -> IO ()
    exec (LaunchArgs network listen (Port nodePort) mStateDir verbosity) = do
        (cfg, sb, tr) <- initTracer (verbosityToMinSeverity verbosity) "serve"
        logInfo tr $ "Running as v" <> T.pack (showVersion version)
        let stateDir = fromMaybe (stateDirForNetwork dataDir network) mStateDir
        let bridgeConfig = HttpBridgeConfig network (Just stateDir) (Just (fromIntegral nodePort)) (verbosityToArgs verbosity) Inherit
        let dbFile = stateDir </> "wallet.db"
        setupStateDir (logInfo tr) stateDir
        exitWith =<< serveWallet @t @k @n @s (cfg, sb, tr) (Just dbFile) listen (Launch bridgeConfig) Nothing

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
    <> progDesc "Serve API that listens for commands/actions."
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
        :: forall t k n s. (t ~ HttpBridge n, s ~ SeqState t, k ~ SeqKey)
        => (KeyToAddress t k, KnownNetwork n)
        => ServeArgs
        -> IO ()
    exec (ServeArgs _ listen (Port nodePort) dbFile verbosity) = do
        (cfg, sb, tr) <- initTracer (verbosityToMinSeverity verbosity) "serve"
        logInfo tr $ "Running as v" <> T.pack (showVersion version)
        exitWith =<< serveWallet @t @k @n @s (cfg, sb, tr) dbFile listen (UseRunning (fromIntegral nodePort)) Nothing

{-------------------------------------------------------------------------------
                                 Options
-------------------------------------------------------------------------------}

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
    (Left Local <$ localNetworkOption) <|> (Right <$> networkOption)
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
