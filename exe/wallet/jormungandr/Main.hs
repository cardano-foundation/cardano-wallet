{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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

import Cardano.BM.Backend.Switchboard
    ( Switchboard )
import Cardano.BM.Setup
    ( shutdown )
import Cardano.BM.Trace
    ( Trace, appendName, logAlert, logInfo )
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
    , putErrLn
    , requireFilePath
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
import Cardano.Wallet.Jormungandr.Binary
    ( getBlockId, runGetOrFail )
import Cardano.Wallet.Jormungandr.Compatibility
    ( BaseUrl (..), Jormungandr, Scheme (..), genConfigFile )
import Cardano.Wallet.Jormungandr.Environment
    ( KnownNetwork (..), Network (..) )
import Cardano.Wallet.Jormungandr.Network
    ( ErrGetBlockchainParams (..), getInitialBlockchainParameters )
import Cardano.Wallet.Network
    ( NetworkLayer (..), defaultRetryPolicy, waitForConnection )
import Cardano.Wallet.Primitive.AddressDerivation
    ( KeyToAddress )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState )
import Cardano.Wallet.Primitive.Types
    ( Hash (..) )
import Cardano.Wallet.Version
    ( showVersion, version )
import Control.Applicative
    ( optional )
import Control.Concurrent.Async
    ( race_ )
import Control.Monad
    ( (>=>) )
import Control.Monad.Trans.Except
    ( runExceptT )
import Data.Coerce
    ( coerce )
import Data.Function
    ( (&) )
import Data.Maybe
    ( fromMaybe )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..), showT )
import Network.HTTP.Client
    ( defaultManagerSettings, newManager )
import Network.Wai.Handler.Warp
    ( setBeforeMainLoop )
import Options.Applicative
    ( CommandFields
    , Mod
    , Parser
    , command
    , footer
    , help
    , helper
    , info
    , long
    , metavar
    , progDesc
    )
import System.Environment
    ( getProgName )
import System.Exit
    ( exitFailure )
import System.FilePath
    ( (</>) )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.Wallet as Wallet
import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Cardano.Wallet.Jormungandr.Network as Jormungandr
import qualified Cardano.Wallet.Jormungandr.Transaction as Jormungandr
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp

{-------------------------------------------------------------------------------
                              Main entry point

  FIXME:
  We instiantiate everything below to 'Testnet' because there's no way
  to actually implement the wallet, transaction and address command without
  any prior knowledge of the target network (since the serialization of
  addresses depends on that).

  This is okay for now as do only have access to a 'Testnet' anyway, but in the
  long run, we may want to (pick one):

  - Provide a `--network` parameter to pretty much every command
  - Construct two executables, one for 'Testnet' and one for 'Mainnet'
  - Use an environment variable
  - ?
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    dataDir <- getDataDir "jormungandr"
    runCli $ cli $ mempty
        <> cmdLaunch dataDir
        <> cmdServe
        <> cmdMnemonic
        <> cmdWallet @(Jormungandr 'Testnet)
        <> cmdTransaction @(Jormungandr 'Testnet)
        <> cmdAddress @(Jormungandr 'Testnet)
        <> cmdVersion

{-------------------------------------------------------------------------------
                            Command - 'launch'
-------------------------------------------------------------------------------}

-- | Arguments for the 'launch' command
data LaunchArgs = LaunchArgs
    { _listen :: Listen
    , _nodePort :: Port "Node"
    , _stateDir :: Maybe FilePath
    , _verbosity :: Verbosity
    , _jormungandrArgs :: JormungandrArgs
    }

data JormungandrArgs = JormungandrArgs
    { _genesisBlock :: FilePath
    , _bftLeaders :: FilePath
    }

cmdLaunch
    :: FilePath
    -> Mod CommandFields (IO ())
cmdLaunch dataDir = command "launch" $ info (helper <*> cmd) $ mempty
    <> progDesc "Launch and monitor a wallet server and its chain producers."
    <> footer
        "Please note that launch will generate a configuration for Jörmungandr \
        \in a folder specified by '--state-dir'."
  where
    cmd = fmap exec $ LaunchArgs
        <$> listenOption
        <*> nodePortOption
        <*> stateDirOption dataDir
        <*> verbosityOption
        <*> (JormungandrArgs
            <$> genesisBlockOption
            <*> bftLeadersOption)
    exec (LaunchArgs listen nodePort mStateDir verbosity jArgs) = do
        requireFilePath (_genesisBlock jArgs)
        requireFilePath (_bftLeaders jArgs)
        cmdName <- getProgName
        block0H <- parseBlock0H (_genesisBlock jArgs)
        let baseUrl = BaseUrl Http "127.0.0.1" (getPort nodePort) "/api"
        let stateDir = fromMaybe (dataDir </> "testnet") mStateDir
        let nodeConfig = stateDir </> "jormungandr-config.json"
        let withStateDir tracer _ = do
                genConfigFile stateDir baseUrl
                    & Aeson.encode
                    & BL.writeFile nodeConfig
                logInfo tracer $ mempty
                    <> "Generated Jörmungandr's configuration to: "
                    <> T.pack nodeConfig
        execLaunch verbosity stateDir withStateDir
            [ commandJormungandr nodeConfig jArgs
            , commandWalletServe cmdName stateDir block0H
            ]
      where
        commandJormungandr nodeConfig (JormungandrArgs block0 bftLeaders) =
            Command "jormungandr" arguments (return ()) Inherit
          where
            logLevel = C.toLower <$> show (verbosityToMinSeverity verbosity)
            arguments = mconcat
              [ [ "--genesis-block", block0 ]
              , [ "--config", nodeConfig ]
              , [ "--secret", bftLeaders ]
              , [ "--log-level", logLevel ]
              ]

        commandWalletServe cmdName stateDir block0H =
            Command cmdName arguments (return ()) Inherit
          where
            arguments = mconcat
                [ [ "serve" ]
                , case listen of
                    ListenOnRandomPort -> ["--random-port"]
                    ListenOnPort port  -> ["--port", showT port]
                , [ "--node-port", showT nodePort ]
                , [ "--database", stateDir </> "wallet.db" ]
                , verbosityToArgs verbosity
                , [ "--genesis-hash", showT block0H ]
                ]

parseBlock0H :: FilePath -> IO (Hash "Genesis")
parseBlock0H file = do
    bytes <- BL.readFile file
    case runGetOrFail getBlockId bytes of
        Right (_, _, block0H) -> return (coerce block0H)
        Left _ -> do
            putErrLn $ mempty
                <> "As far as I can tell, this isn't a valid block file: "
                <> T.pack file
            exitFailure

{-------------------------------------------------------------------------------
                            Command - 'serve'
-------------------------------------------------------------------------------}

-- | Arguments for the 'serve' command
data ServeArgs = ServeArgs
    { _listen :: Listen
    , _nodePort :: Port "Node"
    , _database :: Maybe FilePath
    , _verbosity :: Verbosity
    , _block0H :: Hash "Genesis"
    }

cmdServe
    :: Mod CommandFields (IO ())
cmdServe = command "serve" $ info (helper <*> cmd) $ mempty
    <> progDesc "serve API that listens for commands/actions."
  where
    cmd = fmap exec $ ServeArgs
        <$> listenOption
        <*> nodePortOption
        <*> optional databaseOption
        <*> verbosityOption
        <*> genesisHashOption
    exec
        :: forall t n s. (n ~ 'Testnet, t ~ Jormungandr n, s ~ SeqState t)
        => (KeyToAddress t, KnownNetwork n)
        => ServeArgs
        -> IO ()
    exec (ServeArgs listen nodePort dbFile verbosity block0H) = do
        (cfg, sb, tr) <- initTracer (verbosityToMinSeverity verbosity) "serve"
        logInfo tr "Wallet backend server starting..."
        logInfo tr $ "Running as v" <> T.pack (showVersion version)
        logInfo tr $ "Node is Jörmungandr on " <> toText (networkVal @n)
        withDBLayer cfg tr $ newWalletLayer (sb, tr) >=> startServer tr
      where
        startServer
            :: Trace IO Text
            -> WalletLayer s t
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
            -> DBLayer IO s t
            -> IO (WalletLayer s t)
        newWalletLayer (sb, tracer) db = do
            (nl, blockchainParams) <- newNetworkLayer (sb, tracer)
            let tl = Jormungandr.newTransactionLayer @n block0H
            Wallet.newWalletLayer tracer blockchainParams db nl tl

        newNetworkLayer
            :: (Switchboard Text, Trace IO Text)
            -> IO (NetworkLayer t IO, BlockchainParameters t)
        newNetworkLayer (sb, tracer) = do
            let url = BaseUrl Http "localhost" (getPort nodePort) "/api"
            mgr <- newManager defaultManagerSettings
            let jor = Jormungandr.mkJormungandrLayer mgr url
            let nl = Jormungandr.mkNetworkLayer jor
            waitForService "Jörmungandr" (sb, tracer) nodePort $
                waitForConnection nl defaultRetryPolicy
            blockchainParams <-
                runExceptT (getInitialBlockchainParameters jor (coerce block0H)) >>= \case
                Right a -> return a
                Left (ErrGetBlockchainParamsNetworkUnreachable _) ->
                    handleNetworkUnreachable tracer
                Left (ErrGetBlockchainParamsGenesisNotFound _) ->
                    handleGenesisNotFound (sb, tracer)
                Left (ErrGetBlockchainParamsIncompleteParams _) ->
                    handleNoInitialPolicy tracer
            return (nl, blockchainParams)

        withDBLayer
            :: CM.Configuration
            -> Trace IO Text
            -> (DBLayer IO s t -> IO a)
            -> IO a
        withDBLayer logCfg tracer action = do
            let tracerDB = appendName "database" tracer
            Sqlite.withDBLayer logCfg tracerDB dbFile action

        handleGenesisNotFound
            :: (Switchboard Text, Trace IO Text)
            -> IO a
        handleGenesisNotFound (sb, tracer) = do
            logAlert tracer
                "Failed to retrieve the genesis block. The block doesn't exist!"
            shutdown sb
            putErrLn $ T.pack $ mconcat
                [ "Hint: double-check the genesis hash you've just gave "
                , "me via '--genesis-hash' (i.e. ", showT block0H, ")."
                ]
            exitFailure

        handleNetworkUnreachable
            :: Trace IO Text
            -> IO a
        handleNetworkUnreachable tracer = do
            logAlert tracer "It looks like Jörmungandr is down?"
            exitFailure

        handleNoInitialPolicy
            :: Trace IO Text
            -> IO a
        handleNoInitialPolicy tracer = do
            logAlert tracer $ mconcat
                [ "I successfully retrieved the genesis block from Jörmungandr, "
                , "but there's no initial fee policy defined?"
                ]
            exitFailure

{-------------------------------------------------------------------------------
                                 Options
-------------------------------------------------------------------------------}

-- | --bft-leaders=FILE
bftLeadersOption :: Parser FilePath
bftLeadersOption = optionT $ mempty
    <> long "bft-leaders"
    <> metavar "FILE"
    <> help "Path to BFT leaders' secrets (.yaml/.json)."

-- | --genesis-block=FILE
genesisBlockOption :: Parser FilePath
genesisBlockOption = optionT $ mempty
    <> long "genesis-block"
    <> metavar "FILE"
    <> help "Path to the genesis block in binary format."

-- | --genesis-hash=STRING
genesisHashOption :: Parser (Hash "Genesis")
genesisHashOption = optionT $ mempty
    <> long "genesis-hash"
    <> metavar "STRING"
    <> help "Blake2b_256 hash of the genesis block, in base 16."
