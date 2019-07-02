{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
    , runCli
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
import Cardano.Wallet.Jormungandr.Binary
    ( getBlockId, runGet )
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
import Control.Applicative
    ( optional )
import Control.Concurrent.Async
    ( race_ )
import Data.Coerce
    ( coerce )
import Data.Function
    ( (&) )
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
    , help
    , helper
    , info
    , long
    , metavar
    , progDesc
    )
import Servant.Client
    ( BaseUrl (..), Scheme (..) )
import System.Environment
    ( getProgName )
import System.FilePath
    ( (</>) )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.Wallet as Wallet
import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Cardano.Wallet.Jormungandr.Network as Jormungandr
import qualified Cardano.Wallet.Jormungandr.Transaction as Jormungandr
import qualified Data.ByteString.Lazy as BL
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
main = runCli $ cli $ mempty
    <> cmdLaunch
    <> cmdServe
    <> cmdMnemonic
    <> cmdWallet @(Jormungandr 'Testnet)
    <> cmdTransaction @(Jormungandr 'Testnet)
    <> cmdAddress @(Jormungandr 'Testnet)
    <> cmdVersion

{-------------------------------------------------------------------------------
                            Command - 'launch'

  cardano-wallet launch
    [(--port=INT | --random-port)]
    [--node-port=INT]
    [--state-dir=DIR]
    [(--quiet | --verbose )]
    --genesis-block=FILE
    --node-config=FILE
    --node-secret=FILE
-------------------------------------------------------------------------------}

data LaunchArgs = LaunchArgs
    { _listen :: Listen
    , _nodePort :: Port "Node"
    , _stateDir :: Maybe FilePath
    , _verbosity :: Verbosity
    , _jormungandrArgs :: JormungandrArgs
    }

data JormungandrArgs = JormungandrArgs
    { _genesisBlock :: FilePath
    , _nodeConfig :: FilePath
    , _nodeSecret :: FilePath
    }

-- | cardano-wallet launch
cmdLaunch
    :: Mod CommandFields (IO ())
cmdLaunch = command "launch" $ info (helper <*> cmd) $ mempty
    <> progDesc "Launch and monitor a wallet server and its chain producers."
  where
    cmd = fmap exec $ LaunchArgs
        <$> listenOption
        <*> nodePortOption
        <*> optional stateDirOption
        <*> verbosityOption
        <*> (JormungandrArgs
            <$> genesisBlockOption
            <*> nodeConfigOption
            <*> nodeSecretOption)
    exec (LaunchArgs listen nodePort stateDir verbosity jArgs) = do
        cmdName <- getProgName
        block0H <- runGet getBlockId <$> BL.readFile (_genesisBlock jArgs)
        execLaunch verbosity stateDir
            [ commandJormungandr jArgs
            , commandWalletServe cmdName block0H
            ]
      where
        commandJormungandr (JormungandrArgs block0 nodeConfig nodeSecret) =
            Command "jormungandr" arguments (return ()) Inherit
          where
            arguments = mconcat
              [ [ "--genesis-block", block0 ]
              , [ "--config", nodeConfig ]
              , [ "--secret", nodeSecret ]
              ]

        commandWalletServe cmdName block0H =
            Command cmdName arguments (return ()) Inherit
          where
            arguments = mconcat
                [ [ "serve" ]
                , case listen of
                    ListenOnRandomPort -> ["--random-port"]
                    ListenOnPort port  -> ["--port", showT port]
                , [ "--node-port", showT nodePort ]
                , maybe [] (\d -> ["--database", d </> "wallet.db"]) stateDir
                , verbosityToArgs verbosity
                , [ "--genesis-hash", showT block0H ]
                ]

{-------------------------------------------------------------------------------
                            Command - 'serve'

  cardano-wallet serve
    [--network=STRING]
    [(--port=INT | --random-port)]
    [--node-port=INT]
    [--database=FILE]
    [(--quiet | --verbose )]
    --genesis-hash=STRING
-------------------------------------------------------------------------------}

-- | Arguments for the 'serve' command
data ServeArgs = ServeArgs
    { _listen :: Listen
    , _nodePort :: Port "Node"
    , _database :: Maybe FilePath
    , _verbosity :: Verbosity
    , _block0H :: Hash "Genesis"
    }

-- | cardano-wallet serve
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
        (logCfg, tracer) <- initTracer (verbosityToMinSeverity verbosity) "serve"
        logInfo tracer "Wallet backend server starting..."
        logInfo tracer $ "Running as v" <> T.pack (showVersion version)
        logInfo tracer $ "Node is Jörmungandr on " <> toText (networkVal @n)
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
            let tl = Jormungandr.newTransactionLayer @n block0H
            db <- newDBLayer logCfg tracer
            Wallet.newWalletLayer tracer block0 feePolicy db nl tl

        newNetworkLayer
            :: IO (NetworkLayer t IO, Block Tx, FeePolicy)
        newNetworkLayer = do
            let url = BaseUrl Http "localhost" (getPort nodePort) "/api"
            mgr <- newManager defaultManagerSettings
            let jormungandr = Jormungandr.mkJormungandrLayer mgr url
            let nl = Jormungandr.mkNetworkLayer jormungandr
            waitForConnection nl defaultRetryPolicy
            block0 <- unsafeRunExceptT $ getBlock jormungandr (coerce block0H)
            feePolicy <- unsafeRunExceptT $
                Jormungandr.getInitialFeePolicy jormungandr (coerce block0H)
            return (nl, block0, feePolicy)

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

-- | --node-config=FILE
nodeConfigOption :: Parser FilePath
nodeConfigOption = optionT $ mempty
    <> long "node-config"
    <> metavar "FILE"
    <> help "Path to Jörmungandr's own configuration (.yaml)."

-- | --node-secret=FILE
nodeSecretOption :: Parser FilePath
nodeSecretOption = optionT $ mempty
    <> long "node-secret"
    <> metavar "FILE"
    <> help "Path to BFT leaders' secrets (.yaml)."

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
