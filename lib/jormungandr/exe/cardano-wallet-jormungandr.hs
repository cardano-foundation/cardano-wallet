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
    , nodePortMaybeOption
    , nodePortOption
    , optionT
    , requireFilePath
    , runCli
    , setupStateDir
    , stateDirOption
    , verbosityOption
    , verbosityToMinSeverity
    )
import Cardano.Launcher
    ( StdStream (..) )
import Cardano.Wallet.Api.Server
    ( Listen (..) )
import Cardano.Wallet.Jormungandr
    ( serveWallet )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr, localhostBaseUrl )
import Cardano.Wallet.Jormungandr.Environment
    ( KnownNetwork (..), Network (..) )
import Cardano.Wallet.Jormungandr.Network
    ( JormungandrBackend (..)
    , JormungandrConfig (..)
    , JormungandrConnParams (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( KeyToAddress )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( SeqKey )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState )
import Cardano.Wallet.Primitive.Types
    ( Hash (..) )
import Cardano.Wallet.Version
    ( showVersion, version )
import Control.Applicative
    ( optional )
import Data.Maybe
    ( fromMaybe )
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
import System.Exit
    ( exitWith )
import System.FilePath
    ( (</>) )

import qualified Data.Text as T

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
    , _nodePort :: Maybe (Port "Node")
    , _stateDir :: Maybe FilePath
    , _verbosity :: Verbosity
    , _jormungandrArgs :: JormungandrArgs
    }

data JormungandrArgs = JormungandrArgs
    { genesisBlock :: FilePath
    , secretFile :: FilePath
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
        <*> nodePortMaybeOption
        <*> stateDirOption dataDir
        <*> verbosityOption
        <*> (JormungandrArgs
            <$> genesisBlockOption
            <*> secretFileOption)
    exec (LaunchArgs listen nodePort mStateDir verbosity jArgs) = do
        let minSeverity = verbosityToMinSeverity verbosity
        (cfg, sb, tr) <- initTracer minSeverity "launch"

        requireFilePath (genesisBlock jArgs)
        requireFilePath (secretFile jArgs)

        let stateDir = fromMaybe (dataDir </> "testnet") mStateDir
        let dbFile = stateDir </> "wallet.db"
        let cp = JormungandrConfig
                { _stateDir = stateDir
                , _genesisBlock = genesisBlock jArgs
                , _secretFile = secretFile jArgs
                , _restApiPort = fromIntegral . getPort <$> nodePort
                , _minSeverity = minSeverity
                , _outputStream = Inherit
                }

        logInfo tr $ "Running as v" <> T.pack (showVersion version)
        setupStateDir (logInfo tr) stateDir
        exitWith =<< serveWallet (cfg, sb, tr) (Just dbFile) listen (Launch cp)

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
    <> progDesc "Serve API that listens for commands/actions."
  where
    cmd = fmap exec $ ServeArgs
        <$> listenOption
        <*> nodePortOption
        <*> optional databaseOption
        <*> verbosityOption
        <*> genesisHashOption
    exec
        :: forall t k n s. (n ~ 'Testnet, t ~ Jormungandr n, s ~ SeqState t, k ~ SeqKey)
        => (KeyToAddress t k, KnownNetwork n)
        => ServeArgs
        -> IO ()
    exec (ServeArgs listen nodePort dbFile verbosity block0H) = do
        (cfg, sb, tr) <- initTracer (verbosityToMinSeverity verbosity) "serve"
        logInfo tr $ "Running as v" <> T.pack (showVersion version)
        let baseUrl = localhostBaseUrl $ getPort nodePort
        let cp = JormungandrConnParams block0H baseUrl
        exitWith =<< serveWallet (cfg, sb, tr) dbFile listen (UseRunning cp)

{-------------------------------------------------------------------------------
                                 Options
-------------------------------------------------------------------------------}

-- | --secret=FILE
secretFileOption :: Parser FilePath
secretFileOption = optionT $ mempty
    <> long "secret"
    <> metavar "FILE"
    <> help "Path to secrets (.yaml/.json)."

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
