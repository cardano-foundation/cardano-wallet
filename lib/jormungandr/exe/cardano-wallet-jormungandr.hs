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

import Prelude

import Cardano.BM.Trace
    ( Trace, logInfo )
import Cardano.CLI
    ( Port (..)
    , Verbosity (..)
    , cli
    , cmdAddress
    , cmdMnemonic
    , cmdNetwork
    , cmdStakePool
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
    , setUtf8Encoding
    , setupDirectory
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
    ( Jormungandr, Network (Testnet), localhostBaseUrl )
import Cardano.Wallet.Jormungandr.Network
    ( JormungandrBackend (..)
    , JormungandrConfig (..)
    , JormungandrConnParams (..)
    )
import Cardano.Wallet.Primitive.Model
    ( BlockchainParameters )
import Cardano.Wallet.Primitive.Types
    ( Hash (..) )
import Cardano.Wallet.Version
    ( showVersion, version )
import Control.Applicative
    ( optional, (<|>) )
import Data.List
    ( isPrefixOf )
import Data.Maybe
    ( fromMaybe )
import Data.Text
    ( Text )
import Data.Text.Class
    ( toText )
import Options.Applicative
    ( CommandFields
    , Mod
    , Parser
    , argument
    , command
    , footer
    , help
    , helper
    , info
    , long
    , many
    , metavar
    , option
    , progDesc
    , str
    )
import Options.Applicative.Types
    ( readerAsk, readerError )
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
    setUtf8Encoding
    dataDir <- getDataDir "jormungandr"
    runCli $ cli $ mempty
        <> cmdLaunch dataDir
        <> cmdServe
        <> cmdMnemonic
        <> cmdWallet @(Jormungandr 'Testnet)
        <> cmdTransaction @(Jormungandr 'Testnet)
        <> cmdAddress @(Jormungandr 'Testnet)
        <> cmdStakePool @(Jormungandr 'Testnet)
        <> cmdNetwork @(Jormungandr 'Testnet)
        <> cmdVersion

beforeMainLoop
    :: Trace IO Text
    -> Port "wallet"
    -> Port "node"
    -> BlockchainParameters
    -> IO ()
beforeMainLoop tr port _ _ = do
    logInfo tr $ "Wallet backend server listening on: " <> toText port

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
    { _genesisBlock :: Either (Hash "Genesis") FilePath
    , _configFile :: Maybe FilePath
    , _extraJormungandrArgs :: [String]
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
            <*> configFileOption
            <*> extraArguments)
    exec (LaunchArgs listen nodePort mStateDir verbosity jArgs) = do
        let minSeverity_ = verbosityToMinSeverity verbosity
        (cfg, sb, tr) <- initTracer minSeverity_ "launch"
        case _genesisBlock jArgs of
            Right block0File -> requireFilePath block0File
            Left _ -> pure ()
        let stateDir_ = fromMaybe (dataDir </> "testnet") mStateDir
        let databaseDir = stateDir_ </> "wallets"
        let cp = JormungandrConfig
                { stateDir = stateDir_
                , genesisBlock = _genesisBlock jArgs
                , restApiPort = fromIntegral . getPort <$> nodePort
                , minSeverity = minSeverity_
                , outputStream = Inherit
                , configFile = _configFile jArgs
                , extraArgs = _extraJormungandrArgs jArgs
                }
        setupDirectory (logInfo tr) stateDir_
        setupDirectory (logInfo tr) databaseDir
        logInfo tr $ "Running as v" <> T.pack (showVersion version)
        exitWith =<< serveWallet
            (cfg, sb, tr)
            (Just databaseDir)
            listen
            (Launch cp)
            (beforeMainLoop tr)

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
        :: ServeArgs
        -> IO ()
    exec (ServeArgs listen nodePort databaseDir verbosity block0H) = do
        (cfg, sb, tr) <- initTracer (verbosityToMinSeverity verbosity) "serve"
        let baseUrl = localhostBaseUrl $ getPort nodePort
        let cp = JormungandrConnParams block0H baseUrl
        whenJust databaseDir $ setupDirectory (logInfo tr)
        logInfo tr $ "Running as v" <> T.pack (showVersion version)
        exitWith =<< serveWallet
            (cfg, sb, tr)
            databaseDir
            listen
            (UseRunning cp)
            (beforeMainLoop tr)

    whenJust m fn = case m of
       Nothing -> pure ()
       Just a  -> fn a

{-------------------------------------------------------------------------------
                                 Options
-------------------------------------------------------------------------------}

genesisBlockOption :: Parser (Either (Hash "Genesis") FilePath)
genesisBlockOption =
    fmap Left genesisHashOption <|>
    fmap Right genesisBlockFileOption

-- | --genesis-block=FILE
genesisBlockFileOption :: Parser FilePath
genesisBlockFileOption = optionT $ mempty
    <> long "genesis-block"
    <> metavar "FILE"
    <> help "Path to the genesis block in binary format."

-- | --genesis-block-hash=STRING
genesisHashOption :: Parser (Hash "Genesis")
genesisHashOption = optionT $ mempty
    <> long "genesis-block-hash"
    <> metavar "STRING"
    <> help "Blake2b_256 hash of the genesis block, in base 16."

-- | [--config=FILE.YAML]
configFileOption :: Parser (Maybe FilePath)
configFileOption = optional $ option str $ mempty
    <> long "config"
    <> metavar "FILE.YAML"
    <> help "Config file for jormungandr (note that this will be copied to a temporary location)"

-- | -- [ARGUMENTS...]
extraArguments :: Parser [String]
extraArguments = many $ argument jmArg $ mempty
    <> metavar "[-- ARGUMENTS...]"
    <> help "Extra arguments to be passed to jormungandr."
  where
    jmArg = do
        arg <- readerAsk
        if "--config" `isPrefixOf` arg
            then readerError "The --config option must be placed before the --"
            else pure arg
