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
-- Copyright: © 2018-2020 IOHK
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
    , enableWindowsANSI
    , getDataDir
    , hostPreferenceOption
    , listenOption
    , nodePortMaybeOption
    , nodePortOption
    , optionT
    , requireFilePath
    , runCli
    , setupDirectory
    , stateDirOption
    , syncToleranceOption
    , verbosityOption
    , verbosityToMinSeverity
    , withLogging
    )
import Cardano.Launcher
    ( StdStream (..), withUtf8Encoding )
import Cardano.Wallet.Api.Server
    ( HostPreference, Listen (..) )
import Cardano.Wallet.Jormungandr
    ( serveWallet )
import Cardano.Wallet.Jormungandr.Compatibility
    ( localhostBaseUrl )
import Cardano.Wallet.Jormungandr.Network
    ( JormungandrBackend (..)
    , JormungandrConfig (..)
    , JormungandrConnParams (..)
    )
import Cardano.Wallet.Logging
    ( transformTextTrace )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( BlockchainParameters, Hash (..), SyncTolerance )
import Cardano.Wallet.Version
    ( gitRevision, showFullVersion, version )
import Control.Applicative
    ( optional, (<|>) )
import Data.List
    ( isPrefixOf )
import Data.Maybe
    ( fromMaybe )
import Data.Text
    ( Text )
import Network.Socket
    ( SockAddr )
import Options.Applicative
    ( CommandFields
    , Mod
    , Parser
    , argument
    , command
    , footerDoc
    , help
    , helper
    , info
    , long
    , many
    , metavar
    , progDesc
    )
import Options.Applicative.Types
    ( readerAsk, readerError )
import System.Exit
    ( exitWith )
import System.FilePath
    ( (</>) )

import qualified Data.Text as T
import qualified Options.Applicative.Help.Pretty as D

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
main = withUtf8Encoding $ do
    enableWindowsANSI
    dataDir <- getDataDir "jormungandr"
    runCli $ cli $ mempty
        <> cmdLaunch dataDir
        <> cmdServe
        <> cmdMnemonic
        <> cmdWallet @'Testnet
        <> cmdTransaction @'Testnet
        <> cmdAddress @'Testnet
        <> cmdStakePool @'Testnet
        <> cmdNetwork @'Testnet
        <> cmdVersion

beforeMainLoop
    :: Trace IO Text
    -> SockAddr
    -> Port "node"
    -> BlockchainParameters
    -> IO ()
beforeMainLoop tr addr _ _ = do
    logInfo tr $ "Wallet backend server listening on " <> T.pack (show addr)

{-------------------------------------------------------------------------------
                            Command - 'launch'
-------------------------------------------------------------------------------}

-- | Arguments for the 'launch' command
data LaunchArgs = LaunchArgs
    { _hostPreference :: HostPreference
    , _listen :: Listen
    , _nodePort :: Maybe (Port "Node")
    , _stateDir :: Maybe FilePath
    , _syncTolerance :: SyncTolerance
    , _verbosity :: Verbosity
    , _jormungandrArgs :: JormungandrArgs
    }

data JormungandrArgs = JormungandrArgs
    { genesisBlock :: Either (Hash "Genesis") FilePath
    , extraJormungandrArgs :: [String]
    }

cmdLaunch
    :: FilePath
    -> Mod CommandFields (IO ())
cmdLaunch dataDir = command "launch" $ info (helper <*> cmd) $ mempty
    <> progDesc "Launch and monitor a wallet server and its chain producers."
    <> footerDoc (Just $ D.empty
        <> D.text "Examples:"
        <> D.line
        <> D.text "1) Minimal setup, relying on sensible defaults:" <> D.line
        <> D.text "    launch --genesis-block block0.bin" <> D.line
        <> D.line
        <> D.text "2) Launching a full node: " <> D.line
        <> D.text "    launch --genesis-block block0.bin -- --secret secret.yaml" <> D.line
        <> D.line
        <> D.text "3) Bootstrapping from trusted peers*:" <> D.line
        <> D.text "    launch --genesis-block-hash 4c05c5bb -- --config config.yaml" <> D.line
        <> D.line
        <> D.text "(*) assuming 'trusted_peers' is defined in 'config.yaml'"
        <> D.line
        <> D.line
        <> D.text "Please also note that 'launch' will define a 'rest' and" <> D.line
        <> D.text "'storage' configuration for Jörmungandr so in case you" <> D.line
        <> D.text "provide a configuration file as extra arguments, make sure" <> D.line
        <> D.text "not to define any these configuration settings."
       )
  where
    cmd = fmap exec $ LaunchArgs
        <$> hostPreferenceOption
        <*> listenOption
        <*> nodePortMaybeOption
        <*> stateDirOption dataDir
        <*> syncToleranceOption
        <*> verbosityOption
        <*> (JormungandrArgs
            <$> genesisBlockOption
            <*> extraArguments)
    exec (LaunchArgs hostPreference listen nodePort mStateDir sTolerance verbosity jArgs) = do
        withLogging Nothing (verbosityToMinSeverity verbosity) $ \(cfg, tr) -> do
            case genesisBlock jArgs of
                Right block0File -> requireFilePath block0File
                Left _ -> pure ()
            let stateDir = fromMaybe (dataDir </> "testnet") mStateDir
            let databaseDir = stateDir </> "wallets"
            let cp = JormungandrConfig
                    { _stateDir = stateDir
                    , _genesisBlock = genesisBlock jArgs
                    , _restApiPort = fromIntegral . getPort <$> nodePort
                    , _outputStream = Inherit
                    , _extraArgs = extraJormungandrArgs jArgs
                    }
            setupDirectory (logInfo tr) stateDir
            setupDirectory (logInfo tr) databaseDir
            logInfo tr $
                "Running as v" <> T.pack (showFullVersion version gitRevision)
            exitWith =<< serveWallet @'Testnet
                (cfg, transformTextTrace tr)
                sTolerance
                (Just databaseDir)
                hostPreference
                listen
                (Launch cp)
                (beforeMainLoop tr)

{-------------------------------------------------------------------------------
                            Command - 'serve'
-------------------------------------------------------------------------------}

-- | Arguments for the 'serve' command
data ServeArgs = ServeArgs
    { _hostPreference :: HostPreference
    , _listen :: Listen
    , _nodePort :: Port "Node"
    , _database :: Maybe FilePath
    , _syncTolerance :: SyncTolerance
    , _verbosity :: Verbosity
    , _block0H :: Hash "Genesis"
    }

cmdServe
    :: Mod CommandFields (IO ())
cmdServe = command "serve" $ info (helper <*> cmd) $ mempty
    <> progDesc "Serve API that listens for commands/actions."
  where
    cmd = fmap exec $ ServeArgs
        <$> hostPreferenceOption
        <*> listenOption
        <*> nodePortOption
        <*> optional databaseOption
        <*> syncToleranceOption
        <*> verbosityOption
        <*> genesisHashOption
    exec
        :: ServeArgs
        -> IO ()
    exec (ServeArgs hostPreference listen nodePort databaseDir sTolerance verbosity block0H) = do
        let minSeverity = verbosityToMinSeverity verbosity
        withLogging Nothing minSeverity $ \(cfg, tr) -> do
            let baseUrl = localhostBaseUrl $ getPort nodePort
            let cp = JormungandrConnParams block0H baseUrl
            whenJust databaseDir $ setupDirectory (logInfo tr)
            logInfo tr $
                "Running as v" <> T.pack (showFullVersion version gitRevision)
            exitWith =<< serveWallet @'Testnet
                (cfg, transformTextTrace tr)
                sTolerance
                databaseDir
                hostPreference
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

-- | -- [ARGUMENTS...]
extraArguments :: Parser [String]
extraArguments = many $ argument jmArg $ mempty
    <> metavar "[-- ARGUMENTS...]"
    <> help "Extra arguments to be passed to Jörmungandr."
  where
    jmArg = do
        arg <- readerAsk
        case validate arg of
            Just err -> readerError err
            Nothing -> pure arg
    validate arg
        | "--genesis-block" `isPrefixOf` arg = Just $
            "The " <> arg <> " option must be placed before the --"
        | "--rest-listen" `isPrefixOf` arg = Just $
            suggestion "--rest-listen"
        | "--storage" `isPrefixOf` arg = Just $
            suggestion "--storage"
        | otherwise = Nothing
    suggestion arg = "The " <> arg <> " option is used by the 'launch'"
        <> " command.\nIf you need to use this option,"
        <> " run Jörmungandr separately and use 'serve'."
