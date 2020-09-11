{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Trace
    ( Trace, appendName, logInfo, logNotice )
import Cardano.CLI
    ( LogOutput (..)
    , LoggingOptions (..)
    , Port (..)
    , cli
    , cmdAddress
    , cmdKey
    , cmdMnemonic
    , cmdNetwork
    , cmdStakePool
    , cmdTransaction
    , cmdVersion
    , cmdWallet
    , cmdWalletCreate
    , databaseOption
    , enableWindowsANSI
    , getDataDir
    , helperTracing
    , hostPreferenceOption
    , listenOption
    , loggingOptions
    , loggingSeverityOrOffReader
    , nodePortMaybeOption
    , nodePortOption
    , optionT
    , requireFilePath
    , runCli
    , setupDirectory
    , shutdownHandlerFlag
    , stateDirOption
    , syncToleranceOption
    , withLogging
    )
import Cardano.Launcher
    ( StdStream (..) )
import Cardano.Pool.Jormungandr.Metadata
    ( ApiStakePool )
import Cardano.Startup
    ( ShutdownHandlerLog
    , installSignalHandlers
    , withShutdownHandler
    , withUtf8Encoding
    )
import Cardano.Wallet.Api.Client
    ( addressClient
    , networkClient
    , stakePoolClient
    , transactionClient
    , walletClient
    )
import Cardano.Wallet.Api.Server
    ( HostPreference, Listen (..) )
import Cardano.Wallet.Jormungandr
    ( TracerSeverities
    , Tracers
    , Tracers' (..)
    , serveWallet
    , setupTracers
    , tracerDescriptions
    , tracerLabels
    )
import Cardano.Wallet.Jormungandr.Compatibility
    ( localhostBaseUrl )
import Cardano.Wallet.Jormungandr.Network
    ( JormungandrBackend (..)
    , JormungandrConfig (..)
    , JormungandrConnParams (..)
    )
import Cardano.Wallet.Logging
    ( trMessage, transformTextTrace )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance )
import Cardano.Wallet.Primitive.Types
    ( Hash (..), NetworkParameters )
import Cardano.Wallet.Version
    ( GitRevision, Version, gitRevision, showFullVersion, version )
import Control.Applicative
    ( Const (..), optional, (<|>) )
import Control.Arrow
    ( second )
import Control.Monad
    ( void )
import Control.Tracer
    ( contramap )
import Data.List
    ( isPrefixOf )
import Data.Maybe
    ( fromMaybe )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
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
    , internal
    , long
    , many
    , metavar
    , option
    , progDesc
    , value
    )
import Options.Applicative.Types
    ( readerAsk, readerError )
import System.Environment
    ( getArgs, getExecutablePath )
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
        <> cmdWallet cmdWalletCreate walletClient
        <> cmdTransaction transactionClient walletClient
        <> cmdAddress addressClient
        <> cmdStakePool @ApiStakePool stakePoolClient
        <> cmdNetwork networkClient
        <> cmdVersion
        <> cmdKey

beforeMainLoop
    :: Trace IO MainLog
    -> SockAddr
    -> Port "node"
    -> NetworkParameters
    -> IO ()
beforeMainLoop tr addr _ _ = logInfo tr $ MsgListenAddress addr

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
    , _logging :: LoggingOptions TracerSeverities
    , _jormungandrArgs :: JormungandrArgs
    } deriving (Show, Eq)

data JormungandrArgs = JormungandrArgs
    { genesisBlock :: Either (Hash "Genesis") FilePath
    , extraJormungandrArgs :: [String]
    } deriving (Show, Eq)

cmdLaunch
    :: FilePath
    -> Mod CommandFields (IO ())
cmdLaunch dataDir = command "launch" $ info (helper <*> helper' <*> cmd) $ mempty
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
    helper' = helperTracing tracerDescriptions

    cmd = fmap exec $ LaunchArgs
        <$> hostPreferenceOption
        <*> listenOption
        <*> nodePortMaybeOption
        <*> stateDirOption dataDir
        <*> syncToleranceOption
        <*> loggingOptions tracerSeveritiesOption
        <*> (JormungandrArgs
            <$> genesisBlockOption
            <*> extraArguments)
    exec args@(LaunchArgs hostPreference listen nodePort mStateDir sTolerance logOpt jArgs) = do
        withTracers logOpt $ \tr tracers -> do
            installSignalHandlers (logNotice tr MsgSigTerm)
            logInfo tr $ MsgLaunchArgs args
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
            setupDirectory (logInfo tr . MsgSetupStateDir) stateDir
            setupDirectory (logInfo tr . MsgSetupDatabases) databaseDir
            exitWith =<< serveWallet @('Testnet 0)
                tracers
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
    , _block0H :: Hash "Genesis"
    , _enableShutdownHandler :: Bool
    , _logging :: LoggingOptions TracerSeverities
    } deriving (Show, Eq)

cmdServe
    :: Mod CommandFields (IO ())
cmdServe = command "serve" $ info (helper <*> helper' <*> cmd) $ mempty
    <> progDesc "Serve API that listens for commands/actions."
  where
    helper' = helperTracing tracerDescriptions

    cmd = fmap exec $ ServeArgs
        <$> hostPreferenceOption
        <*> listenOption
        <*> nodePortOption
        <*> optional databaseOption
        <*> syncToleranceOption
        <*> genesisHashOption
        <*> shutdownHandlerFlag
        <*> loggingOptions tracerSeveritiesOption
    exec
        :: ServeArgs
        -> IO ()
    exec args@(ServeArgs hostPreference listen nodePort databaseDir sTolerance block0H enableShutdownHandler logOpt) = do
        withTracers logOpt $ \tr tracers -> do
            installSignalHandlers (logNotice tr MsgSigTerm)
            withShutdownHandlerMaybe tr enableShutdownHandler $ do
                logInfo tr $ MsgServeArgs args
                let baseUrl = localhostBaseUrl $ getPort nodePort
                let cp = JormungandrConnParams block0H baseUrl
                whenJust databaseDir $ setupDirectory (logInfo tr . MsgSetupDatabases)
                exitWith =<< serveWallet @('Testnet 0)
                    tracers
                    sTolerance
                    databaseDir
                    hostPreference
                    listen
                    (UseRunning cp)
                    (beforeMainLoop tr)

    whenJust m fn = case m of
       Nothing -> pure ()
       Just a  -> fn a

    withShutdownHandlerMaybe :: Trace IO MainLog -> Bool -> IO () -> IO ()
    withShutdownHandlerMaybe _ False = void
    withShutdownHandlerMaybe tr True = void . withShutdownHandler trShutdown
      where
        trShutdown = trMessage
            $ contramap (second $ fmap MsgShutdownHandler) tr

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

tracerSeveritiesOption :: Parser TracerSeverities
tracerSeveritiesOption = Tracers
    <$> traceOpt applicationTracer (Just Info)
    <*> traceOpt apiServerTracer (Just Info)
    <*> traceOpt walletEngineTracer (Just Info)
    <*> traceOpt walletDbTracer (Just Info)
    <*> traceOpt stakePoolEngineTracer (Just Info)
    <*> traceOpt stakePoolDbTracer (Just Info)
    <*> traceOpt networkTracer (Just Info)
    <*> traceOpt ntpClientTracer (Just Info)
  where
    traceOpt field def = fmap Const . option loggingSeverityOrOffReader $ mempty
        <> long ("trace-" <> T.unpack (getConst (field tracerLabels)))
        <> value def
        <> metavar "SEVERITY"
        <> internal

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data MainLog
    = MsgCmdLine String [String]
    | MsgVersion Version GitRevision
    | MsgSetupStateDir Text
    | MsgSetupDatabases Text
    | MsgLaunchArgs LaunchArgs
    | MsgServeArgs ServeArgs
    | MsgListenAddress SockAddr
    | MsgSigTerm
    | MsgShutdownHandler ShutdownHandlerLog
    deriving (Show, Eq)

instance ToText MainLog where
    toText msg = case msg of
        MsgCmdLine exe args ->
            T.pack $ unwords ("Command line:":exe:args)
        MsgVersion ver rev ->
            "Running as v" <> T.pack (showFullVersion ver rev)
        MsgSetupStateDir txt -> "Wallet state: " <> txt
        MsgSetupDatabases txt -> "Wallet databases: " <> txt
        MsgLaunchArgs args -> T.pack $ show args
        MsgServeArgs args -> T.pack $ show args
        MsgListenAddress addr ->
            "Wallet backend server listening on " <> T.pack (show addr)
        MsgSigTerm -> "Terminated by signal."
        MsgShutdownHandler msg' -> toText msg'

withTracers
    :: LoggingOptions TracerSeverities
    -> (Trace IO MainLog -> Tracers IO -> IO a)
    -> IO a
withTracers logOpt action =
    withLogging [LogToStdout $ loggingMinSeverity logOpt] $ \(_, tr) -> do
        let trMain = appendName "main" (transformTextTrace tr)
        let tracers = setupTracers (loggingTracers logOpt) tr
        logInfo trMain $ MsgVersion version gitRevision
        logInfo trMain =<< MsgCmdLine <$> getExecutablePath <*> getArgs
        action trMain tracers
