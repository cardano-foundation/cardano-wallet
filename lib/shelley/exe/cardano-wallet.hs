{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
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
import Cardano.BM.Plugin
    ( loadPlugin )
import Cardano.BM.Trace
    ( Trace, appendName, logDebug, logError, logInfo, logNotice )
import Cardano.CLI
    ( LogOutput (..)
    , LoggingOptions
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
    , ekgEnabled
    , enableWindowsANSI
    , helperTracing
    , hostPreferenceOption
    , listenOption
    , loggingMinSeverity
    , loggingOptions
    , loggingSeverityOrOffReader
    , loggingTracers
    , poolMetadataSourceOption
    , runCli
    , setupDirectory
    , shutdownHandlerFlag
    , syncToleranceOption
    , tlsOption
    , tokenMetadataSourceOption
    , withLogging
    )
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
    ( HostPreference, Listen (..), TlsConfiguration )
import Cardano.Wallet.Api.Types
    ( ApiStakePool )
import Cardano.Wallet.Logging
    ( trMessage, transformTextTrace )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance )
import Cardano.Wallet.Primitive.Types
    ( PoolMetadataSource (..), Settings (..), TokenMetadataServer (..) )
import Cardano.Wallet.Shelley
    ( TracerSeverities
    , Tracers
    , Tracers' (..)
    , serveWallet
    , setupTracers
    , tracerDescriptions
    , tracerLabels
    )
import Cardano.Wallet.Shelley.BlockchainSource
    ( BlockchainSource (..) )
import Cardano.Wallet.Shelley.Launch
    ( Mode (Light, Normal)
    , NetworkConfiguration (..)
    , modeOption
    , networkConfigurationOption
    , parseGenesisData
    )
import Cardano.Wallet.Version
    ( GitRevision, Version, showFullVersion )
import Control.Applicative
    ( Const (..), optional )
import Control.Exception.Base
    ( AsyncException (..) )
import Control.Monad
    ( void, when )
import Control.Monad.Trans.Except
    ( runExceptT )
import Control.Tracer
    ( contramap )
import Data.Bifunctor
    ( second )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Network.URI
    ( URI )
-- See ADP-1910
import "optparse-applicative" Options.Applicative
    ( CommandFields
    , Mod
    , Parser
    , command
    , helper
    , info
    , internal
    , long
    , metavar
    , option
    , progDesc
    , value
    )
import Ouroboros.Network.Client.Wallet
    ( tunedForMainnetPipeliningStrategy )
import System.Environment
    ( getArgs, getExecutablePath )
import System.Exit
    ( ExitCode (..), exitWith )
import UnliftIO.Exception
    ( catch, withException )

import qualified Cardano.BM.Backend.EKGView as EKG
import qualified Cardano.Wallet.Shelley.Launch.Blockfrost as Blockfrost
import qualified Cardano.Wallet.Version as V
import qualified Data.Text as T
import qualified System.Info as I

{-------------------------------------------------------------------------------
                              Main entry point
-------------------------------------------------------------------------------}

main :: IO ()
main = withUtf8Encoding $ do
    enableWindowsANSI
    runCli $ cli $ mempty
        <> cmdServe
        <> cmdMnemonic
        <> cmdKey
        <> cmdWallet cmdWalletCreate walletClient
        <> cmdAddress addressClient
        <> cmdTransaction transactionClient walletClient
        <> cmdNetwork networkClient
        <> cmdStakePool @ApiStakePool stakePoolClient
        <> cmdVersion

beforeMainLoop :: Trace IO MainLog -> URI -> IO ()
beforeMainLoop tr = logInfo tr . MsgListenAddress

{-------------------------------------------------------------------------------
                            Command - 'serve'
-------------------------------------------------------------------------------}

-- | Arguments for the 'serve' command
data ServeArgs = ServeArgs
    { _hostPreference :: HostPreference
    , _mode :: Mode
    , _listen :: Listen
    , _tlsConfig :: Maybe TlsConfiguration
    , _networkConfiguration :: NetworkConfiguration
    , _database :: Maybe FilePath
    , _syncTolerance :: SyncTolerance
    , _enableShutdownHandler :: Bool
    , _poolMetadataSourceOpt :: Maybe PoolMetadataSource
    , _tokenMetadataSourceOpt :: Maybe TokenMetadataServer
    , _logging :: LoggingOptions TracerSeverities
    } deriving (Show)

cmdServe :: Mod CommandFields (IO ())
cmdServe = command "serve" $ info (helper <*> helper' <*> cmd) $
    progDesc "Serve API that listens for commands/actions."
  where
    helper' = helperTracing tracerDescriptions

    cmd = fmap exec $ ServeArgs
        <$> hostPreferenceOption
        <*> modeOption
        <*> listenOption
        <*> optional tlsOption
        <*> networkConfigurationOption
        <*> optional databaseOption
        <*> syncToleranceOption
        <*> shutdownHandlerFlag
        <*> optional poolMetadataSourceOption
        <*> optional tokenMetadataSourceOption
        <*> loggingOptions tracerSeveritiesOption

    exec :: ServeArgs -> IO ()
    exec args@(ServeArgs
      host
      mode
      listen
      tlsConfig
      networkConfig
      databaseDir
      sTolerance
      enableShutdownHandler
      poolMetadataFetching
      tokenMetadataServerURI
      logOpt) = withTracers logOpt $ \tr tracers -> do
        withShutdownHandlerMaybe tr enableShutdownHandler $ do
            logDebug tr $ MsgServeArgs args

            (discriminant, netParams, vData, block0)
                <- runExceptT (parseGenesisData networkConfig) >>= \case
                        Right x -> pure x
                        Left err -> do
                            logError tr (MsgFailedToParseGenesis $ T.pack err)
                            exitWith $ ExitFailure 33
            whenJust databaseDir $
                setupDirectory (logInfo tr . MsgSetupDatabases)

            blockchainSource <- case mode of
                Normal conn -> pure $ NodeSource conn vData
                Light token -> BlockfrostSource <$> Blockfrost.readToken token
                    `catch` \case
                        Blockfrost.BadTokenFile f -> do
                            logError tr $ MsgBlockfrostTokenFileError f
                            exitWith $ ExitFailure 1
                        Blockfrost.EmptyToken f -> do
                            logError tr $ MsgBlockfrostTokenError f
                            exitWith $ ExitFailure 1
                        Blockfrost.InvalidToken f -> do
                            logError tr $ MsgBlockfrostTokenError f
                            exitWith $ ExitFailure 1

            exitWith =<< serveWallet
                blockchainSource
                netParams
                tunedForMainnetPipeliningStrategy
                discriminant
                []
                tracers
                sTolerance
                databaseDir
                Nothing
                host
                listen
                tlsConfig
                (Settings <$> poolMetadataFetching)
                tokenMetadataServerURI
                block0
                (beforeMainLoop tr)

    whenJust m fn = case m of
       Nothing -> pure ()
       Just a  -> fn a
    withShutdownHandlerMaybe :: Trace IO MainLog -> Bool -> IO () -> IO ()
    withShutdownHandlerMaybe _ False = void
    withShutdownHandlerMaybe tr True = void . withShutdownHandler trShutdown
      where
        trShutdown = trMessage $ contramap (second (fmap MsgShutdownHandler)) tr

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data MainLog
    = MsgCmdLine String [String]
    | MsgVersion Version GitRevision String String
    | MsgSetupStateDir Text
    | MsgSetupDatabases Text
    | MsgServeArgs ServeArgs
    | MsgListenAddress URI
    | MsgSigTerm
    | MsgSigInt
    | MsgShutdownHandler ShutdownHandlerLog
    | MsgFailedToParseGenesis Text
    | MsgBlockfrostTokenFileError FilePath
    | MsgBlockfrostTokenError FilePath
    deriving (Show)

instance ToText MainLog where
    toText = \case
        MsgCmdLine exe args ->
            T.pack $ unwords ("Command line:":exe:args)
        MsgVersion ver rev arch os ->
            "Running as " <> T.pack (showFullVersion ver rev) <> " on " <>
            T.pack arch <> "-" <> T.pack os
        MsgSetupStateDir txt ->
            "Wallet state: " <> txt
        MsgSetupDatabases txt ->
            "Wallet databases: " <> txt
        MsgServeArgs args ->
            T.pack $ show args
        MsgListenAddress url ->
            "Wallet backend server listening on " <> T.pack (show url)
        MsgSigTerm ->
            "Terminated by signal."
        MsgSigInt ->
            "Interrupted by user."
        MsgShutdownHandler msg' ->
            toText msg'
        MsgFailedToParseGenesis hint -> T.unwords
            [ "Failed to parse Byron genesis configuration. You may want to check"
            , "the filepath given via --genesis and make sure it points to a "
            , "valid JSON Byron genesis file. The genesis file must be Byron, not"
            , "Shelley as it used to feed the wallet with the initial blockchain"
            , "parameters."
            , "Here's (perhaps) some helpful hint:", hint
            ]
        MsgBlockfrostTokenFileError tokenFile -> T.unwords
            [ "File"
            , "'" <> T.pack tokenFile <> "'"
            , "specified in the --blockfrost-token-file can't be read."
            ]
        MsgBlockfrostTokenError tokenFile -> T.unwords
            [ "File"
            , "'" <> T.pack tokenFile <> "'"
            , "specified in the --blockfrost-token-file\
            \ argument doesn't contain a valid Blockfrost API token."
            ]

withTracers
    :: LoggingOptions TracerSeverities
    -> (Trace IO MainLog -> Tracers IO -> IO a)
    -> IO a
withTracers logOpt action =
    withLogging [LogToStdStreams (loggingMinSeverity logOpt)] $ \(sb, (cfg, tr)) -> do
        ekgEnabled >>= flip when (EKG.plugin cfg tr sb >>= loadPlugin sb)
        let trMain = appendName "main" (transformTextTrace tr)
        let tracers = setupTracers (loggingTracers logOpt) tr
        logInfo trMain $ MsgVersion V.version V.gitRevision I.arch I.os
        logInfo trMain =<< MsgCmdLine <$> getExecutablePath <*> getArgs
        installSignalHandlers (logNotice trMain MsgSigTerm)
        let logInterrupt UserInterrupt = logNotice trMain MsgSigInt
            logInterrupt _ = pure ()
        action trMain tracers `withException` logInterrupt


{-------------------------------------------------------------------------------
                                 Options
-------------------------------------------------------------------------------}


tracerSeveritiesOption :: Parser TracerSeverities
tracerSeveritiesOption = Tracers
    <$> traceOpt applicationTracer (Just Info)
    <*> traceOpt apiServerTracer (Just Info)
    <*> traceOpt tokenMetadataTracer (Just Info)
    <*> traceOpt walletEngineTracer (Just Info)
    <*> traceOpt walletDbTracer (Just Info)
    <*> traceOpt poolsEngineTracer (Just Info)
    <*> traceOpt poolsDbTracer (Just Info)
    <*> traceOpt ntpClientTracer (Just Info)
    <*> traceOpt networkTracer (Just Info)
  where
    traceOpt field def = fmap Const . option loggingSeverityOrOffReader $ mempty
        <> long ("trace-" <> T.unpack (getConst (field tracerLabels)))
        <> value def
        <> metavar "SEVERITY"
        <> internal
