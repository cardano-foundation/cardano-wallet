{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
    ( Trace, appendName, logDebug, logError, logInfo, logNotice )
import Cardano.CLI
    ( LoggingOptions
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
    , helperTracing
    , hostPreferenceOption
    , listenOption
    , loggingMinSeverity
    , loggingOptions
    , loggingSeverityOrOffReader
    , loggingTracers
    , poolMetadataProxyOption
    , runCli
    , setupDirectory
    , shutdownHandlerFlag
    , syncToleranceOption
    , tlsOption
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
import Cardano.Wallet.Shelley
    ( TracerSeverities
    , Tracers
    , Tracers' (..)
    , serveWallet
    , setupTracers
    , tracerDescriptions
    , tracerLabels
    )
import Cardano.Wallet.Shelley.Launch
    ( NetworkConfiguration (..)
    , networkConfigurationOption
    , nodeSocketOption
    , parseGenesisData
    )
import Cardano.Wallet.Version
    ( GitRevision, Version, gitRevision, showFullVersion, version )
import Control.Applicative
    ( Const (..), optional )
import Control.Monad
    ( void )
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
import Network.Socket
    ( SockAddr )
import Network.URI
    ( URI )
import Options.Applicative
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
import System.Environment
    ( getArgs, getExecutablePath )
import System.Exit
    ( ExitCode (..), exitWith )

import qualified Data.Text as T

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

beforeMainLoop
    :: Trace IO MainLog
    -> SockAddr
    -> IO ()
beforeMainLoop tr =
    logInfo tr . MsgListenAddress

{-------------------------------------------------------------------------------
                            Command - 'serve'
-------------------------------------------------------------------------------}

-- | Arguments for the 'serve' command
data ServeArgs = ServeArgs
    { _hostPreference :: HostPreference
    , _listen :: Listen
    , _tlsConfig :: Maybe TlsConfiguration
    , _nodeSocket :: FilePath
    , _networkConfiguration :: NetworkConfiguration
    , _database :: Maybe FilePath
    , _syncTolerance :: SyncTolerance
    , _enableShutdownHandler :: Bool
    , _poolMetadataProxy :: Maybe URI
    , _logging :: LoggingOptions TracerSeverities
    } deriving (Show)

cmdServe
    :: Mod CommandFields (IO ())
cmdServe = command "serve" $ info (helper <*> helper' <*> cmd) $ mempty
    <> progDesc "Serve API that listens for commands/actions."
  where
    helper' = helperTracing tracerDescriptions

    cmd = fmap exec $ ServeArgs
        <$> hostPreferenceOption
        <*> listenOption
        <*> optional tlsOption
        <*> nodeSocketOption
        <*> networkConfigurationOption
        <*> optional databaseOption
        <*> syncToleranceOption
        <*> shutdownHandlerFlag
        <*> optional poolMetadataProxyOption
        <*> loggingOptions tracerSeveritiesOption
    exec
        :: ServeArgs -> IO ()
    exec args@(ServeArgs
      host
      listen
      tlsConfig
      nodeSocket
      networkConfig
      databaseDir
      sTolerance
      enableShutdownHandler
      poolMetadataProxy
      logOpt) = do
        withTracers logOpt $ \tr tracers -> do
            installSignalHandlers (logNotice tr MsgSigTerm)
            withShutdownHandlerMaybe tr enableShutdownHandler $ do
                logDebug tr $ MsgServeArgs args

                (discriminant, gp, vData, block0)
                    <- runExceptT (parseGenesisData networkConfig) >>= \case
                            Right x -> pure x
                            Left err -> do
                                logError tr (MsgFailedToParseGenesis $ T.pack err)
                                exitWith $ ExitFailure 33

                whenJust databaseDir $ setupDirectory (logInfo tr . MsgSetupDatabases)
                exitWith =<< serveWallet
                    discriminant
                    tracers
                    sTolerance
                    databaseDir
                    host
                    listen
                    tlsConfig
                    poolMetadataProxy
                    nodeSocket
                    block0
                    (gp, vData)
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

-- FIXME: reduce duplication. See 'cardano-wallet-jormungandr.hs'

data MainLog
    = MsgCmdLine String [String]
    | MsgVersion Version GitRevision
    | MsgSetupStateDir Text
    | MsgSetupDatabases Text
    | MsgServeArgs ServeArgs
    | MsgListenAddress SockAddr
    | MsgSigTerm
    | MsgShutdownHandler ShutdownHandlerLog
    | MsgFailedToParseGenesis Text
    deriving (Show)

instance ToText MainLog where
    toText msg = case msg of
        MsgCmdLine exe args ->
            T.pack $ unwords ("Command line:":exe:args)
        MsgVersion ver rev ->
            "Running as v" <> T.pack (showFullVersion ver rev)
        MsgSetupStateDir txt ->
            "Wallet state: " <> txt
        MsgSetupDatabases txt ->
            "Wallet databases: " <> txt
        MsgServeArgs args ->
            T.pack $ show args
        MsgListenAddress addr ->
            "Wallet backend server listening on " <> T.pack (show addr)
        MsgSigTerm ->
            "Terminated by signal."
        MsgShutdownHandler msg' ->
            toText msg'
        MsgFailedToParseGenesis hint -> T.unwords
            [ "Failed to parse genesis configuration. You may want to check the"
            , "filepath given via --genesis and make sure it points to a valid"
            , "JSON genesis file. Here's (perhaps) some helpful hint:", hint
            ]

withTracers
    :: LoggingOptions TracerSeverities
    -> (Trace IO MainLog -> Tracers IO -> IO a)
    -> IO a
withTracers logOpt action =
    withLogging Nothing (loggingMinSeverity logOpt) $ \(_, tr) -> do
        let trMain = appendName "main" (transformTextTrace tr)
        let tracers = setupTracers (loggingTracers logOpt) tr
        logInfo trMain $ MsgVersion version gitRevision
        logInfo trMain =<< MsgCmdLine <$> getExecutablePath <*> getArgs
        action trMain tracers


{-------------------------------------------------------------------------------
                                 Options
-------------------------------------------------------------------------------}


tracerSeveritiesOption :: Parser TracerSeverities
tracerSeveritiesOption = Tracers
    <$> traceOpt applicationTracer (Just Info)
    <*> traceOpt apiServerTracer (Just Info)
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
