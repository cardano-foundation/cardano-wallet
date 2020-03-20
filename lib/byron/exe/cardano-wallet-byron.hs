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

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Trace
    ( Trace, appendName, logDebug, logError, logInfo, logNotice )
import Cardano.Chain.Genesis
    ( GenesisData (..), readGenesisData )
import Cardano.CLI
    ( LoggingOptions (..)
    , cli
    , cmdKey
    , cmdMnemonic
    , cmdNetwork
    , cmdVersion
    , databaseOption
    , enableWindowsANSI
    , helperTracing
    , hostPreferenceOption
    , listenOption
    , loggingOptions
    , loggingSeverityOrOffReader
    , optionT
    , runCli
    , setupDirectory
    , shutdownHandlerFlag
    , syncToleranceOption
    , withLogging
    )
import Cardano.Startup
    ( ShutdownHandlerLog
    , installSignalHandlers
    , withShutdownHandler
    , withUtf8Encoding
    )
import Cardano.Wallet.Api.Server
    ( HostPreference, Listen (..) )
import Cardano.Wallet.Byron
    ( SomeNetworkDiscriminant (..)
    , TracerSeverities
    , Tracers
    , Tracers' (..)
    , serveWallet
    , setupTracers
    , tracerDescriptions
    , tracerLabels
    )
import Cardano.Wallet.Byron.Compatibility
    ( KnownNetwork (..)
    , NodeVersionData
    , emptyGenesis
    , fromGenesisData
    , fromProtocolMagicId
    , mainnetVersionData
    , testnetVersionData
    )
import Cardano.Wallet.Byron.Transaction
    ( fromGenesisTxOut )
import Cardano.Wallet.Logging
    ( trMessage, transformTextTrace )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( ProtocolMagic (..), SyncTolerance )
import Cardano.Wallet.Version
    ( GitRevision, Version, gitRevision, showFullVersion, version )
import Control.Applicative
    ( Const (..), optional, (<|>) )
import Control.Monad
    ( void )
import Control.Monad.Trans.Except
    ( runExceptT )
import Control.Tracer
    ( contramap )
import Data.Function
    ( (&) )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import GHC.TypeLits
    ( SomeNat (..), someNatVal )
import Network.Socket
    ( SockAddr )
import Options.Applicative
    ( CommandFields
    , Mod
    , Parser
    , command
    , flag'
    , help
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
        <> cmdNetwork
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
    , _nodeSocket :: FilePath
    , _networkConfiguration :: NetworkConfiguration
    , _database :: Maybe FilePath
    , _syncTolerance :: SyncTolerance
    , _enableShutdownHandler :: Bool
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
        <*> nodeSocketOption
        <*> networkConfigurationOption
        <*> optional databaseOption
        <*> syncToleranceOption
        <*> shutdownHandlerFlag
        <*> loggingOptions tracerSeveritiesOption
    exec
        :: ServeArgs
        -> IO ()
    exec args@(ServeArgs
      host
      listen
      nodeSocket
      networkConfig
      databaseDir
      sTolerance
      enableShutdownHandler
      logOpt) = do
        withTracers logOpt $ \tr tracers -> do
            installSignalHandlers (logNotice tr MsgSigTerm)
            withShutdownHandlerMaybe tr enableShutdownHandler $ do
                logDebug tr $ MsgServeArgs args

                (discriminant, bp, vData, block0) <-
                    parseGenesisData tr networkConfig

                whenJust databaseDir $ setupDirectory (logInfo tr . MsgSetupDatabases)
                exitWith =<< serveWallet
                    discriminant
                    tracers
                    sTolerance
                    databaseDir
                    host
                    listen
                    nodeSocket
                    block0
                    (bp, vData)
                    (beforeMainLoop tr)

    whenJust m fn = case m of
       Nothing -> pure ()
       Just a  -> fn a
    withShutdownHandlerMaybe :: Trace IO MainLog -> Bool -> IO () -> IO ()
    withShutdownHandlerMaybe _ False = void
    withShutdownHandlerMaybe tr True = void . withShutdownHandler trShutdown
      where
        trShutdown = trMessage $ contramap (\(n, x) -> (n, fmap MsgShutdownHandler x)) tr


    parseGenesisData tr = \case
        MainnetConfig (discriminant, vData) -> pure
            ( discriminant
            , blockchainParameters @'Mainnet
            , vData
            , emptyGenesis (blockchainParameters @'Mainnet)
            )
        TestnetConfig genesisFile -> do
            (genesisData, genesisHash) <-
                runExceptT (readGenesisData genesisFile) >>= \case
                    Right a -> pure a
                    Left err -> do
                        logError tr $ MsgFailedToParseGenesis $ T.pack $ show err
                        exitWith $ ExitFailure 33

            let (discriminant, vData) = genesisData
                    & gdProtocolMagicId
                    & fromProtocolMagicId
                    & someTestnetDiscriminant

            let (bp, outs) = fromGenesisData (genesisData, genesisHash)
            pure
                ( discriminant
                , bp
                , vData
                , fromGenesisTxOut bp outs
                )

{-------------------------------------------------------------------------------
                                 Options
-------------------------------------------------------------------------------}

data NetworkConfiguration where
    MainnetConfig
        :: (SomeNetworkDiscriminant, NodeVersionData)
        -> NetworkConfiguration

    TestnetConfig
        :: FilePath
        -> NetworkConfiguration

-- | Hand-written as there's no Show instance for 'NodeVersionData'
instance Show NetworkConfiguration where
    show = \case
        MainnetConfig{} ->
            "MainnetConfig"
        TestnetConfig genesisFile ->
            "TestnetConfig " <> show genesisFile

-- | --node-socket=FILE
nodeSocketOption :: Parser FilePath
nodeSocketOption = optionT $ mempty
    <> long "node-socket"
    <> metavar "FILE"
    <> help "Path to the node's domain socket."

-- | --mainnet | (--testnet=FILE)
networkConfigurationOption :: Parser NetworkConfiguration
networkConfigurationOption =
    mainnetFlag <|> (TestnetConfig <$> testnetOption)
  where
    -- --mainnet
    mainnetFlag = flag'
        (MainnetConfig (SomeNetworkDiscriminant $ Proxy @'Mainnet, mainnetVersionData))
        (long "mainnet")

    -- | --testnet=FILE
    testnetOption
        :: Parser FilePath
    testnetOption = optionT $ mempty
        <> long "testnet"
        <> metavar "FILE"
        <> help "Path to the genesis .json file."

someTestnetDiscriminant
    :: ProtocolMagic
    -> (SomeNetworkDiscriminant, NodeVersionData)
someTestnetDiscriminant pm@(ProtocolMagic n) =
    case someNatVal (fromIntegral n) of
        Just (SomeNat proxy) ->
            ( SomeNetworkDiscriminant $ mapProxy proxy
            , testnetVersionData pm
            )
        _ -> error "networkDiscriminantFlag: failed to convert \
            \ProtocolMagic to SomeNat."
  where
    mapProxy :: forall a. Proxy a -> Proxy ('Testnet a)
    mapProxy _proxy = Proxy @('Testnet a)

tracerSeveritiesOption :: Parser TracerSeverities
tracerSeveritiesOption = Tracers
    <$> traceOpt applicationTracer (Just Info)
    <*> traceOpt apiServerTracer (Just Info)
    <*> traceOpt walletEngineTracer (Just Info)
    <*> traceOpt walletDbTracer (Just Info)
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
