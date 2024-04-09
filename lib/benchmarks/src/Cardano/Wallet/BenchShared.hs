{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Restore benchmark CLI handling and setup functions which are shared between
-- backends.

module Cardano.Wallet.BenchShared
    ( -- * CLI Parser
      RestoreBenchArgs (..)
    , getRestoreBenchArgs
    , argsNetworkDir

    -- * Main function
    , execBenchWithNode

    , initBenchmarkLogging

    -- * Benchmark runner
    , runBenchmarks
    , bench
    , Time
    , withTempSqliteFile
    ) where

import Prelude

import Cardano.BM.Configuration.Static
    ( defaultConfigStdout
    )
import Cardano.BM.Data.Severity
    ( Severity (..)
    )
import Cardano.BM.Extra
    ( trMessageText
    )
import Cardano.BM.Setup
    ( setupTrace_
    )
import Cardano.BM.Trace
    ( Trace
    , nullTracer
    )
import Cardano.Launcher.Node
    ( CardanoNodeConfig (..)
    , CardanoNodeConn
    , NodePort (..)
    , cardanoNodeConn
    , withCardanoNode
    )
import Cardano.Startup
    ( installSignalHandlers
    )
import Cardano.Wallet.Network.Ports
    ( getRandomPort
    )
import Control.DeepSeq
    ( NFData
    , rnf
    )
import Control.Monad
    ( forM
    )
import Criterion.Measurement
    ( getTime
    , initializeTime
    , secs
    )
import Data.Aeson
    ( ToJSON (..)
    )
import Data.Functor
    ( (<&>)
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText (..)
    )
import Fmt
    ( Buildable (..)
    , nameF
    , pretty
    )
import GHC.Generics
    ( Generic
    )
-- See ADP-1910
import "optparse-applicative" Options.Applicative
    ( HasValue
    , Mod
    , Parser
    , eitherReader
    , execParser
    , help
    , helper
    , info
    , long
    , metavar
    , option
    , optional
    , short
    , showDefaultWith
    , strArgument
    , strOption
    , switch
    , value
    )
import Say
    ( sayErr
    )
import System.Directory
    ( createDirectoryIfMissing
    )
import System.Environment
    ( lookupEnv
    )
import System.Exit
    ( ExitCode (..)
    , die
    )
import System.FilePath
    ( (</>)
    )
import Test.Utils.Startup
    ( withNoBuffering
    )
import UnliftIO.Concurrent
    ( threadDelay
    )
import UnliftIO.Exception
    ( evaluate
    , try
    )
import UnliftIO.Temporary
    ( withSystemTempDirectory
    )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.BM.Data.BackendKind as CM
import Cardano.Launcher
    ( ProcessHasExited
    )

{-------------------------------------------------------------------------------
               CLI option handling and cardano-node configuration
-------------------------------------------------------------------------------}

execBenchWithNode
    :: (RestoreBenchArgs -> cfg)
    -- ^ Get backend-specific network configuration from args
    -> (Trace IO Text -> cfg -> CardanoNodeConn -> IO ())
    -- ^ Action to run
    -> IO ExitCode
execBenchWithNode networkConfig action = withNoBuffering $ do
    args <- getRestoreBenchArgs

    (_logCfg, tr') <- initBenchmarkLogging "bench-restore" Info
    let tr = if argQuiet args then nullTracer else tr'
    installSignalHandlers (return ())

    case argUseAlreadyRunningNodeSocketPath args of
        Just conn -> do
            action tr (networkConfig args) conn
            pure ExitSuccess
        Nothing -> do
            res <- try $ withNetworkConfiguration args $ \nodeConfig ->
                withCardanoNode
                    (trMessageText tr)
                    Nothing
                    Nothing
                    nodeConfig
                    $ action tr (networkConfig args)
            case res of
                Left (exited :: ProcessHasExited) -> do
                    sayErr $ "FAIL: cardano-node exited with status "
                        <> toText exited
                    pure $ ExitFailure 1
                Right _ -> pure ExitSuccess

withNetworkConfiguration :: RestoreBenchArgs -> (CardanoNodeConfig -> IO a) -> IO a
withNetworkConfiguration args action = do
    -- Temporary directory for storing socket and node database
    let withNodeDir cb = case argNodeDatabaseDir args of
            Nothing -> withSystemTempDirectory "cw-node" cb
            Just d -> do
                createDirectoryIfMissing True d
                cb d

    let networkDir = argsNetworkDir args
    port <- fromIntegral <$> getRandomPort
    withNodeDir $ \dir -> action CardanoNodeConfig
        { nodeDir          = dir
        , nodeConfigFile   = networkDir </> "config.json"
        , nodeDatabaseDir  = fromMaybe "db" (argNodeDatabaseDir args)
        , nodeDlgCertFile  = Nothing
        , nodeSignKeyFile  = Nothing
        , nodeTopologyFile = networkDir </> "topology.json"
        , nodeOpCertFile   = Nothing
        , nodeKesKeyFile   = Nothing
        , nodeVrfKeyFile   = Nothing
        , nodePort         = Just (NodePort port)
        , nodeLoggingHostname = Nothing
        , nodeExecutable   = Nothing
        , nodeOutputFile   = Nothing
        }

argsNetworkDir :: RestoreBenchArgs -> FilePath
argsNetworkDir args = argConfigsDir args </> argNetworkName args

{-------------------------------------------------------------------------------
                                   CLI Parser
-------------------------------------------------------------------------------}

data RestoreBenchArgs = RestoreBenchArgs
    { argNetworkName :: String
    , argConfigsDir :: FilePath
    , argNodeDatabaseDir :: Maybe FilePath
    , argUseAlreadyRunningNodeSocketPath :: Maybe CardanoNodeConn
    , argQuiet :: Bool
    } deriving (Show, Eq)

restoreBenchArgsParser
    :: Maybe String
    -> Maybe FilePath
    -> Maybe FilePath
    -> Maybe CardanoNodeConn
    -> Parser RestoreBenchArgs
restoreBenchArgsParser envNetwork envConfigsDir envNodeDatabaseDir envNodeSocket = RestoreBenchArgs
    <$> strArgument
        ( metavar "NETWORK"
          <> envDefault "NETWORK" envNetwork
          <> help "Blockchain to use. Defaults to $NETWORK.")
    <*> strOption
        ( long "cardano-node-configs"
          <> short 'c'
          <> metavar "DIR"
          <> envDefault "CARDANO_NODE_CONFIGS" envConfigsDir
          <> help "Directory containing configurations for each network. \
              \This must contain a subdirectory corresponding to NETWORK, \
              \which has the files configuration.json and topology.json.")
    <*> optional (strOption
        ( long "node-db"
          <> metavar "DB"
          <> envDefault "NODE_DB" envNodeDatabaseDir
          <> help "Directory to put cardano-node state. Defaults to $NODE_DB, \
              \falls back to temporary directory"))
    <*> optional (option (eitherReader cardanoNodeConn)
        ( long "running-node"
          <> metavar "SOCKET"
          <> envDefault "CARDANO_NODE_SOCKET_PATH" envNodeSocket
          <> help "Path to the socket of an already running cardano-node. \
              \Also set by $CARDANO_NODE_SOCKET_PATH. If not set, cardano-node \
              \will automatically be started."))
    <*> switch
        ( long ("quiet")
          <> help "Reduce unnecessary log output.")
  where
    envDefault :: HasValue f => String -> Maybe a -> Mod f a
    envDefault name env = showDefaultWith (const ('$':name))
        <> maybe mempty value env

-- Add fallback environment variables to parsed args. These are set by
-- `nix/haskell.nix` or `./buildkite/bench-restore.sh` or manually.
getRestoreBenchArgsParser :: IO (Parser RestoreBenchArgs)
getRestoreBenchArgsParser = restoreBenchArgsParser
    <$> lookupEnv' "NETWORK"
    <*> lookupEnv' "CARDANO_NODE_CONFIGS"
    <*> lookupEnv' "NODE_DB"
    <*> parseEnv cardanoNodeConn "CARDANO_NODE_SOCKET"
  where
    lookupEnv' k = lookupEnv k <&> \case
        Just "" -> Nothing
        Just v -> Just v
        Nothing -> Nothing
    parseEnv p k = lookupEnv' k >>= traverse (either exit pure . p)
        where exit err = die (k ++ ": " ++ err)

getRestoreBenchArgs :: IO RestoreBenchArgs
getRestoreBenchArgs = do
    argsParser <- getRestoreBenchArgsParser
    execParser (info (helper <*> argsParser) mempty)

{-------------------------------------------------------------------------------
                                Benchmark runner
-------------------------------------------------------------------------------}

newtype Time = Time
    { unTime :: Double
    } deriving (Show, Generic)

instance Buildable Time where
    build = build . secs . unTime

instance ToJSON Time where
    toJSON = toJSON . pretty @_ @Text

runBenchmarks :: Buildable a => [IO a] -> IO ()
runBenchmarks bs = do
    initializeTime
    -- NOTE: Adding an artificial delay between successive runs to get a better
    -- output for the heap profiling.
    rs <- forM bs $ \io -> io <* let _2s = 2000000 in threadDelay _2s
    sayErr "\n\nAll results:"
    mapM_ (sayErr . pretty) rs

bench :: NFData a => Text -> IO a -> IO (a, Time)
bench benchName action = do
    sayErr $ "Running " <> benchName
    start <- getTime
    res <- action
    evaluate (rnf res)
    finish <- getTime
    let t = Time $ finish - start
    (res, t) <$ sayErr (pretty $ nameF (build benchName) (build t))

initBenchmarkLogging :: Text -> Severity -> IO (CM.Configuration, Trace IO Text)
initBenchmarkLogging name minSeverity = do
    c <- defaultConfigStdout
    CM.setMinSeverity c minSeverity
    CM.setSetupBackends c [CM.KatipBK, CM.AggregationBK]
    (tr, _sb) <- setupTrace_ c name
    pure (c, tr)

withTempSqliteFile :: (FilePath -> IO a) -> IO a
withTempSqliteFile action =
    withSystemTempDirectory "bench-restoration" $ \dir -> do
        let path = dir </> "restoration.db"
        action path
