{-# LANGUAGE LambdaCase #-}

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

    -- * Benchmark runner
    , runBenchmarks
    , bench
    ) where

import Prelude

import Cardano.BM.Configuration.Static
    ( defaultConfigStdout )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Setup
    ( setupTrace_ )
import Cardano.BM.Trace
    ( Trace )
import Cardano.Launcher.Node
    ( CardanoNodeConfig (..)
    , CardanoNodeConn (..)
    , NodePort (..)
    , withCardanoNode
    )
import Cardano.Startup
    ( installSignalHandlers )
import Cardano.Wallet.Logging
    ( trMessageText )
import Cardano.Wallet.Network.Ports
    ( getRandomPort )
import Control.Concurrent
    ( threadDelay )
import Control.DeepSeq
    ( rnf )
import Control.Exception
    ( evaluate )
import Control.Monad
    ( forM, mapM_, void )
import Criterion.Measurement
    ( getTime, initializeTime, secs )
import Data.Functor
    ( (<&>) )
import Data.Text
    ( Text )
import Fmt
    ( fmt, (+|), (|+) )
import Options.Applicative
    ( HasValue
    , Mod
    , Parser
    , execParser
    , help
    , helper
    , info
    , long
    , metavar
    , optional
    , short
    , showDefaultWith
    , strArgument
    , strOption
    , value
    )
import Say
    ( sayErr )
import System.Environment
    ( lookupEnv )
import System.FilePath
    ( (</>) )
import System.IO
    ( BufferMode (..), hSetBuffering, stderr, stdout )
import System.IO.Temp
    ( withSystemTempDirectory )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.BM.Data.BackendKind as CM

{-------------------------------------------------------------------------------
               CLI option handling and cardano-node configuration
-------------------------------------------------------------------------------}

execBenchWithNode
    :: (RestoreBenchArgs -> cfg)
    -- ^ Get backend-specific network configuration from args
    -> (Trace IO Text -> cfg -> FilePath -> IO ())
    -- ^ Action to run
    -> IO ()
execBenchWithNode networkConfig action = do
    args <- getRestoreBenchArgs

    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    (_logCfg, tr) <- initBenchmarkLogging Info
    installSignalHandlers (return ())

    void $ withNetworkConfiguration args $ \nodeConfig ->
        withCardanoNode (trMessageText tr) nodeConfig $ \cp ->
            action tr (networkConfig args) (nodeSocketFile cp)

withNetworkConfiguration :: RestoreBenchArgs -> (CardanoNodeConfig -> IO a) -> IO a
withNetworkConfiguration args action = do
    -- Temporary directory for storing socket and node database
    let withDbDir cb = case argNodeDatabaseDir args of
            Nothing -> withSystemTempDirectory "cw-byron" cb
            Just d -> cb d

    let networkDir = argsNetworkDir args
    port <- fromIntegral <$> getRandomPort
    withDbDir $ \dbDir -> action CardanoNodeConfig
        { nodeConfigFile   = networkDir </> "configuration.json"
        , nodeDatabaseDir  = dbDir
        , nodeDlgCertFile  = Nothing
        , nodeSignKeyFile  = Nothing
        , nodeTopologyFile = networkDir </> "topology.json"
        , nodeOpCertFile   = Nothing
        , nodeKesKeyFile   = Nothing
        , nodeVrfKeyFile   = Nothing
        , nodePort         = Just (NodePort port)
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
    } deriving (Show, Eq)

restoreBenchArgsParser
    :: Maybe String
    -> Maybe FilePath
    -> Maybe FilePath
    -> Parser RestoreBenchArgs
restoreBenchArgsParser envNetwork envConfigsDir envNodeDatabaseDir = RestoreBenchArgs
    <$> strArgument
        ( metavar "NETWORK"
          <> envDefault "NETWORK" envNetwork
          <> help "Blockchain to use. Defaults to $NETWORK.")
    <*> strOption
        ( long "cardano-node-configs"
          <> short 'c'
          <> metavar "DIR"
          <> envDefault "CARDANO_NODE_CONFIGS" envConfigsDir
          <> help "Directory containing configurations for each network.")
    <*> optional (strOption
        ( long "node-db"
          <> metavar "DB"
          <> envDefault "NODE_DB" envNodeDatabaseDir
          <> help "Directory to put cardano-node state. Defaults to $NODE_DB, falls back to temporary directory"))
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
  where
    lookupEnv' k = lookupEnv k <&> \case
        Just "" -> Nothing
        Just v -> Just v
        Nothing -> Nothing

getRestoreBenchArgs :: IO RestoreBenchArgs
getRestoreBenchArgs = do
    argsParser <- getRestoreBenchArgsParser
    execParser (info (helper <*> argsParser) mempty)

{-------------------------------------------------------------------------------
                                Benchmark runner
-------------------------------------------------------------------------------}

runBenchmarks :: [IO (Text, Double)] -> IO ()
runBenchmarks bs = do
    initializeTime
    -- NOTE: Adding an artificial delay between successive runs to get a better
    -- output for the heap profiling.
    rs <- forM bs $ \io -> io <* let _2s = 2000000 in threadDelay _2s
    sayErr "\n\nAll results:"
    mapM_ (uncurry printResult) rs

bench :: Text -> IO () -> IO (Text, Double)
bench benchName action = do
    sayErr $ "Running " <> benchName
    start <- getTime
    res <- action
    evaluate (rnf res)
    finish <- getTime
    let dur = finish - start
    printResult benchName dur
    pure (benchName, dur)

printResult :: Text -> Double -> IO ()
printResult benchName dur = sayErr . fmt $ "  "+|benchName|+": "+|secs dur|+""

initBenchmarkLogging :: Severity -> IO (CM.Configuration, Trace IO Text)
initBenchmarkLogging minSeverity = do
    c <- defaultConfigStdout
    CM.setMinSeverity c minSeverity
    CM.setSetupBackends c [CM.KatipBK, CM.AggregationBK]
    (tr, _sb) <- setupTrace_ c "bench-restore"
    pure (c, tr)
