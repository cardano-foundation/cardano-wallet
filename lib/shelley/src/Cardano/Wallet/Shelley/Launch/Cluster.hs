{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides functions to launch cardano-nodes in a cluster for /testing/.

module Cardano.Wallet.Shelley.Launch.Cluster
    ( -- * Local test cluster launcher
      withCluster
    , LocalClusterConfig (..)
    , localClusterConfigFromEnv
    , ClusterEra (..)

      -- * Node launcher
    , NodeParams (..)
    , singleNodeParams
    , RunningNode (..)

      -- * Cluster node launcher
    , withBFTNode
    , withStakePool
    , PoolConfig (..)
    , defaultPoolConfigs
    , poolConfigsFromEnv
    , clusterEraFromEnv
    , clusterToApiEra
    , withSMASH

      -- * Configuration
    , LogFileConfig (..)
    , logFileConfigFromEnv
    , minSeverityFromEnv
    , nodeMinSeverityFromEnv
    , walletMinSeverityFromEnv
    , testMinSeverityFromEnv
    , testLogDirFromEnv
    , walletListenFromEnv
    , tokenMetadataServerFromEnv

      -- * Faucets
    , sendFaucetFundsTo
    , sendFaucetAssetsTo
    , moveInstantaneousRewardsTo
    , oneMillionAda
    , genMonetaryPolicyScript

    -- * Text fixture global vars
    , operators

    -- * Logging
    , ClusterLog (..)
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPub, xpubPublicKey )
import Cardano.Api.Shelley
    ( ShelleyGenesis (..) )
import Cardano.BM.Data.Output
    ( ScribeDefinition (..)
    , ScribeFormat (..)
    , ScribeKind (..)
    , ScribePrivacy (..)
    )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Chain.Genesis
    ( readGenesisData )
import Cardano.CLI
    ( parseLoggingSeverity )
import Cardano.Launcher
    ( LauncherLog, ProcessHasExited (..) )
import Cardano.Launcher.Node
    ( CardanoNodeConfig (..)
    , CardanoNodeConn
    , NodePort (..)
    , nodeSocketFile
    , withCardanoNode
    )
import Cardano.Pool.Metadata
    ( SMASHPoolId (..) )
import Cardano.Startup
    ( restrictFileMode )
import Cardano.Wallet.Api.Server
    ( Listen (..) )
import Cardano.Wallet.Api.Types
    ( ApiEra (..), HealthStatusSMASH (..) )
import Cardano.Wallet.Logging
    ( BracketLog (..), bracketTracer )
import Cardano.Wallet.Network.Ports
    ( randomUnusedTCPPorts )
import Cardano.Wallet.Primitive.AddressDerivation
    ( hex )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , EpochNo (..)
    , NetworkParameters (..)
    , PoolId (..)
    , TokenMetadataServer (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( AssetId (..), TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxOut )
import Cardano.Wallet.Shelley.Compatibility
    ( NodeVersionData, StandardShelley, nodeToClientVersion )
import Cardano.Wallet.Shelley.Launch
    ( TempDirLog (..), envFromText, isEnvSet, lookupEnvNonEmpty )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex, unsafeRunExceptT )
import Control.Monad
    ( forM, forM_, replicateM, replicateM_, unless, void, when, (>=>) )
import Control.Monad.Trans.Except
    ( withExceptT )
import Control.Retry
    ( constantDelay, limitRetriesByCumulativeDelay, retrying )
import Control.Tracer
    ( Tracer (..), contramap, traceWith )
import Crypto.Hash.Utils
    ( blake2b256 )
import Data.Aeson
    ( FromJSON (..), object, toJSON, (.:), (.=) )
import Data.Bifunctor
    ( bimap )
import Data.ByteArray.Encoding
    ( Base (..), convertToBase )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58 )
import Data.Char
    ( toLower )
import Data.Either
    ( fromRight, isLeft, isRight )
import Data.Functor
    ( ($>), (<&>) )
import Data.List
    ( intercalate, nub, permutations, sort )
import Data.Maybe
    ( catMaybes, fromMaybe )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Time.Clock
    ( UTCTime, addUTCTime, getCurrentTime )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime, utcTimeToPOSIXSeconds )
import Ouroboros.Network.Magic
    ( NetworkMagic (..) )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData (..), nodeToClientCodecCBORTerm )
import System.Directory
    ( copyFile, createDirectory, createDirectoryIfMissing, makeAbsolute )
import System.Environment
    ( getEnvironment )
import System.Exit
    ( ExitCode (..), die )
import System.FilePath
    ( (<.>), (</>) )
import System.IO.Unsafe
    ( unsafePerformIO )
import System.Process.Typed
    ( ProcessConfig
    , proc
    , readProcess
    , readProcessStdout_
    , setEnv
    , setEnvInherit
    )
import Test.Utils.Paths
    ( getTestData )
import Test.Utils.StaticServer
    ( withStaticServer )
import UnliftIO.Async
    ( async, link, race, wait )
import UnliftIO.Chan
    ( newChan, readChan, writeChan )
import UnliftIO.Concurrent
    ( threadDelay )
import UnliftIO.Exception
    ( SomeException, finally, handle, throwIO, throwString )
import UnliftIO.MVar
    ( MVar, modifyMVar, newMVar, putMVar, swapMVar, takeMVar )

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Legacy
import qualified Cardano.Wallet.Byron.Compatibility as Byron
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Shelley.Compatibility as Shelley
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml

-- | Returns the shelley test data path, which is usually relative to the
-- package sources, but can be overridden by the @SHELLEY_TEST_DATA@ environment
-- variable.
getShelleyTestDataPath :: IO FilePath
getShelleyTestDataPath = fromMaybe source <$> lookupEnvNonEmpty var
  where
    source = $(getTestData) </> "cardano-node-shelley"
    var = "SHELLEY_TEST_DATA"

logFileConfigFromEnv :: IO LogFileConfig
logFileConfigFromEnv = LogFileConfig
    <$> nodeMinSeverityFromEnv
    <*> testLogDirFromEnv
    <*> pure Info

minSeverityFromEnv :: Severity -> String -> IO Severity
minSeverityFromEnv def var = lookupEnvNonEmpty var >>= \case
    Nothing -> pure def
    Just arg -> either die pure (parseLoggingSeverity arg)

-- Allow configuring @cardano-node@ log level with the
-- @CARDANO_NODE_TRACING_MIN_SEVERITY@ environment variable.
nodeMinSeverityFromEnv :: IO Severity
nodeMinSeverityFromEnv =
    minSeverityFromEnv Info "CARDANO_NODE_TRACING_MIN_SEVERITY"

-- Allow configuring integration tests and wallet log level with
-- @CARDANO_WALLET_TRACING_MIN_SEVERITY@ environment variable.
walletMinSeverityFromEnv :: IO Severity
walletMinSeverityFromEnv =
    minSeverityFromEnv Warning "CARDANO_WALLET_TRACING_MIN_SEVERITY"

-- Allow configuring integration tests and wallet log level with
-- @TESTS_TRACING_MIN_SEVERITY@ environment variable.
testMinSeverityFromEnv :: IO Severity
testMinSeverityFromEnv =
    minSeverityFromEnv Notice "TESTS_TRACING_MIN_SEVERITY"

-- | Allow configuring which port the wallet server listen to in an integration
-- setup. Crashes if the variable is not a number.
walletListenFromEnv :: IO Listen
walletListenFromEnv = envFromText "CARDANO_WALLET_PORT" >>= \case
    Nothing -> pure ListenOnRandomPort
    Just (Right port) -> pure $ ListenOnPort port
    Just (Left e) -> die $ show e

tokenMetadataServerFromEnv :: IO (Maybe TokenMetadataServer)
tokenMetadataServerFromEnv = envFromText "TOKEN_METADATA_SERVER" >>= \case
    Nothing -> pure Nothing
    Just (Right s) -> pure (Just s)
    Just (Left e) -> die $ show e

-- | Directory for extra logging. Buildkite will set this environment variable
-- and upload logs in it automatically.
testLogDirFromEnv :: IO (Maybe FilePath)
testLogDirFromEnv = traverse makeAbsolute =<< lookupEnvNonEmpty "TESTS_LOGDIR"

-- NOTE
-- Fixture wallets we use in integration tests comes from "initialFunds"
-- referenced in the genesis file. As of today, initial funds are declared as a
-- key-value map in JSON, and parsing libraries does not enforce any particular
-- ordering on the keys when parsing the map. Because this wallet uses sequential
-- derivation, it relies on addresses being discovered in a certain order.
newtype PreserveInitialFundsOrdering =
    PreserveInitialFundsOrdering [TxOut]
    deriving (Show)

{- HLINT ignore "Use concatMap" -}
instance FromJSON PreserveInitialFundsOrdering where
    parseJSON = Aeson.withObject "ByronGenesis" $ \obj -> do
        initialFunds <- obj .: "nonAvvmBalances"

        pure $ PreserveInitialFundsOrdering $ concat $
            map (map mkOut . Map.toList) initialFunds
      where
        mkOut = Byron.fromTxOut . uncurry Legacy.TxOut .
            bimap unsafeMkAddress unsafeMkLovelace
        unsafeMkAddress =
            fromRight bomb . Byron.decodeAddressBase58
          where
            bomb = error "PreserveInitialFundsOrdering: address not valid base58"

        unsafeMkLovelace =
            fromRight bomb . Byron.mkLovelace . read
          where
            bomb = error "PreserveInitialFundsOrdering: invalid lovelace value"

--------------------------------------------------------------------------------
-- For Integration
--------------------------------------------------------------------------------

-- | Make a 'ProcessConfig' for running @cardano-cli@. The program must be on
-- the @PATH@, as normal. Sets @CARDANO_NODE_SOCKET_PATH@ for the subprocess, if
-- a 'CardanoNodeConn' is provided.
cliConfigBase
    :: Tracer IO ClusterLog -- ^ for logging the command
    -> Maybe CardanoNodeConn -- ^ optional cardano-node socket path
    -> [String] -- ^ command-line arguments
    -> IO (ProcessConfig () () ())
cliConfigBase tr conn args = do
    traceWith tr (MsgCLI args)
    env <- getEnvironment
    let mkEnv c = ("CARDANO_NODE_SOCKET_PATH", nodeSocketFile c):env
    let cliEnv = maybe setEnvInherit (setEnv . mkEnv) conn
    pure $ cliEnv $ proc "cardano-cli" args

cliConfigNode
    :: Tracer IO ClusterLog -- ^ for logging the command
    -> CardanoNodeConn -- ^ cardano-node socket path
    -> [String] -- ^ command-line arguments
    -> IO (ProcessConfig () () ())
cliConfigNode tr conn = cliConfigBase tr (Just conn)

cliConfig
    :: Tracer IO ClusterLog -- ^ for logging the command
    -> [String] -- ^ command-line arguments
    -> IO (ProcessConfig () () ())
cliConfig tr = cliConfigBase tr Nothing

-- | A quick helper to interact with the 'cardano-cli'. Assumes the cardano-cli
-- is available in PATH.
cli :: Tracer IO ClusterLog -> [String] -> IO ()
cli tr = cliConfig tr >=> void . readProcessStdoutOrFail

cliLine :: Tracer IO ClusterLog -> [String] -> IO String
cliLine tr = cliConfig tr >=>
    fmap (BL8.unpack . getFirstLine) . readProcessStdoutOrFail

readProcessStdoutOrFail :: ProcessConfig () () () -> IO BL.ByteString
readProcessStdoutOrFail processConfig = do
    (st, out, err) <- readProcess processConfig
    case st of
        ExitSuccess -> pure out
        ExitFailure _ -> throwIO $ userError $ mconcat
            [ "command failed: "
            , BL8.unpack err
            ]


getFirstLine :: BL8.ByteString -> BL8.ByteString
getFirstLine = BL8.takeWhile (\c -> c /= '\r' && c /= '\n')

-- | Like 'cli', but sets the node socket path.
cliNode
    :: Tracer IO ClusterLog
    -> CardanoNodeConn
    -> [String]
    -> IO (ExitCode, BL8.ByteString, BL8.ByteString)
cliNode tr conn = cliConfigNode tr conn >=> readProcess

-- | Runs a @cardano-cli@ command and retries for up to 30 seconds if the
-- command failed.
--
-- Assumes @cardano-cli@ is available in @PATH@.
cliRetry
    :: Tracer IO ClusterLog
    -> Text -- ^ message to print before running command
    -> ProcessConfig () a b
    -> IO ()
cliRetry tr msg processConfig = do
    (st, out, err) <- retrying pol (const isFail) (const cmd)
    traceWith tr $ MsgCLIStatus msg st out err
    case st of
        ExitSuccess -> pure ()
        ExitFailure _ -> throwIO $ ProcessHasExited
            ("cardano-cli failed: " <> BL8.unpack err) st
  where
    cmd = do
        traceWith tr $ MsgCLIRetry msg
        (st, out, err) <- readProcess processConfig
        case st of
            ExitSuccess -> pure ()
            ExitFailure code -> traceWith tr (MsgCLIRetryResult msg code err)
        pure (st, out, err)
    isFail (st, _, _) = pure (st /= ExitSuccess)
    pol = limitRetriesByCumulativeDelay 30_000_000 $ constantDelay 1_000_000

newtype PoolConfig = PoolConfig
    { retirementEpoch :: Maybe EpochNo
      -- ^ An optional retirement epoch. If specified, then a pool retirement
      -- certificate will be published after the pool is initially registered.
    }
    deriving (Eq, Show)


defaultPoolConfigs :: [PoolConfig]
defaultPoolConfigs =
    [ -- This pool should never retire:
      PoolConfig {retirementEpoch = Nothing}
      -- This pool should retire almost immediately:
    , PoolConfig {retirementEpoch = Just 3}
      -- This pool should retire, but not within the duration of a test run:
    , PoolConfig {retirementEpoch = Just 100_000}
      -- This pool should retire, but not within the duration of a test run:
    , PoolConfig {retirementEpoch = Just 1_000_000}
    ]

poolConfigsFromEnv :: IO [PoolConfig]
poolConfigsFromEnv = isEnvSet "NO_POOLS" <&> \case
    False -> defaultPoolConfigs
    True -> []

localClusterConfigFromEnv :: IO LocalClusterConfig
localClusterConfigFromEnv = LocalClusterConfig
    <$> poolConfigsFromEnv
    <*> clusterEraFromEnv
    <*> logFileConfigFromEnv

data ClusterEra
    = ByronNoHardFork
    | ShelleyHardFork
    | AllegraHardFork
    | MaryHardFork
    | AlonzoHardFork
    deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | Convert @ClusterEra@ to a @ApiEra@.
clusterToApiEra :: ClusterEra -> ApiEra
clusterToApiEra = \case
    ByronNoHardFork -> ApiByron
    ShelleyHardFork -> ApiShelley
    AllegraHardFork -> ApiAllegra
    MaryHardFork -> ApiMary
    AlonzoHardFork -> ApiAlonzo

-- | Defaults to the latest era.
clusterEraFromEnv :: IO ClusterEra
clusterEraFromEnv =
    fmap withDefault . traverse getEra =<< lookupEnvNonEmpty var
  where
    var = "LOCAL_CLUSTER_ERA"
    getEra env = case map toLower env of
        "byron" -> pure ByronNoHardFork
        "shelley" -> pure ShelleyHardFork
        "allegra" -> pure AllegraHardFork
        "mary" -> pure MaryHardFork
        "alonzo" -> pure AlonzoHardFork
        _ -> die $ var ++ ": unknown era"
    withDefault = fromMaybe maxBound

data LocalClusterConfig = LocalClusterConfig
    { cfgStakePools :: [PoolConfig]
    -- ^ Stake pools to register.
    , cfgLastHardFork :: ClusterEra
    -- ^ Which era to use.
    , cfgNodeLogging :: LogFileConfig
    -- ^ Log severity for node.
    } deriving (Show)

-- | Information about a launched node.
data RunningNode = RunningNode
    CardanoNodeConn
    -- ^ Socket path
    Block
    -- ^ Genesis block
    (NetworkParameters, NodeVersionData)

-- | Execute an action after starting a cluster of stake pools. The cluster also
-- contains a single BFT node that is pre-configured with keys available in the
-- test data.
--
-- This BFT node is essential in order to bootstrap the chain and allow
-- registering pools. Passing `0` as a number of pool will simply start a single
-- BFT node.
--
-- The cluster is configured to automatically hard fork to Shelley at epoch 1
-- and then to Allegra at epoch 2. Callback actions can be provided to run
-- a little time after the hard forks are scheduled.
--
-- The callback actions are not guaranteed to use the same node.
withCluster
    :: Tracer IO ClusterLog
    -- ^ Trace for subprocess control logging.
    -> FilePath
    -- ^ Temporary directory to create config files in.
    -> LocalClusterConfig
    -- ^ The configurations of pools to spawn.
    -> (RunningNode -> IO ())
    -- ^ Setup action to run using the BFT node.
    -> (RunningNode -> IO a)
    -- ^ Action to run once when the stake pools are setup.
    -> IO a
withCluster tr dir LocalClusterConfig{..} onSetup onClusterStart =
    bracketTracer' tr "withCluster" $ do
        traceWith tr $ MsgStartingCluster dir
        resetGlobals
        putClusterEra dir cfgLastHardFork
        let poolCount = length cfgStakePools
        (port0:ports) <- randomUnusedTCPPorts (poolCount + 2)
        systemStart <- addUTCTime 1 <$> getCurrentTime
        let bftCfg = NodeParams systemStart cfgLastHardFork
                (head $ rotate ports) cfgNodeLogging
        withBFTNode tr dir bftCfg $ \bftSocket block0 params -> do
            waitForSocket tr bftSocket

            onSetup $ RunningNode bftSocket block0 params

            (rawTx, faucetPrv) <- prepareKeyRegistration tr dir
            tx <- signTx tr dir rawTx [faucetPrv]
            submitTx tr bftSocket "pre-registered stake key" tx

            waitGroup <- newChan
            doneGroup <- newChan
            let waitAll = do
                    traceWith tr $
                        MsgDebug "waiting for stake pools to register"
                    replicateM poolCount (readChan waitGroup)

            let onException :: SomeException -> IO ()
                onException e = do
                    traceWith tr $
                        MsgDebug $ "exception while starting pool: " <>
                        T.pack (show e)
                    writeChan waitGroup (Left e)

            let pledgeOf 0 = 2*oneMillionAda
                pledgeOf _ = oneMillionAda
            asyncs <- forM (zip3 [0..] cfgStakePools $ tail $ rotate ports) $
                \(idx, poolConfig, (port, peers)) -> do
                    async (handle onException $ do
                        let spCfg = NodeParams systemStart cfgLastHardFork
                                (port, peers) cfgNodeLogging
                        withStakePool
                            tr dir idx spCfg (pledgeOf idx) poolConfig $ do
                                writeChan waitGroup $ Right port
                                readChan doneGroup)
            mapM_ link asyncs

            let cancelAll = do
                    traceWith tr $ MsgDebug "stopping all stake pools"
                    replicateM_ poolCount (writeChan doneGroup ())
                    mapM_ wait asyncs

            traceWith tr $ MsgRegisteringStakePools poolCount
            group <- waitAll
            if length (filter isRight group) /= poolCount then do
                cancelAll
                let errors = show (filter isLeft group)
                throwIO $ ProcessHasExited
                    ("cluster didn't start correctly: " <> errors)
                    (ExitFailure 1)
            else do
                let cfg = NodeParams systemStart cfgLastHardFork
                        (port0, ports) cfgNodeLogging
                withRelayNode tr dir cfg $ \socket -> do
                    let runningRelay = RunningNode socket block0 params
                    onClusterStart runningRelay `finally` cancelAll
  where
    -- | Get permutations of the size (n-1) for a list of n elements, alongside
    -- with the element left aside. `[a]` is really expected to be `Set a`.
    --
    -- >>> rotate [1,2,3]
    -- [(1,[2,3]), (2, [1,3]), (3, [1,2])]
    rotate :: Ord a => [a] -> [(a, [a])]
    rotate = nub . fmap (\(x:xs) -> (x, sort xs)) . permutations

data LogFileConfig = LogFileConfig
    { minSeverityTerminal :: Severity
      -- ^ Minimum logging severity
    , extraLogDir :: Maybe FilePath
      -- ^ Optional additional output to log file
    , minSeverityFile :: Severity
      -- ^ Minimum logging severity for 'extraLogFile'
    } deriving (Show)

-- | Configuration parameters which update the @node.config@ test data file.
data NodeParams = NodeParams
    { systemStart :: UTCTime
      -- ^ Genesis block start time
    , nodeHardForks :: ClusterEra
      -- ^ Era to hard fork into.
    , nodePeers :: (Int, [Int])
      -- ^ A list of ports used by peers and this node
    , nodeLogConfig :: LogFileConfig
      -- ^ The node will always log to "cardano-node.log" relative to the
      -- config. This option can set the minimum severity and add another output
      -- file.
    } deriving (Show)

singleNodeParams :: Severity -> Maybe (FilePath, Severity) -> IO NodeParams
singleNodeParams severity extraLogFile = do
    systemStart <- getCurrentTime
    let logCfg = LogFileConfig
            { minSeverityTerminal = severity
            , extraLogDir = fmap fst extraLogFile
            , minSeverityFile = maybe severity snd extraLogFile
            }
    pure $ NodeParams systemStart maxBound (0, []) logCfg

withBFTNode
    :: Tracer IO ClusterLog
    -- ^ Trace for subprocess control logging
    -> FilePath
    -- ^ Parent state directory. Node data will be created in a subdirectory of
    -- this.
    -> NodeParams
    -- ^ Parameters used to generate config files.
    -> (CardanoNodeConn -> Block -> (NetworkParameters, NodeVersionData) -> IO a)
    -- ^ Callback function with genesis parameters
    -> IO a
withBFTNode tr baseDir params action =
    bracketTracer' tr "withBFTNode" $ do
        createDirectoryIfMissing False dir
        source <- getShelleyTestDataPath

        let copyKeyFile f = do
                let dst = dir </> f
                copyFile (source </> f) dst
                restrictFileMode dst
                pure dst

        [bftCert, bftPrv, vrfPrv, kesPrv, opCert] <- forM
            [ "bft-leader" <> ".byron.cert"
            , "bft-leader" <> ".byron.skey"
            , "bft-leader" <> ".vrf.skey"
            , "bft-leader" <> ".kes.skey"
            , "bft-leader" <> ".opcert"
            ]
            copyKeyFile

        (config, block0, networkParams, versionData)
            <- genConfig dir systemStart hardForks (setLoggingName name logCfg)
        topology <- genTopology dir peers

        let cfg = CardanoNodeConfig
                { nodeDir = dir
                , nodeConfigFile = config
                , nodeTopologyFile = topology
                , nodeDatabaseDir = "db"
                , nodeDlgCertFile = Just bftCert
                , nodeSignKeyFile = Just bftPrv
                , nodeOpCertFile = Just opCert
                , nodeKesKeyFile = Just kesPrv
                , nodeVrfKeyFile = Just vrfPrv
                , nodePort = Just (NodePort port)
                , nodeLoggingHostname = Just name
                }

        withCardanoNodeProcess tr name cfg $ \socket ->
            action socket block0 (networkParams, versionData)
  where
    name = "bft"
    dir = baseDir </> name
    NodeParams systemStart hardForks (port, peers) logCfg = params

-- | Launches a @cardano-node@ with the given configuration which will not forge
-- blocks, but has every other cluster node as its peer. Any transactions
-- submitted to this node will be broadcast to every node in the cluster.
withRelayNode
    :: Tracer IO ClusterLog
    -- ^ Trace for subprocess control logging
    -> FilePath
    -- ^ Parent state directory. Node data will be created in a subdirectory of
    -- this.
    -> NodeParams
    -- ^ Parameters used to generate config files.
    -> (CardanoNodeConn -> IO a)
    -- ^ Callback function with socket path
    -> IO a
withRelayNode tr baseDir params act =
    bracketTracer' tr "withRelayNode" $ do
        createDirectory dir

        let logCfg' = setLoggingName name logCfg
        (config, _, _, _) <- genConfig dir systemStart hardForks logCfg'
        topology <- genTopology dir peers

        let cfg = CardanoNodeConfig
                { nodeDir = dir
                , nodeConfigFile = config
                , nodeTopologyFile = topology
                , nodeDatabaseDir = "db"
                , nodeDlgCertFile = Nothing
                , nodeSignKeyFile = Nothing
                , nodeOpCertFile = Nothing
                , nodeKesKeyFile = Nothing
                , nodeVrfKeyFile = Nothing
                , nodePort = Just (NodePort port)
                , nodeLoggingHostname = Just name
                }

        withCardanoNodeProcess tr name cfg act
  where
    name = "node"
    dir = baseDir </> name
    NodeParams systemStart hardForks (port, peers) logCfg = params

-- | Populates the configuration directory of a stake pool @cardano-node@.
--
-- Returns a tuple with:
--  * A config for launching the stake pool node.
--  * The public operator certificate - used for verifying registration.
--  * A transaction which should be submitted to register the pool.
setupStakePoolData
    :: Tracer IO ClusterLog
    -- ^ Logging object.
    -> FilePath
    -- ^ Output directory.
    -> String
    -- ^ Short name of the stake pool.
    -> NodeParams
    -- ^ Parameters used for generating config files.
    -> String
    -- ^ Base URL of metadata server.
    -> Integer
    -- ^ Pledge (and stake) amount.
    -> Maybe EpochNo
    -- ^ Optional retirement epoch.
    -> IO (CardanoNodeConfig, FilePath, FilePath)
setupStakePoolData tr dir name params url pledgeAmt mRetirementEpoch = do
    let NodeParams systemStart hardForks (port, peers) logCfg = params

    (opPrv, opPub, opCount, metadata) <- genOperatorKeyPair tr dir
    (vrfPrv, vrfPub) <- genVrfKeyPair tr dir
    (kesPrv, kesPub) <- genKesKeyPair tr dir
    (stakePrv, stakePub) <- genStakeAddrKeyPair tr dir

    stakeCert <- issueStakeCert tr dir "stake-pool" stakePub
    poolRegistrationCert <- issuePoolRegistrationCert
        tr dir opPub vrfPub stakePub url metadata pledgeAmt
    mPoolRetirementCert <- traverse
        (issuePoolRetirementCert tr dir opPub) mRetirementEpoch
    dlgCert <- issueDlgCert tr dir stakePub opPub
    opCert <- issueOpCert tr dir kesPub opPrv opCount

    let logCfg' = setLoggingName name logCfg
    (config, _, _, _) <- genConfig dir systemStart hardForks logCfg'
    topology <- genTopology dir peers

    -- In order to get a working stake pool we need to.
    --
    -- 1. Register a stake key for our pool.
    -- 2. Register the stake pool
    -- 3. Delegate funds to our pool's key.
    --
    -- We cheat a bit here by delegating to our stake address right away
    -- in the transaction used to registered the stake key and the pool
    -- itself.  Thus, in a single transaction, we end up with a
    -- registered pool with some stake!

    let certificates = catMaybes
            [ pure stakeCert
            , pure poolRegistrationCert
            , pure dlgCert
            , mPoolRetirementCert
            ]
    (rawTx, faucetPrv) <- preparePoolRegistration
        tr dir stakePub certificates pledgeAmt
    tx <- signTx tr dir rawTx [faucetPrv, stakePrv, opPrv]

    let cfg = CardanoNodeConfig
            { nodeDir = dir
            , nodeConfigFile = config
            , nodeTopologyFile = topology
            , nodeDatabaseDir = "db"
            , nodeDlgCertFile = Nothing
            , nodeSignKeyFile = Nothing
            , nodeOpCertFile = Just opCert
            , nodeKesKeyFile = Just kesPrv
            , nodeVrfKeyFile = Just vrfPrv
            , nodePort = Just (NodePort port)
            , nodeLoggingHostname = Just name
            }

    pure (cfg, opPub, tx)

-- | Start a "stake pool node". The pool will register itself.
withStakePool
    :: Tracer IO ClusterLog
    -- ^ Trace for subprocess control logging
    -> FilePath
    -- ^ Parent state directory. Node and stake pool data will be created in a
    -- subdirectory of this.
    -> Int
    -- ^ Stake pool index in the cluster
    -> NodeParams
    -- ^ Configuration for the underlying node
    -> Integer
    -- ^ Pledge amount / initial stake
    -> PoolConfig
    -- ^ Pool configuration
    -> IO a
    -- ^ Action to run with the stake pool running
    -> IO a
withStakePool tr baseDir idx params pledgeAmt poolConfig action =
    bracketTracer' tr "withStakePool" $ do
        createDirectory dir
        withStaticServer dir $ \url -> do
            traceWith tr $ MsgStartedStaticServer dir url
            (cfg, opPub, tx) <- setupStakePoolData
                tr dir name params url pledgeAmt (retirementEpoch poolConfig)
            withCardanoNodeProcess tr name cfg $ \socket -> do
                submitTx tr socket name tx
                timeout 120
                    ( "pool registration"
                    , waitUntilRegistered tr socket name opPub )
                action
  where
    dir = baseDir </> name
    name = "pool-" ++ show idx

-- | Run a SMASH stub server, serving some delisted pool IDs.
withSMASH
    :: FilePath
    -- ^ Parent directory to store static files
    -> (String -> IO a)
    -- ^ Action, taking base URL
    -> IO a
withSMASH parentDir action = do
    let staticDir = parentDir </> "smash"
    let baseDir = staticDir </> "api" </> "v1"

    -- write pool metadatas
    forM_ operatorsFixture $ \(poolId, _, _, _, metadata) -> do
        let bytes = Aeson.encode metadata

        let metadataDir = baseDir </> "metadata"
            poolDir = metadataDir </> T.unpack (toText poolId)
            hash = blake2b256S (BL.toStrict bytes)
            hashFile = poolDir </> hash

        createDirectoryIfMissing True poolDir
        BL8.writeFile (poolDir </> hashFile) bytes

    -- write delisted pools
    let delisted = [SMASHPoolId (T.pack
            "b45768c1a2da4bd13ebcaa1ea51408eda31dcc21765ccbd407cda9f2")]
        bytes = Aeson.encode delisted
    BL8.writeFile (baseDir </> "delisted") bytes

    -- health check
    let health = Aeson.encode (HealthStatusSMASH "OK" "1.2.0")
    BL8.writeFile (baseDir </> "status") health

    withStaticServer staticDir action

withCardanoNodeProcess
    :: Tracer IO ClusterLog
    -> String
    -> CardanoNodeConfig
    -> (CardanoNodeConn -> IO a)
    -> IO a
withCardanoNodeProcess tr name cfg = withCardanoNode tr' cfg >=> throwErrs
  where
    tr' = contramap (MsgLauncher name) tr
    throwErrs = either throwIO pure

setLoggingName :: String -> LogFileConfig -> LogFileConfig
setLoggingName name cfg = cfg { extraLogDir = filename <$> extraLogDir cfg }
    where filename = (</> (name <.> "log"))

genConfig
    :: FilePath
    -- ^ A top-level directory where to put the configuration.
    -> UTCTime
    -- ^ Genesis block start time
    -> ClusterEra
    -- ^ Last era to hard fork into.
    -> LogFileConfig
    -- ^ Minimum severity level for logging and optional /extra/ logging output
    -> IO (FilePath, Block, NetworkParameters, NodeVersionData)
genConfig dir systemStart clusterEra logCfg = do
    let LogFileConfig severity mExtraLogFile extraSev = logCfg
    let startTime = round @_ @Int . utcTimeToPOSIXSeconds $ systemStart
    let systemStart' = posixSecondsToUTCTime . fromRational . toRational $ startTime
    source <- getShelleyTestDataPath

    let fileScribe (path, sev) = ScribeDefinition
                { scName = path
                , scFormat = ScText
                , scKind = FileSK
                , scMinSev = sev
                , scMaxSev = Critical
                , scPrivacy = ScPublic
                , scRotation = Nothing
                }

    let scribes = map fileScribe $ catMaybes
            [ Just ("cardano-node.log", severity)
            , (, extraSev) . T.pack <$> mExtraLogFile
            ]

    ----
    -- Configuration
    Yaml.decodeFileThrow (source </> "node.config")
        >>= withAddedKey "ShelleyGenesisFile" shelleyGenesisFile
        >>= withAddedKey "ByronGenesisFile" byronGenesisFile
        >>= withAddedKey "AlonzoGenesisFile" alonzoGenesisFile
        >>= withHardForks clusterEra
        >>= withAddedKey "minSeverity" Debug
        >>= withScribes scribes
        >>= withObject (addMinSeverityStdout severity)
        >>= Yaml.encodeFile (dir </> "node.config")

    ----
    -- Byron Genesis
    Yaml.decodeFileThrow @_ @Aeson.Value (source </> "byron-genesis.yaml")
        >>= withAddedKey "startTime" startTime
        >>= withObject transformInitialFunds
        >>= Aeson.encodeFile byronGenesisFile

    ----
    -- Shelley Genesis
    Yaml.decodeFileThrow @_ @Aeson.Value (source </> "shelley-genesis.yaml")
        >>= withObject (pure . updateSystemStart systemStart')
        >>= Aeson.encodeFile shelleyGenesisFile

    Yaml.decodeFileThrow @_ @Aeson.Value (source </> "alonzo-genesis.yaml")
        >>= Aeson.encodeFile alonzoGenesisFile

    ----
    -- Initial Funds.
    PreserveInitialFundsOrdering initialFunds <-
        Yaml.decodeFileThrow @_ @Aeson.Value (source </> "byron-genesis.yaml")
        >>= withAddedKey "startTime" startTime
        >>= either fail pure . Aeson.parseEither parseJSON
    (byronGenesisData, byronGenesisHash) <- unsafeRunExceptT
        $ withExceptT show
        $ readGenesisData byronGenesisFile
    let (byronParams, _) = Byron.fromGenesisData (byronGenesisData, byronGenesisHash)
    let gp = genesisParameters byronParams
    let block0 = Byron.genesisBlockFromTxOuts gp initialFunds

    ----
    -- Parameters
    shelleyGenesis <- Yaml.decodeFileThrow
        @_ @(ShelleyGenesis StandardShelley) shelleyGenesisFile
    let networkMagic = sgNetworkMagic shelleyGenesis
    let shelleyParams = fst $ Shelley.fromGenesisData shelleyGenesis []
    let versionData =
            ( NodeToClientVersionData $ NetworkMagic networkMagic
            , nodeToClientCodecCBORTerm nodeToClientVersion
            )

    pure
        ( dir </> "node.config"
        , block0
        , byronParams { protocolParameters = protocolParameters shelleyParams }
        , versionData
        )
  where
    shelleyGenesisFile :: FilePath
    shelleyGenesisFile = dir </> "shelley-genesis.json"

    alonzoGenesisFile :: FilePath
    alonzoGenesisFile = dir </> "alonzo-genesis.json"

    byronGenesisFile :: FilePath
    byronGenesisFile = dir </> "byron-genesis.json"

    withScribes scribes =
        withAddedKey "setupScribes" scribes
        >=> withAddedKey "defaultScribes"
            (map (\s -> [toJSON $ scKind s, toJSON $ scName s]) scribes)

    -- we need to specify genesis file location every run in tmp
    withAddedKey k v = withObject (pure . HM.insert k (toJSON v))
    withHardForks era = withObject (pure . HM.union (HM.fromList hardForks))
      where
        hardForks =
            [ ("Test" <> T.pack (show hardFork) <> "AtEpoch", Yaml.Number 0)
            | hardFork <- [ShelleyHardFork .. era] ]

-- | Generate a topology file from a list of peers.
genTopology :: FilePath -> [Int] -> IO FilePath
genTopology dir peers = do
    let file = dir </> "node.topology"
    Aeson.encodeFile file $ Aeson.object [ "Producers" .= map encodePeer peers ]
    pure file
  where
    encodePeer :: Int -> Aeson.Value
    encodePeer port = Aeson.object
        [ "addr"    .= ("127.0.0.1" :: String)
        , "port"    .= port
        , "valency" .= (1 :: Int)
        ]

-- | Create a key pair for a node operator's offline key and a new certificate
-- issue counter
genOperatorKeyPair :: Tracer IO ClusterLog -> FilePath -> IO (FilePath, FilePath, FilePath, Aeson.Value)
genOperatorKeyPair tr dir = do
    traceWith tr $ MsgGenOperatorKeyPair dir
    (_poolId, pub, prv, count, metadata) <- takeMVar operators >>= \case
        [] -> fail "genOperatorKeyPair: Awe crap! No more operators available!"
        (op:q) -> putMVar operators q $> op

    let opPub = dir </> "op.pub"
    let opPrv = dir </> "op.prv"
    let opCount = dir </> "op.count"

    Aeson.encodeFile opPub pub
    Aeson.encodeFile opPrv prv
    Aeson.encodeFile opCount count

    pure (opPrv, opPub, opCount, metadata)

-- | Create a key pair for a node KES operational key
genKesKeyPair :: Tracer IO ClusterLog -> FilePath -> IO (FilePath, FilePath)
genKesKeyPair tr dir = do
    let kesPub = dir </> "kes.pub"
    let kesPrv = dir </> "kes.prv"
    cli tr
        [ "node", "key-gen-KES"
        , "--verification-key-file", kesPub
        , "--signing-key-file", kesPrv
        ]
    pure (kesPrv, kesPub)

-- | Create a key pair for a node VRF operational key
genVrfKeyPair :: Tracer IO ClusterLog -> FilePath -> IO (FilePath, FilePath)
genVrfKeyPair tr dir = do
    let vrfPub = dir </> "vrf.pub"
    let vrfPrv = dir </> "vrf.prv"
    cli tr
        [ "node", "key-gen-VRF"
        , "--verification-key-file", vrfPub
        , "--signing-key-file", vrfPrv
        ]
    pure (vrfPrv, vrfPub)

-- | Create a stake address key pair
genStakeAddrKeyPair :: Tracer IO ClusterLog -> FilePath -> IO (FilePath, FilePath)
genStakeAddrKeyPair tr dir = do
    let stakePub = dir </> "stake.pub"
    let stakePrv = dir </> "stake.prv"
    cli tr
        [ "stake-address", "key-gen"
        , "--verification-key-file", stakePub
        , "--signing-key-file", stakePrv
        ]
    pure (stakePrv, stakePub)

-- | Issue a node operational certificate
issueOpCert :: Tracer IO ClusterLog -> FilePath -> FilePath -> FilePath -> FilePath -> IO FilePath
issueOpCert tr dir kesPub opPrv opCount = do
    let file = dir </> "op.cert"
    cli tr
        [ "node", "issue-op-cert"
        , "--kes-verification-key-file", kesPub
        , "--cold-signing-key-file", opPrv
        , "--operational-certificate-issue-counter-file", opCount
        , "--kes-period", "0"
        , "--out-file", file
        ]
    pure file

-- | Create a stake address registration certificate
issueStakeCert
    :: Tracer IO ClusterLog
    -> FilePath
    -> String
    -> FilePath
    -> IO FilePath
issueStakeCert tr dir prefix stakePub = do
    let file = dir </> prefix <> "-stake.cert"
    cli tr
        [ "stake-address", "registration-certificate"
        , "--staking-verification-key-file", stakePub
        , "--out-file", file
        ]
    pure file

-- | Create a stake pool registration certificate
issuePoolRegistrationCert
    :: Tracer IO ClusterLog
    -> FilePath
    -> FilePath
    -> FilePath
    -> FilePath
    -> String
    -> Aeson.Value
    -> Integer
    -> IO FilePath
issuePoolRegistrationCert
    tr dir opPub vrfPub stakePub baseURL metadata pledgeAmt = do
        let file  = dir </> "pool.cert"
        let bytes = Aeson.encode metadata
        BL8.writeFile (dir </> "metadata.json") bytes
        cli tr
            [ "stake-pool", "registration-certificate"
            , "--cold-verification-key-file", opPub
            , "--vrf-verification-key-file", vrfPub
            , "--pool-pledge", show pledgeAmt
            , "--pool-cost", "0"
            , "--pool-margin", "0.1"
            , "--pool-reward-account-verification-key-file", stakePub
            , "--pool-owner-stake-verification-key-file", stakePub
            , "--metadata-url", baseURL </> "metadata.json"
            , "--metadata-hash", blake2b256S (BL.toStrict bytes)
            , "--mainnet"
            , "--out-file", file
            ]
        pure file

issuePoolRetirementCert
    :: Tracer IO ClusterLog
    -> FilePath
    -> FilePath
    -> EpochNo
    -> IO FilePath
issuePoolRetirementCert tr dir opPub retirementEpoch = do
    let file  = dir </> "pool-retirement.cert"
    cli tr
        [ "stake-pool", "deregistration-certificate"
        , "--cold-verification-key-file", opPub
        , "--epoch", show (unEpochNo retirementEpoch)
        , "--out-file", file
        ]
    pure file

-- | Create a stake address delegation certificate.
issueDlgCert :: Tracer IO ClusterLog -> FilePath -> FilePath -> FilePath -> IO FilePath
issueDlgCert tr dir stakePub opPub = do
    let file = dir </> "dlg.cert"
    cli tr
        [ "stake-address", "delegation-certificate"
        , "--staking-verification-key-file", stakePub
        , "--stake-pool-verification-key-file", opPub
        , "--out-file", file
        ]
    pure file

-- | Generate a raw transaction. We kill two birds one stone here by also
-- automatically delegating 'pledge' amount to the given stake key.
preparePoolRegistration
    :: Tracer IO ClusterLog
    -> FilePath
    -> FilePath
    -> [FilePath]
    -> Integer
    -> IO (FilePath, FilePath)
preparePoolRegistration tr dir stakePub certs pledgeAmt = do
    let file = dir </> "tx.raw"
    addr <- genSinkAddress tr dir (Just stakePub)
    (faucetInput, faucetPrv) <- takeFaucet
    cli tr $
        [ "transaction", "build-raw"
        , "--tx-in", faucetInput
        , "--tx-out", addr <> "+" <> show pledgeAmt
        , "--ttl", "400"
        , "--fee", show (faucetAmt - pledgeAmt - depositAmt)
        , "--out-file", file
        ] ++ mconcat ((\cert -> ["--certificate-file",cert]) <$> certs)

    pure (file, faucetPrv)

-- | For creating test fixtures. Returns PolicyId, signing key, and verification
-- key hash, all hex-encoded. Files are put in the given directory.
genMonetaryPolicyScript
    :: Tracer IO ClusterLog
    -> FilePath -- ^ Directory
    -> IO (String, (String, String))
genMonetaryPolicyScript tr dir = do
    let policyPub = dir </> "policy.pub"
    let policyPrv = dir </> "policy.prv"

    cli tr
        [ "address", "key-gen"
        , "--verification-key-file", policyPub
        , "--signing-key-file", policyPrv
        ]
    skey <- T.unpack <$> readKeyFromFile policyPrv
    vkeyHash <- cliLine tr
        [ "address", "key-hash"
        , "--payment-verification-key-file", policyPub
        ]
    script <- writeMonetaryPolicyScriptFile dir vkeyHash
    policyId <- cliLine tr
        [ "transaction", "policyid"
        , "--script-file", script
        ]

    pure (policyId, (skey, vkeyHash))

writeMonetaryPolicyScriptFile
    :: FilePath -- ^ Destination directory for script file
    -> String -- ^ The script verification key hash
    -> IO FilePath -- ^ Returns the filename written
writeMonetaryPolicyScriptFile dir keyHash = do
    let scriptFile = dir </> keyHash <.> "script"
    Aeson.encodeFile scriptFile $ object
        [ "type" .= Aeson.String "sig"
        , "keyHash" .= keyHash
        ]
    pure scriptFile

writePolicySigningKey
    :: FilePath -- ^ destination directory for key file
    -> String -- ^ Name of file, keyhash perhaps.
    -> String -- ^ The cbor-encoded key material, encoded in hex
    -> IO FilePath -- ^ Returns the filename written
writePolicySigningKey dir keyHash cborHex = do
    let keyFile = dir </> keyHash <.> "skey"
    Aeson.encodeFile keyFile $ object
        [ "type" .= Aeson.String "PaymentSigningKeyShelley_ed25519"
        , "description" .= Aeson.String "Payment Signing Key"
        , "cborHex" .= cborHex
        ]
    pure keyFile

-- | Dig in to a @cardano-cli@ TextView key file to get the hex-encoded key.
readKeyFromFile :: FilePath -> IO Text
readKeyFromFile f = do
    textView <- either throwString pure =<< Aeson.eitherDecodeFileStrict' f
    either throwString pure $ Aeson.parseEither
        (Aeson.withObject "TextView" (.: "cborHex")) textView

sendFaucetFundsTo
    :: Tracer IO ClusterLog
    -> CardanoNodeConn
    -> FilePath
    -> [(String, Coin)]
    -> IO ()
sendFaucetFundsTo tr conn dir targets = batch 80 targets $
    sendFaucet tr conn dir "ada" . map coinBundle
  where
    coinBundle = fmap (\c -> (TokenBundle.fromCoin c, []))

-- | Create transactions to fund the given faucet addresses with Ada and assets.
--
-- Beside the 'TokenBundle' of Ada and assets, there is a list of
-- @(signing key, verification key hash)@ pairs needed to sign the
-- minting transaction.
sendFaucetAssetsTo
    :: Tracer IO ClusterLog
    -> CardanoNodeConn
    -> FilePath
    -> Int -- ^ batch size
    -> [(String, (TokenBundle, [(String, String)]))] -- ^ (address, assets)
    -> IO ()
sendFaucetAssetsTo tr conn dir batchSize targets = do
    era <- getClusterEra dir
    when (era >= MaryHardFork) $
        batch batchSize targets $ sendFaucet tr conn dir "assets"

-- | Build, sign, and send a batch of faucet funding transactions using
-- @cardano-cli@. This function is used by 'sendFaucetFundsTo' and
-- 'sendFaucetAssetsTo'.
sendFaucet
    :: Tracer IO ClusterLog
    -> CardanoNodeConn
    -> FilePath
    -> String -- ^ label for logging
    -> [(String, (TokenBundle, [(String, String)]))]
    -> IO ()
sendFaucet tr conn dir what targets = do
    (faucetInput, faucetPrv) <- takeFaucet
    let file = dir </> "faucet-tx.raw"

    let mkOutput addr (TokenBundle (Coin c) tokens) =
            [ "--tx-out"
            , unwords $ [ addr, show c, "lovelace"] ++
                map (("+ " ++) . cliAsset) (TokenMap.toFlatList tokens)
            ]
        cliAsset (aid, (TokenQuantity q)) = unwords [show q, cliAssetId aid]
        cliAssetId (AssetId pid (UnsafeTokenName name)) =
            T.unpack (toText pid) ++
            (if BS.null name then "" else "." ++ B8.unpack name)
        mkMint [] = []
        mkMint assets = ["--mint", intercalate " + " (map cliAsset assets)]

    let total = fromIntegral $ sum $
            map (unCoin . TokenBundle.getCoin . fst . snd) targets
    when (total > faucetAmt) $ error "sendFaucetFundsTo: too much to pay"

    let targetAssets = concatMap (snd . TokenBundle.toFlatList . fst . snd) targets

    scripts <- forM (nub $ concatMap (map snd . snd . snd) targets) $
        writeMonetaryPolicyScriptFile dir

    cli tr $
        [ "transaction", "build-raw"
        , "--tx-in", faucetInput
        , "--ttl", "6000000"
            -- Big enough to allow minting in the actual integration tests,
            -- before the wallet API supports it.
        , "--fee", show (faucetAmt - total)
        , "--out-file", file
        ] ++
        concatMap (uncurry mkOutput . fmap fst) targets ++
        mkMint targetAssets ++
        (concatMap (\f -> ["--minting-script-file", f]) scripts)

    policyKeys <- forM (nub $ concatMap (snd . snd) targets) $
        \(skey, keyHash) -> writePolicySigningKey dir keyHash skey

    tx <- signTx tr dir file (faucetPrv:policyKeys)
    submitTx tr conn (what ++ " faucet tx") tx

batch :: Int -> [a] -> ([a] -> IO b) -> IO ()
batch s xs = forM_ (group s xs)
  where
    -- TODO: Use split package?
    -- https://stackoverflow.com/questions/12876384/grouping-a-list-into-lists-of-n-elements-in-haskell
    group :: Int -> [a] -> [[a]]
    group _ [] = []
    group n l
      | n > 0 = (take n l) : (group n (drop n l))
      | otherwise = error "Negative or zero n"

moveInstantaneousRewardsTo
    :: Tracer IO ClusterLog
    -> CardanoNodeConn
    -> FilePath
    -> [(XPub, Coin)]
    -> IO ()
moveInstantaneousRewardsTo tr conn dir targets = do
    certs <- mapM (mkVerificationKey >=> mkMIRCertificate) targets
    (faucetInput, faucetPrv) <- takeFaucet
    let file = dir </> "mir-tx.raw"

    let total = fromIntegral $ sum $ map (unCoin . snd) targets
    let totalDeposit = fromIntegral (length targets) * depositAmt
    when (total > faucetAmt) $ error "moveInstantaneousRewardsTo: too much to pay"

    sink <- genSinkAddress tr dir Nothing

    cli tr $
        [ "transaction", "build-raw"
        , "--tx-in", faucetInput
        , "--ttl", "600"
        , "--fee", show (faucetAmt - 1_000_000 - totalDeposit)
        , "--tx-out", sink <> "+" <> "1000000"
        , "--out-file", file
        ] ++ concatMap (\x -> ["--certificate-file", x]) (mconcat certs)

    testData <- getShelleyTestDataPath
    let bftPrv = testData </> "bft-leader" <> ".skey"

    tx <- signTx tr dir file [faucetPrv, bftPrv]
    submitTx tr conn "MIR certificates" tx
  where
    mkVerificationKey
        :: (XPub, Coin)
        -> IO (String, FilePath, Coin)
    mkVerificationKey (xpub, reward) = do
        let base16 = T.unpack $ T.decodeUtf8 $ hex $ xpubPublicKey xpub
        let json = Aeson.object
                [ "type" .= Aeson.String "StakeVerificationKeyShelley_ed25519"
                , "description" .= Aeson.String "Stake Verification Key"
                , "cborHex" .= Aeson.String ("5820" <> T.pack base16)
                ]
        let file = dir </> base16 <> ".vk"
        BL8.writeFile file (Aeson.encode json)
        pure (base16, file, reward)

    mkMIRCertificate
        :: (String, FilePath, Coin)
        -> IO [FilePath]
    mkMIRCertificate (pub, stakeVK, Coin reward) = do
        let mirCert = dir </> pub <> ".mir"
        stakeCert <- issueStakeCert tr dir pub stakeVK
        stakeAddr <- cliLine tr
            [ "stake-address"
            , "build"
            , "--mainnet"
            , "--stake-verification-key-file" , stakeVK
            ]

        cli tr
            [ "governance", "create-mir-certificate"
            , "--reserves"
            , "--reward", show reward
            , "--stake-address", stakeAddr
            , "--out-file", mirCert
            ]
        pure [stakeCert, mirCert]

-- | Generate a raw transaction. We kill two birds one stone here by also
-- automatically delegating 'pledge' amount to the given stake key.
prepareKeyRegistration
    :: Tracer IO ClusterLog
    -> FilePath
    -> IO (FilePath, FilePath)
prepareKeyRegistration tr dir = do
    let file = dir </> "tx.raw"

    let stakePub = dir </> "pre-registered-stake.pub"
    Aeson.encodeFile stakePub preRegisteredStakeKey

    (faucetInput, faucetPrv) <- takeFaucet

    cert <- issueStakeCert tr dir "pre-registered" stakePub
    sink <- genSinkAddress tr dir Nothing

    cli tr
        [ "transaction", "build-raw"
        , "--tx-in", faucetInput
        , "--tx-out", sink <> "+" <> "1000000"
        , "--ttl", "400"
        , "--fee", show (faucetAmt - depositAmt - 1_000_000)
        , "--certificate-file", cert
        , "--out-file", file
        ]
    pure (file, faucetPrv)

genSinkAddress
    :: Tracer IO ClusterLog
    -> FilePath -- ^ Directory to put keys
    -> Maybe FilePath -- ^ Stake pub
    -> IO String
genSinkAddress tr dir stakePub = do
    let sinkPrv = dir </> "sink.prv"
    let sinkPub = dir </> "sink.pub"
    cli tr
        [ "address", "key-gen"
        , "--signing-key-file", sinkPrv
        , "--verification-key-file", sinkPub
        ]
    cliLine tr $
        [ "address", "build"
        , "--mainnet"
        , "--payment-verification-key-file", sinkPub
        ] ++ maybe [] (\key -> ["--stake-verification-key-file", key]) stakePub

-- | Sign a transaction with all the necessary signatures.
signTx
    :: Tracer IO ClusterLog
    -> FilePath -- ^ Output directory
    -> FilePath -- ^ Tx body file
    -> [FilePath] -- ^ Signing keys for witnesses
    -> IO FilePath
signTx tr dir rawTx keys = do
    let file = dir </> "tx.signed"
    cli tr $
        [ "transaction", "sign"
        , "--tx-body-file", rawTx
        , "--mainnet"
        , "--out-file", file
        ]
        ++ concatMap (\key -> ["--signing-key-file", key]) keys
    pure file

-- | Submit a transaction through a running node.
submitTx :: Tracer IO ClusterLog -> CardanoNodeConn -> String -> FilePath -> IO ()
submitTx tr conn name signedTx =
    cliRetry tr ("Submitting transaction for " <> T.pack name) =<<
        cliConfigNode tr conn
        [ "transaction", "submit"
        , "--tx-file", signedTx
        , "--mainnet", "--cardano-mode"
        ]

-- | Wait for a command which depends on connecting to the given socket path to
-- succeed.
--
-- It retries every second, for up to 30 seconds. An exception is thrown if
-- it has waited for too long.
waitForSocket :: Tracer IO ClusterLog -> CardanoNodeConn -> IO ()
waitForSocket tr conn = do
    let msg = "Checking for usable socket file " <> toText conn
    -- TODO: check whether querying the tip works just as well.
    cliRetry tr msg =<< cliConfigNode tr conn
        ["query", "tip"
        , "--mainnet"
        --, "--testnet-magic", "764824073"
        , "--cardano-mode"
        ]
    traceWith tr $ MsgSocketIsReady conn

-- | Wait until a stake pool shows as registered on-chain.
waitUntilRegistered :: Tracer IO ClusterLog -> CardanoNodeConn -> String -> FilePath -> IO ()
waitUntilRegistered tr conn name opPub = do
    poolId <- fmap getFirstLine . readProcessStdout_ =<< cliConfig tr
        [ "stake-pool", "id"
        , "--stake-pool-verification-key-file", opPub
        ]
    (exitCode, distribution, err) <- cliNode tr conn
        [ "query", "stake-distribution"
        , "--mainnet"
        ]
    traceWith tr $ MsgStakeDistribution name exitCode distribution err
    unless (BL8.toStrict poolId `BS.isInfixOf` BL8.toStrict distribution) $ do
        threadDelay 5000000
        waitUntilRegistered tr conn name opPub


-- | Hard-wired faucets referenced in the genesis file. Purpose is simply to
-- fund some initial transaction for the cluster. Faucet have plenty of money to
-- pay for certificates and are intended for a one-time usage in a single
-- transaction.
takeFaucet :: IO (String, String)
takeFaucet = do
    i <- modifyMVar faucetIndex (\i -> pure (i+1, i))
    source <- getShelleyTestDataPath
    let basename = source </> "faucet-addrs" </> "faucet" <> show i
    base58Addr <- BS.readFile $ basename <> ".addr"
    let addr = fromMaybe (error $ "decodeBase58 failed for " ++ show base58Addr)
            . decodeBase58 bitcoinAlphabet
            . T.encodeUtf8
            . T.strip
            $ T.decodeUtf8 base58Addr

    let txin = B8.unpack (hex $ blake2b256 addr) <> "#0"
    let signingKey = basename <> ".shelley.key"
    pure (txin, signingKey)

-- | List of faucets also referenced in the shelley 'genesis.yaml'
faucetIndex :: MVar Int
faucetIndex = unsafePerformIO $ newMVar 1
{-# NOINLINE faucetIndex #-}

operators :: MVar [(PoolId, Aeson.Value, Aeson.Value, Aeson.Value, Aeson.Value)]
operators = unsafePerformIO $ newMVar operatorsFixture
{-# NOINLINE operators #-}

operatorsFixture :: [(PoolId, Aeson.Value, Aeson.Value, Aeson.Value, Aeson.Value)]
operatorsFixture =
    [ ( PoolId $ unsafeFromHex
          "ec28f33dcbe6d6400a1e5e339bd0647c0973ca6c0cf9c2bbe6838dc6"
      , Aeson.object
          [ "type" .= Aeson.String "StakePoolVerificationKey_ed25519"
          , "description" .= Aeson.String "Stake pool operator key"
          , "cborHex" .= Aeson.String
              "5820a12804d805eff46c691da5b11eb703cbf7463983e325621b41ac5b24e4b51887"
          ]
      , Aeson.object
          [ "type" .= Aeson.String "StakePoolSigningKey_ed25519"
          , "description" .= Aeson.String "Stake pool operator key"
          , "cborHex" .= Aeson.String
              "5820d8f81c455ef786f47ad9f573e49dc417e0125dfa8db986d6c0ddc03be8634dc6"
          ]
      , Aeson.object
          [ "type" .= Aeson.String "NodeOperationalCertificateIssueCounter"
          , "description" .= Aeson.String "Next certificate issue number: 0"
          , "cborHex" .= Aeson.String
              "82005820a12804d805eff46c691da5b11eb703cbf7463983e325621b41ac5b24e4b51887"
          ]
      , Aeson.object
          [ "name" .= Aeson.String "Genesis Pool A"
          , "ticker" .= Aeson.String "GPA"
          , "description" .= Aeson.Null
          , "homepage" .= Aeson.String "https://iohk.io"
          ]
      )
    , ( PoolId $ unsafeFromHex
          "1b3dc19c6ab89eaffc8501f375bb03c11bf8ed5d183736b1d80413d6"
      , Aeson.object
          [ "type" .= Aeson.String "StakePoolVerificationKey_ed25519"
          , "description" .= Aeson.String "Stake pool operator key"
          , "cborHex" .= Aeson.String
              "5820109440baecebefd92e3b933b4a717dae8d3291edee85f27ebac1f40f945ad9d4"
          ]
      , Aeson.object
          [ "type" .= Aeson.String "StakePoolSigningKey_ed25519"
          , "description" .= Aeson.String "Stake pool operator key"
          , "cborHex" .= Aeson.String
              "5820fab9d94c52b3e222ed494f84020a29ef8405228d509a924106d05ed01c923547"
          ]
      , Aeson.object
          [ "type" .= Aeson.String "NodeOperationalCertificateIssueCounter"
          , "description" .= Aeson.String "Next certificate issue number: 0"
          , "cborHex" .= Aeson.String
              "82005820109440baecebefd92e3b933b4a717dae8d3291edee85f27ebac1f40f945ad9d4"
          ]
      , Aeson.object
          [ "name" .= Aeson.String "Genesis Pool B"
          , "ticker" .= Aeson.String "GPB"
          , "description" .= Aeson.Null
          , "homepage" .= Aeson.String "https://iohk.io"
          ]
      )
    , ( PoolId $ unsafeFromHex
          "b45768c1a2da4bd13ebcaa1ea51408eda31dcc21765ccbd407cda9f2"
      , Aeson.object
          [ "type" .= Aeson.String "StakePoolVerificationKey_ed25519"
          , "description" .= Aeson.String "Stake pool operator key"
          , "cborHex" .= Aeson.String
              "5820c7383d89aa33656464a7796b06616c4590d6db018b2f73640be985794db0702d"
          ]
      , Aeson.object
          [ "type" .= Aeson.String "StakePoolSigningKey_ed25519"
          , "description" .= Aeson.String "Stake pool operator key"
          , "cborHex" .= Aeson.String
              "5820047572e48be93834d6d7ddb01bb1ad889b4de5a7a1a78112f1edd46284250869"
          ]
      , Aeson.object
          [ "type" .= Aeson.String "NodeOperationalCertificateIssueCounter"
          , "description" .= Aeson.String "Next certificate issue number: 0"
          , "cborHex" .= Aeson.String
              "82005820c7383d89aa33656464a7796b06616c4590d6db018b2f73640be985794db0702d"
          ]
      , Aeson.object
          [ "name" .= Aeson.String "Genesis Pool C"
          , "ticker" .= Aeson.String "GPC"
          , "description" .= Aeson.String "Lorem Ipsum Dolor Sit Amet."
          , "homepage" .= Aeson.String "https://iohk.io"
          ]
      )
    , ( PoolId $ unsafeFromHex
          "bb114cb37d75fa05260328c235a3dae295a33d0ba674a5eb1e3e568e"
      , Aeson.object
          [ "type" .= Aeson.String "StakePoolVerificationKey_ed25519"
          , "description" .= Aeson.String "Stake Pool Operator Verification Key"
          , "cborHex" .= Aeson.String
              "58203263e07605b9fc0100eb520d317f472ae12989fbf27fc71f46310bc0f24f2970"
          ]
      , Aeson.object
          [ "type" .= Aeson.String "StakePoolSigningKey_ed25519"
          , "description" .= Aeson.String "Stake Pool Operator Signing Key"
          , "cborHex" .= Aeson.String
              "58208f50de27d74325eaf57767d70277210b31eb97cdc3033f632a9791a3677a64d2"
          ]
      , Aeson.object
          [ "type" .= Aeson.String "NodeOperationalCertificateIssueCounter"
          , "description" .= Aeson.String "Next certificate issue number: 0"
          , "cborHex" .= Aeson.String
              "820058203263e07605b9fc0100eb520d317f472ae12989fbf27fc71f46310bc0f24f2970"
          ]
      , Aeson.object
          [ "name" .= Aeson.String "Genesis Pool D"
          , "ticker" .= Aeson.String "GPD"
          , "description" .= Aeson.String "Lorem Ipsum Dolor Sit Amet."
          , "homepage" .= Aeson.String "https://iohk.io"
          ]
      )
    ]

-- | Allow running the test cluster a second time in the same process.
resetGlobals :: IO ()
resetGlobals = do
    void $ swapMVar faucetIndex 1
    void $ swapMVar operators operatorsFixture

getClusterEra :: FilePath -> IO ClusterEra
getClusterEra dir = read <$> readFile (dir </> "era")

putClusterEra :: FilePath -> ClusterEra -> IO ()
putClusterEra dir = writeFile (dir </> "era") . show

-- | A public stake key associated with a mnemonic that we pre-registered for
-- STAKE_POOLS_JOIN_05.
--
-- ["over", "decorate", "flock", "badge", "beauty"
-- , "stamp", "chest", "owner", "excess", "omit"
-- , "bid", "raccoon", "spin", "reduce", "rival"
-- ]
preRegisteredStakeKey
    :: Aeson.Value
preRegisteredStakeKey = Aeson.object
    [ "type" .= Aeson.String "StakeVerificationKeyShelley_ed25519"
    , "description" .= Aeson.String "Free form text"
    , "cborHex" .= Aeson.String
        "5820949fc9e6b7e1e12e933ac35de5a565c9264b0ac5b631b4f5a21548bc6d65616f"
    ]

-- | Deposit amount required for registering certificates.
depositAmt :: Integer
depositAmt = 1000000

-- | Initial amount in each of these special cluster faucet
faucetAmt :: Integer
faucetAmt = 1000 * oneMillionAda

-- | Just one million Ada, in Lovelace.
oneMillionAda :: Integer
oneMillionAda = 1_000_000_000_000

-- | Add a "systemStart" field in a given object with the current POSIX time as a
-- value.
updateSystemStart
    :: UTCTime
    -> Aeson.Object
    -> Aeson.Object
updateSystemStart systemStart =
    HM.insert "systemStart" (toJSON systemStart)

-- | Add a @setupScribes[1].scMinSev@ field in a given config object.
-- The full lens library would be quite helpful here.
addMinSeverityStdout
    :: MonadFail m
    => Severity
    -> Aeson.Object
    -> m Aeson.Object
addMinSeverityStdout severity ob = case HM.lookup "setupScribes" ob of
    Just (Aeson.Array scribes) -> do
        let scribes' = Aeson.Array $ fmap setMinSev scribes
        pure $ HM.insert "setupScribes" scribes' ob
    _ -> fail "setupScribes logging config is missing or the wrong type"
  where
    sev = toJSON $ show severity
    setMinSev (Aeson.Object scribe)
        | HM.lookup "scKind" scribe == Just (Aeson.String "StdoutSK")
            = Aeson.Object (HM.insert "scMinSev" sev scribe)
        | otherwise = Aeson.Object scribe
    setMinSev a = a

-- | Transform initial funds back to a big object instead of a list of
-- singletons.
transformInitialFunds
    :: Aeson.Object
    -> IO Aeson.Object
transformInitialFunds = pure . HM.update toObject "nonAvvmBalances"
  where
    toObject = \case
        Aeson.Array xs ->
            pure $ Aeson.Object $ HM.fromList (singleton <$> V.toList xs)
        _ ->
            error "transformInitialFunds: expected initialFunds to be an array."
    singleton = \case
        Aeson.Object obj ->
            head $ HM.toList obj
        _ ->
            error "transformInitialFunds: expected initialFunds to be many singletons"

-- | Do something with an a JSON object. Fails if the given JSON value isn't an
-- object.
withObject
    :: MonadFail m
    => (Aeson.Object -> m Aeson.Object)
    -> Aeson.Value
    -> m Aeson.Value
withObject action = \case
    Aeson.Object m -> Aeson.Object <$> action m
    _ -> fail
        "withObject: was given an invalid JSON. Expected an Object but got \
        \something else."

-- | Little helper to run an action within a certain delay. Fails if the action
-- takes too long.
timeout :: Int -> (String, IO a) -> IO a
timeout t (title, action) = do
    race (threadDelay $ t * 1000000) action >>= \case
        Left _  -> fail ("Waited too long for: " <> title)
        Right a -> pure a

-- | Hash a ByteString using blake2b_256 and encode it in base16
blake2b256S :: ByteString -> String
blake2b256S =
    T.unpack
    . T.decodeUtf8
    . convertToBase Base16
    . blake2b256

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data ClusterLog
    = MsgRegisteringStakePools Int -- ^ How many pools
    | MsgStartingCluster FilePath
    | MsgLauncher String LauncherLog
    | MsgStartedStaticServer String FilePath
    | MsgTempDir TempDirLog
    | MsgBracket Text BracketLog
    | MsgCLIStatus Text ExitCode BL8.ByteString BL8.ByteString
    | MsgCLIRetry Text
    | MsgCLIRetryResult Text Int BL8.ByteString
    | MsgSocketIsReady CardanoNodeConn
    | MsgStakeDistribution String ExitCode BL8.ByteString BL8.ByteString
    | MsgDebug Text
    | MsgGenOperatorKeyPair FilePath
    | MsgCLI [String]
    deriving (Show)

instance ToText ClusterLog where
    toText = \case
        MsgStartingCluster dir ->
            "Configuring cluster in " <> T.pack dir
        MsgRegisteringStakePools 0 -> mconcat
                [ "Not registering any stake pools due to "
                , "NO_POOLS=1. Some tests may fail."
                ]
        MsgRegisteringStakePools n -> mconcat
                [ T.pack (show n)
                , " stake pools are being registered on chain... "
                , "Can be skipped using NO_POOLS=1."
                ]
        MsgLauncher name msg ->
            T.pack name <> " " <> toText msg
        MsgStartedStaticServer baseUrl fp ->
            "Started a static server for " <> T.pack fp
                <> " at " <> T.pack baseUrl
        MsgTempDir msg -> toText msg
        MsgBracket name b -> name <> ": " <> toText b
        MsgCLIStatus msg st out err -> case st of
            ExitSuccess -> "Successfully finished " <> msg
            ExitFailure code -> "Failed " <> msg <> " with exit code " <>
                T.pack (show code)  <> ":\n" <> indent out <> "\n" <> indent err
        MsgCLIRetry msg -> msg
        MsgCLIRetryResult msg code err ->
            "Failed " <> msg <> " with exit code " <>
                T.pack (show code) <> ":\n" <> indent err
        MsgSocketIsReady conn ->
            toText conn <> " is ready."
        MsgStakeDistribution name st out err -> case st of
            ExitSuccess ->
                "Stake distribution query for " <> T.pack name <>
                ":\n" <> indent out
            ExitFailure code ->
                "Query of stake-distribution failed with status " <>
                T.pack (show code) <> ":\n" <> indent err
        MsgDebug msg -> msg
        MsgGenOperatorKeyPair dir ->
            "Generating stake pool operator key pair in " <> T.pack dir
        MsgCLI args -> T.pack $ unwords ("cardano-cli":args)
      where
        indent = T.unlines . map ("  " <>) . T.lines . T.decodeUtf8With T.lenientDecode . BL8.toStrict

instance HasPrivacyAnnotation ClusterLog
instance HasSeverityAnnotation ClusterLog where
    getSeverityAnnotation = \case
        MsgStartingCluster _ -> Notice
        MsgRegisteringStakePools _ -> Notice
        MsgLauncher _ _ -> Info
        MsgStartedStaticServer _ _ -> Info
        MsgTempDir msg -> getSeverityAnnotation msg
        MsgBracket _ _ -> Debug
        MsgCLIStatus _ ExitSuccess _ _-> Debug
        MsgCLIStatus _ (ExitFailure _) _ _-> Error
        MsgCLIRetry _ -> Info
        MsgCLIRetryResult{} -> Info
        -- NOTE: ^ Some failures are expected, so for cleaner logs we use Info,
        -- instead of Warning.
        MsgSocketIsReady _ -> Info
        MsgStakeDistribution _ ExitSuccess _ _-> Info
        MsgStakeDistribution _ (ExitFailure _) _ _-> Info
        -- NOTE: ^ Some failures are expected, so for cleaner logs we use Info,
        -- instead of Warning.
        MsgDebug _ -> Debug
        MsgGenOperatorKeyPair _ -> Debug
        MsgCLI _ -> Debug

bracketTracer' :: Tracer IO ClusterLog -> Text -> IO a -> IO a
bracketTracer' tr name = bracketTracer (contramap (MsgBracket name) tr)
