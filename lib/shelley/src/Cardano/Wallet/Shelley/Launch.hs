{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- NOTE Temporary until we can fully enable the cluster
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides a function to launch cardano-node for /testing/.

module Cardano.Wallet.Shelley.Launch
    ( -- * Integration Launcher
      withCluster
    , withBFTNode
    , withStakePool
    , withSMASH
    , NodeParams (..)
    , singleNodeParams
    , PoolConfig (..)
    , defaultPoolConfigs
    , poolConfigsFromEnv
    , RunningNode (..)

      -- * Configuration
    , minSeverityFromEnv
    , nodeMinSeverityFromEnv
    , walletMinSeverityFromEnv
    , testMinSeverityFromEnv
    , testLogDirFromEnv
    , walletListenFromEnv

      -- * Faucets
    , sendFaucetFundsTo
    , moveInstantaneousRewardsTo
    , oneMillionAda

    -- * Network
    , NetworkConfiguration (..)
    , nodeSocketOption
    , networkConfigurationOption
    , parseGenesisData

      -- * Utils
    , withSystemTempDir
    , withTempDir
    , isEnvSet
    , envFromText
    , lookupEnvNonEmpty

    -- * global vars
    , operators

    -- * Logging
    , ClusterLog (..)
    , TempDirLog (..)
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPub, xpubPublicKey )
import Cardano.Api.Shelley.Genesis
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
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..), nullTracer )
import Cardano.Chain.Genesis
    ( GenesisData (..), readGenesisData )
import Cardano.CLI
    ( optionT, parseLoggingSeverity )
import Cardano.Launcher
    ( LauncherLog, ProcessHasExited (..) )
import Cardano.Launcher.Node
    ( CardanoNodeConfig (..)
    , CardanoNodeConn (..)
    , NodePort (..)
    , withCardanoNode
    )
import Cardano.Ledger.Shelley
    ( ShelleyEra )
import Cardano.Pool.Metadata
    ( SMASHPoolId (..) )
import Cardano.Startup
    ( restrictFileMode )
import Cardano.Wallet.Api.Server
    ( Listen (..) )
import Cardano.Wallet.Api.Types
    ( HealthStatusSMASH (..) )
import Cardano.Wallet.Logging
    ( BracketLog (..), bracketTracer )
import Cardano.Wallet.Network.Ports
    ( randomUnusedTCPPorts )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..), hex )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter
    , currentRelativeTime
    , hoistTimeInterpreter
    , interpretQuery
    , mkTimeInterpreter
    , timeUntilEpoch
    )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , EpochLength (unEpochLength)
    , EpochNo (..)
    , NetworkParameters (..)
    , PoolId (..)
    , ProtocolMagic (..)
    , SlotLength (..)
    , SlottingParameters (..)
    , StartTime (..)
    , stabilityWindowByron
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxOut )
import Cardano.Wallet.Shelley
    ( SomeNetworkDiscriminant (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( NodeVersionData, StandardShelley )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex, unsafeRunExceptT )
import Control.Arrow
    ( first, second )
import Control.Monad
    ( forM, forM_, replicateM, replicateM_, unless, void, when, (>=>) )
import Control.Monad.Fail
    ( MonadFail )
import Control.Monad.IO.Unlift
    ( MonadUnliftIO, liftIO )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT, withExceptT )
import Control.Retry
    ( constantDelay, limitRetriesByCumulativeDelay, retrying )
import Control.Tracer
    ( Tracer (..), contramap, traceWith )
import Crypto.Hash.Utils
    ( blake2b256 )
import Data.Aeson
    ( FromJSON (..), toJSON, (.:), (.=) )
import Data.ByteArray.Encoding
    ( Base (..), convertToBase )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58 )
import Data.Either
    ( fromRight, isLeft, isRight )
import Data.Fixed
    ( Micro )
import Data.Function
    ( (&) )
import Data.Functor
    ( ($>), (<&>) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.List
    ( isInfixOf, isPrefixOf, nub, permutations, sort )
import Data.Maybe
    ( catMaybes, fromMaybe, isJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( getQuantity )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError, ToText (..) )
import Data.Time.Clock
    ( NominalDiffTime, UTCTime, addUTCTime, getCurrentTime )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime, utcTimeToPOSIXSeconds )
import Fmt
    ( fmt, secondsF )
import GHC.TypeLits
    ( KnownNat, Nat, SomeNat (..), someNatVal )
import Options.Applicative
    ( Parser, eitherReader, flag', help, long, metavar, option, (<|>) )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( RelativeTime (..), mkSlotLength )
import Ouroboros.Consensus.Config.SecurityParam
    ( SecurityParam (..) )
import Ouroboros.Consensus.Shelley.Node
    ( sgNetworkMagic )
import Ouroboros.Consensus.Util.Counting
    ( exactlyTwo )
import Ouroboros.Network.Magic
    ( NetworkMagic (..) )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersion (..)
    , NodeToClientVersionData (..)
    , nodeToClientCodecCBORTerm
    )
import System.Directory
    ( copyFile, createDirectory, createDirectoryIfMissing, makeAbsolute )
import System.Environment
    ( lookupEnv, setEnv )
import System.Exit
    ( ExitCode (..), die )
import System.FilePath
    ( isValid, (</>) )
import System.Info
    ( os )
import System.IO.Temp
    ( createTempDirectory, getCanonicalTemporaryDirectory )
import System.IO.Unsafe
    ( unsafePerformIO )
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
    ( SomeException, finally, handle, throwIO )
import UnliftIO.MVar
    ( MVar, modifyMVar, newMVar, putMVar, readMVar, takeMVar )
import UnliftIO.Process
    ( readProcess, readProcessWithExitCode )
import UnliftIO.Temporary
    ( withTempDirectory )

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Legacy
import qualified Cardano.Slotting.Slot as Cardano
import qualified Cardano.Wallet.Byron.Compatibility as Byron
import qualified Cardano.Wallet.Primitive.Types as W
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
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import qualified Ouroboros.Consensus.HardFork.History.EraParams as HF
import qualified Ouroboros.Consensus.HardFork.History.Qry as HF
import qualified Ouroboros.Consensus.HardFork.History.Summary as HF

-- | Shelley hard fork network configuration has two genesis datas.
-- As a special case for mainnet, we hardcode the byron genesis data.
data NetworkConfiguration where
    -- | Mainnet does not have network discrimination.
    MainnetConfig
        :: NetworkConfiguration

    -- | Testnet has network magic.
    TestnetConfig
        :: FilePath
        -- ^ Genesis data in JSON format, for byron era.
        -> NetworkConfiguration

    -- | Staging does not have network discrimination.
    StagingConfig
        :: FilePath
        -- ^ Genesis data in JSON format, for byron era.
        -> NetworkConfiguration
  deriving (Show, Eq)

-- | --node-socket=FILE
nodeSocketOption :: Parser FilePath
nodeSocketOption = option (eitherReader check) $ mempty
    <> long "node-socket"
    <> metavar (if isWindows then "PIPENAME" else "FILE")
    <> help helpText
  where
    check :: String -> Either String FilePath
    check name
        | isWindows = if isValidWindowsPipeName name
            then Right name
            else Left ("Invalid pipe name. " ++ pipeHelp)
        | otherwise = if isValid name
            then Right name
            else Left "Invalid file path"

    helpText = mconcat
        [ "Path to the node's domain socket file (POSIX) "
        , "or pipe name (Windows). "
        , "Note: Maximum length for POSIX socket files is approx. 100 bytes. "
        , "Note: ", pipeHelp ]
    pipeHelp = "Windows named pipes are of the form \\\\.\\pipe\\cardano-node"

isWindows :: Bool
isWindows = os == "mingw32"

isValidWindowsPipeName :: FilePath -> Bool
isValidWindowsPipeName name = slashPipe `isPrefixOf` name
    && isValid (drop (length slashPipe) name)
  where
    slashPipe = "\\\\.\\pipe\\"

-- | --mainnet --shelley-genesis=FILE
-- --testnet --byron-genesis=FILE --shelley-genesis=FILE
-- --staging --byron-genesis=FILE --shelley-genesis=FILE
networkConfigurationOption :: Parser NetworkConfiguration
networkConfigurationOption = mainnet <|> testnet <|> staging
  where
    mainnet = mainnetFlag
    testnet = TestnetConfig <$> genesisFileOption "byron" "testnet"
    staging = StagingConfig <$> genesisFileOption "byron" "staging"

    mainnetFlag = flag' MainnetConfig $ mempty
        <> long "mainnet"
        <> help "Use Cardano mainnet protocol"

    genesisFileOption :: String -> String -> Parser FilePath
    genesisFileOption era net = optionT $ mempty
        <> long net
        <> metavar "FILE"
        <> help ("Path to the " <> era <> " genesis data in JSON format.")

someCustomDiscriminant
    :: (forall (pm :: Nat). KnownNat pm => Proxy pm -> SomeNetworkDiscriminant)
    -> ProtocolMagic
    -> (SomeNetworkDiscriminant, NodeVersionData)
someCustomDiscriminant mkSomeNetwork pm@(ProtocolMagic n) =
    case someNatVal (fromIntegral n) of
        Just (SomeNat proxy) ->
            ( mkSomeNetwork proxy
            , Shelley.testnetVersionData pm
            )
        _ -> error "networkDiscriminantFlag: failed to convert \
            \ProtocolMagic to SomeNat."

parseGenesisData
    :: NetworkConfiguration
    -> ExceptT String IO
        (SomeNetworkDiscriminant, NetworkParameters, NodeVersionData, Block)
parseGenesisData = \case
    MainnetConfig -> do
        let nm = NetworkMagic $ fromIntegral $ W.getProtocolMagic W.mainnetMagic
        let mainnetVersionData =
                ( NodeToClientVersionData nm
                , nodeToClientCodecCBORTerm NodeToClientV_5
                )
        pure
            ( SomeNetworkDiscriminant $ Proxy @'Mainnet
            , Byron.mainnetNetworkParameters
            , mainnetVersionData
            , Byron.emptyGenesis (genesisParameters Byron.mainnetNetworkParameters)
            )

    TestnetConfig byronGenesisFile -> do
        (genesisData, genesisHash) <-
            withExceptT show $ readGenesisData byronGenesisFile

        let mkSomeNetwork
                :: forall (pm :: Nat). KnownNat pm
                => Proxy pm
                -> SomeNetworkDiscriminant
            mkSomeNetwork _ = SomeNetworkDiscriminant $ Proxy @('Testnet pm)


        let pm = Byron.fromProtocolMagicId $ gdProtocolMagicId genesisData
        let (discriminant, vData) = someCustomDiscriminant mkSomeNetwork pm
        let (np, outs) = Byron.fromGenesisData (genesisData, genesisHash)
        let block0 = Byron.genesisBlockFromTxOuts (genesisParameters np) outs

        pure
            ( discriminant
            , np
            , vData
            , block0
            )

    StagingConfig byronGenesisFile -> do
        (genesisData, genesisHash) <-
            withExceptT show $ readGenesisData byronGenesisFile

        let mkSomeNetwork
                :: forall (pm :: Nat). KnownNat pm
                => Proxy pm
                -> SomeNetworkDiscriminant
            mkSomeNetwork _ = SomeNetworkDiscriminant $ Proxy @('Staging pm)

        let pm = Byron.fromProtocolMagicId $ gdProtocolMagicId genesisData
        let (discriminant, vData) = someCustomDiscriminant mkSomeNetwork pm
        let (np, outs) = Byron.fromGenesisData (genesisData, genesisHash)
        let block0 = Byron.genesisBlockFromTxOuts (genesisParameters np) outs

        pure
            ( discriminant
            , np
            , vData
            , block0
            )

-- | Returns the shelley test data path, which is usually relative to the
-- package sources, but can be overridden by the @SHELLEY_TEST_DATA@ environment
-- variable.
getShelleyTestDataPath :: IO FilePath
getShelleyTestDataPath = fromMaybe source <$> lookupEnvNonEmpty var
  where
    source = $(getTestData) </> "cardano-node-shelley"
    var = "SHELLEY_TEST_DATA"

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
    minSeverityFromEnv Critical "CARDANO_WALLET_TRACING_MIN_SEVERITY"

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

instance FromJSON PreserveInitialFundsOrdering where
    parseJSON source = do
        initialFunds <- flip (Aeson.withObject "ByronGenesis") source $ \obj ->
            obj .: "nonAvvmBalances"
        let outs = mconcat (Map.toList <$> initialFunds)
                & map (first unsafeMkAddress)
                & map (second unsafeMkLovelace)
                & map (Byron.fromTxOut . uncurry Legacy.TxOut)
        pure $ PreserveInitialFundsOrdering outs
      where
        unsafeMkAddress =
            either bomb id . Byron.decodeAddressBase58
          where
            bomb = error "PreserveInitialFundsOrdering: address not valid base58"

        unsafeMkLovelace =
            either bomb id . Byron.mkLovelace . read
          where
            bomb = error "PreserveInitialFundsOrdering: invalid lovelace value"

--------------------------------------------------------------------------------
-- For Integration
--------------------------------------------------------------------------------

-- | A quick helper to interact with the 'cardano-cli'. Assumes the cardano-cli
-- is available in PATH.
cli :: Tracer IO ClusterLog -> [String] -> IO String
cli tr args = do
    traceWith tr $ MsgCLI args
    readProcess "cardano-cli" args stdin
  where
    stdin = ""

-- | Runs a @cardano-cli@ command and retries for up to 30 seconds if the
-- command failed.
--
-- Assumes @cardano-cli@ is available in @PATH@ and that the env var
-- @CARDANO_NODE_SOCKET_PATH@ has already been set.
cliRetry
    :: Tracer IO ClusterLog
    -> String -- ^ message to print before running command
    -> [String] -- ^ arguments to @cardano-cli@
    -> IO String
cliRetry tr msg args = do
    (st, out, err) <- retrying pol (const isFail) (const cmd)
    traceWith tr $ MsgCLIStatus msg st out err
    case st of
        ExitSuccess -> pure out
        ExitFailure _ -> throwIO $ ProcessHasExited
            (unwords (prog:args) <> " failed: " <> err) st
  where
    prog = "cardano-cli"
    cmd = do
        traceWith tr $ MsgCLIRetry msg
        (st, out, err) <- readProcessWithExitCode "cardano-cli" args mempty
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

data RunningNode = RunningNode
    FilePath
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
    -- ^ Trace for subprocess control logging
    -> Severity
    -- ^ Minimum logging severity for @cardano-node@
    -> [PoolConfig]
    -- ^ The configurations of pools to spawn.
    -> FilePath
    -- ^ Parent state directory for cluster
    -> Maybe (FilePath, Severity)
    -> (RunningNode -> IO ())
    -- ^ Action to run when the Byron BFT node is up
    -> (RunningNode -> IO ())
    -- ^ Action to run when we have transitioned to the Shelley era.
    --
    -- Can be used to transfer byron faucet funds to shelley faucets.
    -> (RunningNode -> IO a)
    -- ^ Action to run when we have transitioned to the Allegra era and
    -- stake pools are running
    -> IO a
withCluster tr severity poolConfigs dir logFile onByron onShelley onClusterStart =
    bracketTracer' tr "withCluster" $ do
        traceWith tr $ MsgStartingCluster dir
        let poolCount = length poolConfigs
        (port0:ports) <- randomUnusedTCPPorts (poolCount + 2)
        systemStart <- addUTCTime 1 <$> getCurrentTime
        let bftCfg = NodeParams severity systemStart (head $ rotate ports) logFile
        withBFTNode tr dir bftCfg $ \bftSocket block0 params -> do
            let runningBftNode = RunningNode bftSocket block0 params
            waitForSocket tr bftSocket *> onByron runningBftNode
            ti <- timeInterpreterFromTestingConfig (fst params) dir
            waitForEpoch 1 "Shelley" tr ti
            onShelley runningBftNode

            setEnv "CARDANO_NODE_SOCKET_PATH" bftSocket
            (rawTx, faucetPrv) <- prepareKeyRegistration tr dir
            tx <- signTx tr dir rawTx [faucetPrv]
            submitTx tr "pre-registered stake key" tx

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
            asyncs <- forM (zip3 [0..] poolConfigs $ tail $ rotate ports) $
                \(idx, poolConfig, (port, peers)) -> do
                    async (handle onException $ do
                        let spCfg =
                                NodeParams severity systemStart (port, peers) logFile
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
                waitForEpoch 3 "Allegra" tr ti
                let cfg = NodeParams severity systemStart (port0, ports) logFile
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

-- | Configuration parameters which update the @node.config@ test data file.
data NodeParams = NodeParams
    { minSeverity :: Severity
      -- ^ Minimum logging severity
    , systemStart :: UTCTime
      -- ^ Genesis block start time
    , nodePeers :: (Int, [Int])
      -- ^ A list of ports used by peers and this node
    , extraLogFile :: Maybe (FilePath, Severity)
      -- ^ The node will always log to "cardano-node.log" relative to the
      -- config. This option can be set for an additional output.
    } deriving (Show)

singleNodeParams :: Severity -> Maybe (FilePath, Severity) -> IO NodeParams
singleNodeParams severity extraLogFile = do
    systemStart <- getCurrentTime
    pure $ NodeParams severity systemStart (0, []) extraLogFile

withBFTNode
    :: Tracer IO ClusterLog
    -- ^ Trace for subprocess control logging
    -> FilePath
    -- ^ Parent state directory. Node data will be created in a subdirectory of
    -- this.
    -> NodeParams
    -- ^ Parameters used to generate config files.
    -> (FilePath -> Block -> (NetworkParameters, NodeVersionData) -> IO a)
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

        let extraLogFile = (fmap (first (</> (name ++ ".log"))) logDir)
        (config, block0, networkParams, versionData)
            <- genConfig dir severity extraLogFile systemStart
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

        withCardanoNodeProcess tr name cfg $ \(CardanoNodeConn socket) -> do
            action socket block0 (networkParams, versionData)
  where
    name = "bft"
    dir = baseDir </> name
    NodeParams severity systemStart (port, peers) logDir = params

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
    -> (FilePath -> IO a)
    -- ^ Callback function with socket path
    -> IO a
withRelayNode tr baseDir (NodeParams severity systemStart (port, peers) logDir) act =
    bracketTracer' tr "withRelayNode" $ do
        createDirectory dir

        let extraLogFile = (fmap (first (</> (name ++ ".log"))) logDir)
        (config, _, _, _) <-
            genConfig dir severity extraLogFile systemStart
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

        withCardanoNodeProcess tr name cfg $ \(CardanoNodeConn socket) ->
            act socket
  where
    name = "node"
    dir = baseDir </> name

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
    let NodeParams severity systemStart (port, peers) logDir = params

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

    let extraLogFile = (fmap (first (</> (name ++ ".log"))) logDir)
    (config, _, _, _) <- genConfig dir severity extraLogFile systemStart
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
            withCardanoNodeProcess tr name cfg $ \_ -> do
                submitTx tr name tx
                timeout 120
                    ("pool registration", waitUntilRegistered tr name opPub)
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
    pools <- readMVar operators
    forM_ pools $ \(poolId, _, _, _, metadata) -> do
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

updateVersion :: Tracer IO ClusterLog -> FilePath -> IO ()
updateVersion tr tmpDir = do
    let updatePath = tmpDir </> "update-proposal"
    let votePath = tmpDir </> "update-vote"
    let network = "--mainnet"
    source <- getShelleyTestDataPath
    void $ cli tr
        [ "byron", "create-update-proposal"
        , "--filepath", updatePath
        , network
        , "--signing-key", source </> "bft-leader.byron.skey"
        , "--protocol-version-major", "1"
        , "--protocol-version-minor", "0"
        , "--protocol-version-alt", "0"
        , "--application-name", "cardano-sl"
        , "--software-version-num", "1"
        , "--system-tag", "linux"
        , "--installer-hash", "0"
        ]
    void $ cli tr
        [ "byron", "create-proposal-vote"
        , "--proposal-filepath", updatePath
        , network
        , "--signing-key", source </> "bft-leader.byron.skey"
        , "--vote-yes"
        , "--output-filepath", votePath
        ]

    void $ cli tr
        [ "byron", "submit-update-proposal"
        , network
        , "--filepath", updatePath
        ]
    void $ cli tr
        [ "byron", "submit-proposal-vote"
        , network
        , "--filepath", votePath
        ]

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

genConfig
    :: FilePath
    -- ^ A top-level directory where to put the configuration.
    -> Severity
    -- ^ Minimum severity level for logging
    -> Maybe (FilePath, Severity)
    -- ^ Optional /extra/ logging output
    -> UTCTime
    -- ^ Genesis block start time
    -> IO (FilePath, Block, NetworkParameters, NodeVersionData)
genConfig dir severity mExtraLogFile systemStart = do
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

    let scribes = catMaybes
            [ Just $ fileScribe ("cardano-node.log", severity)
            , fileScribe . first T.pack <$> mExtraLogFile
            ]

    ----
    -- Configuration
    Yaml.decodeFileThrow (source </> "node.config")
        >>= withAddedKey "ShelleyGenesisFile" shelleyGenesisFile
        >>= withAddedKey "ByronGenesisFile" byronGenesisFile
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
            , nodeToClientCodecCBORTerm NodeToClientV_5
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

    byronGenesisFile :: FilePath
    byronGenesisFile = dir </> "byron-genesis.json"

    withScribes scribes =
        withAddedKey "setupScribes" scribes
        >=> withAddedKey "defaultScribes"
            (map (\s -> [toJSON $ scKind s, toJSON $ scName s]) scribes)

    -- we need to specify genesis file location every run in tmp
    withAddedKey k v = withObject (pure . HM.insert k (toJSON v))


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
    void $ cli tr
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
    void $ cli tr
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
    void $ cli tr
        [ "stake-address", "key-gen"
        , "--verification-key-file", stakePub
        , "--signing-key-file", stakePrv
        ]
    pure (stakePrv, stakePub)

-- | Issue a node operational certificate
issueOpCert :: Tracer IO ClusterLog -> FilePath -> FilePath -> FilePath -> FilePath -> IO FilePath
issueOpCert tr dir kesPub opPrv opCount = do
    let file = dir </> "op.cert"
    void $ cli tr
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
    void $ cli tr
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
        void $ cli tr
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
    void $ cli tr
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
    void $ cli tr
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
    let sinkPrv = dir </> "sink.prv"
    let sinkPub = dir </> "sink.pub"
    void $ cli tr
        [ "address", "key-gen"
        , "--signing-key-file", sinkPrv
        , "--verification-key-file", sinkPub
        ]
    addr <- cli tr
        [ "address", "build"
        , "--payment-verification-key-file", sinkPub
        , "--stake-verification-key-file", stakePub
        , "--mainnet"
        ]

    (faucetInput, faucetPrv) <- takeFaucet
    void $ cli tr $
        [ "transaction", "build-raw"
        , "--tx-in", faucetInput
        , "--tx-out", init addr <> "+" <> show pledgeAmt
        , "--ttl", "400"
        , "--fee", show (faucetAmt - pledgeAmt - depositAmt)
        , "--out-file", file
        ] ++ mconcat ((\cert -> ["--certificate-file",cert]) <$> certs)

    pure (file, faucetPrv)

sendFaucetFundsTo
    :: Tracer IO ClusterLog
    -> FilePath
    -> [(String, Coin)]
    -> IO ()
sendFaucetFundsTo tr dir allTargets = do
    forM_ (group 80 allTargets) sendBatch
  where
    sendBatch targets = do
        (faucetInput, faucetPrv) <- takeFaucet
        let file = dir </> "faucet-tx.raw"
        let outputs = flip concatMap targets $ \(addr, (Coin c)) ->
                ["--tx-out", addr <> "+" <> show c]

        let total = fromIntegral $ sum $ map (unCoin . snd) targets
        when (total > faucetAmt) $ error "sendFaucetFundsTo: too much to pay"

        void $ cli tr $
            [ "transaction", "build-raw"
            , "--tx-in", faucetInput
            , "--ttl", "600"
            , "--fee", show (faucetAmt - total)
            , "--out-file", file
            ] ++ outputs

        tx <- signTx tr dir file [faucetPrv]
        submitTx tr "faucet tx" tx

    -- TODO: Use split package?
    -- https://stackoverflow.com/questions/12876384/grouping-a-list-into-lists-of-n-elements-in-haskell
    group :: Int -> [a] -> [[a]]
    group _ [] = []
    group n l
      | n > 0 = (take n l) : (group n (drop n l))
      | otherwise = error "Negative or zero n"

moveInstantaneousRewardsTo
    :: Tracer IO ClusterLog
    -> FilePath
    -> [(XPub, Coin)]
    -> IO ()
moveInstantaneousRewardsTo tr dir targets = do
    certs <- mapM (mkVerificationKey >=> mkMIRCertificate) targets
    (faucetInput, faucetPrv) <- takeFaucet
    let file = dir </> "mir-tx.raw"

    let total = fromIntegral $ sum $ map (unCoin . snd) targets
    let totalDeposit = fromIntegral (length targets) * depositAmt
    when (total > faucetAmt) $ error "moveInstantaneousRewardsTo: too much to pay"

    sink <- genSinkAddress tr dir

    void $ cli tr $
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
    submitTx tr "MIR certificates" tx
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
        void $ cli tr
            [ "governance", "create-mir-certificate"
            , "--reserves"
            , "--reward", show reward
            , "--stake-verification-key-file", stakeVK
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
    sink <- genSinkAddress tr dir

    void $ cli tr
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
    -> FilePath
    -> IO String
genSinkAddress tr dir = do
    let sinkPrv = dir </> "sink.prv"
    let sinkPub = dir </> "sink.pub"
    void $ cli tr
        [ "address", "key-gen"
        , "--signing-key-file", sinkPrv
        , "--verification-key-file", sinkPub
        ]
    addr <- cli tr
        [ "address", "build"
        , "--payment-verification-key-file", sinkPub
        , "--mainnet"
        ]
    pure (init addr)

-- | Sign a transaction with all the necessary signatures.
signTx
    :: Tracer IO ClusterLog
    -> FilePath
    -> FilePath
    -> [FilePath]
    -> IO FilePath
signTx tr dir rawTx keys = do
    let file = dir </> "tx.signed"
    void $ cli tr $
        [ "transaction", "sign"
        , "--tx-body-file", rawTx
        , "--mainnet"
        , "--out-file", file
        ] ++ mconcat ((\key -> ["--signing-key-file", key]) <$> keys)
    pure file

-- | Submit a transaction through a running node.
submitTx :: Tracer IO ClusterLog -> String -> FilePath -> IO ()
submitTx tr name signedTx = do
    void $ cliRetry tr ("Submitting transaction for " ++ name)
        [ "transaction", "submit"
        , "--tx-file", signedTx
        , "--mainnet", "--cardano-mode"
        ]

-- | Wait for a command which depends on connecting to the given socket path to
-- succeed.
--
-- It retries every second, for up to 30 seconds. An exception is thrown if
-- it has waited for too long.
--
-- As a side effect, after this subroutine finishes, the environment variable
-- @CARDANO_NODE_SOCKET_PATH@ is set.
waitForSocket :: Tracer IO ClusterLog -> FilePath -> IO ()
waitForSocket tr socketPath = do
    setEnv "CARDANO_NODE_SOCKET_PATH" socketPath
    let msg = "Checking for usable socket file " <> socketPath
    -- TODO: check whether querying the tip works just as well.
    void $ cliRetry tr msg
        ["query", "tip"
        , "--mainnet"
        --, "--testnet-magic", "764824073"
        , "--cardano-mode"
        ]
    traceWith tr $ MsgSocketIsReady socketPath

-- | Wait until a stake pool shows as registered on-chain.
waitUntilRegistered :: Tracer IO ClusterLog -> String -> FilePath -> IO ()
waitUntilRegistered tr name opPub = do
    poolId <- init <$> cli tr
        [ "stake-pool", "id"
        , "--stake-pool-verification-key-file", opPub
        ]
    (exitCode, distribution, err) <- readProcessWithExitCode "cardano-cli"
        [ "query", "stake-distribution"
        , "--mainnet"
        , "--cardano-mode"
        ] mempty
    traceWith tr $ MsgStakeDistribution name exitCode distribution err
    unless (poolId `isInfixOf` distribution) $ do
        threadDelay 5000000
        waitUntilRegistered tr name opPub


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
operators = unsafePerformIO $ newMVar
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
{-# NOINLINE operators #-}

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
    :: Monad m
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

-- | Create a temporary directory and remove it after the given IO action has
-- finished -- unless the @NO_CLEANUP@ environment variable has been set.
withTempDir
    :: MonadUnliftIO m
    => Tracer m TempDirLog
    -> FilePath -- ^ Parent directory
    -> String -- ^ Directory name template
    -> (FilePath -> m a) -- ^ Callback that can use the directory
    -> m a
withTempDir tr parent name action = isEnvSet "NO_CLEANUP" >>= \case
    True -> do
        dir <- liftIO $ createTempDirectory parent name
        let tr' = contramap (MsgNoCleanup dir) tr
        bracketTracer tr' $ action dir
    False -> withTempDirectory parent name action

withSystemTempDir
    :: MonadUnliftIO m
    => Tracer m TempDirLog
    -> String   -- ^ Directory name template
    -> (FilePath -> m a) -- ^ Callback that can use the directory
    -> m a
withSystemTempDir tr name action = do
    parent <- liftIO getCanonicalTemporaryDirectory
    withTempDir tr parent name action

{-------------------------------------------------------------------------------
                                     Utils
-------------------------------------------------------------------------------}

-- | Looks up an environment variable, treating variables which are defined but
-- empty the same as variables which are undefined.
lookupEnvNonEmpty :: MonadUnliftIO m => String -> m (Maybe String)
lookupEnvNonEmpty = liftIO . fmap nonEmpty . lookupEnv
  where
    nonEmpty (Just "") = Nothing
    nonEmpty m = m

-- | Returns true iff an environment variable is defined and non-empty.
isEnvSet :: MonadUnliftIO m => String -> m Bool
isEnvSet = fmap isJust . lookupEnvNonEmpty

-- | Parses an environment variable using text-class.
envFromText
    :: (MonadUnliftIO m, FromText a)
    => String
    -> m (Maybe (Either TextDecodingError a))
envFromText = liftIO . fmap (fmap (fromText . T.pack)) . lookupEnv

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data ClusterLog
    = MsgRegisteringStakePools Int -- ^ How many pools
    | MsgWaitingForEpoch NominalDiffTime EpochNo Text
    | MsgStartingCluster FilePath
    | MsgLauncher String LauncherLog
    | MsgStartedStaticServer String FilePath
    | MsgTempDir TempDirLog
    | MsgBracket Text BracketLog
    | MsgCLIStatus String ExitCode String String
    | MsgCLIRetry String
    | MsgCLIRetryResult String Int String
    | MsgSocketIsReady FilePath
    | MsgStakeDistribution String ExitCode String String
    | MsgDebug Text
    | MsgGenOperatorKeyPair FilePath
    | MsgCLI [String]
    deriving (Show)

instance ToText ClusterLog where
    toText = \case
        MsgStartingCluster dir ->
            "Configuring cluster in " <> T.pack dir
        MsgWaitingForEpoch dt (EpochNo e) thing -> mconcat
            [ "Waiting "
            , fmt (secondsF 1 dt) <> "s"
            , " to reach "
            , thing
            , " at epoch "
            , T.pack (show e)
            , "..."
            ]
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
            ExitSuccess -> "Successfully finished " <> T.pack msg
            ExitFailure code -> "Failed " <> T.pack msg <> " with exit code " <>
                T.pack (show code)  <> ":\n" <> indent out <> "\n" <> indent err
        MsgCLIRetry msg -> T.pack msg
        MsgCLIRetryResult msg code err ->
            "Failed " <> T.pack msg <> " with exit code " <>
                T.pack (show code) <> ":\n" <> indent err
        MsgSocketIsReady socketPath ->
            T.pack socketPath <> " is ready."
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
        indent = T.unlines . map ("  " <>) . T.lines . T.pack

instance HasPrivacyAnnotation ClusterLog
instance HasSeverityAnnotation ClusterLog where
    getSeverityAnnotation = \case
        MsgStartingCluster _ -> Notice
        MsgRegisteringStakePools _ -> Notice
        MsgWaitingForEpoch{} -> Notice
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

data TempDirLog = MsgNoCleanup FilePath BracketLog deriving (Show)

instance ToText TempDirLog where
    toText = \case
        MsgNoCleanup _ BracketStart -> ""
        MsgNoCleanup dir _ -> "NO_CLEANUP of temporary directory " <> T.pack dir

instance HasPrivacyAnnotation TempDirLog
instance HasSeverityAnnotation TempDirLog where
    getSeverityAnnotation = \case
        MsgNoCleanup _ BracketStart -> Debug
        MsgNoCleanup _ _ -> Notice

bracketTracer' :: Tracer IO ClusterLog -> Text -> IO a -> IO a
bracketTracer' tr name = bracketTracer (contramap (MsgBracket name) tr)

-- | Wait until the given epoch starts.
waitForEpoch
    :: EpochNo -- ^ Epoch (start) to wait for.
    -> Text -- ^ Thing we're waiting for (for log message).
    -> Tracer IO ClusterLog
    -> TimeInterpreter Identity
    -> IO ()
waitForEpoch e thing tr ti = do
    now <- currentRelativeTime ti
    let delta = runIdentity $ interpretQuery ti (timeUntilEpoch e now)
    traceWith tr $ MsgWaitingForEpoch delta e thing
    threadDelay . fromIntegral . fromEnum $ realToFrac @_ @Micro delta

-- | Return a TimeInterpreter based on the genesis files in the cluster setup
-- directory.
--
-- Assumes era parameters are changed at the shelley fork at epoch 1, but that
-- they are not changed further.
--
-- The purpose is to concentrate the assumions about test cluster slotting to
-- one place, and allow us existing @TimeInterpreter@ queries with it.
timeInterpreterFromTestingConfig
    :: NetworkParameters  -- ^ Genesis parameters
    -> FilePath -- ^ Config dir
    -> IO (TimeInterpreter Identity)
timeInterpreterFromTestingConfig genesisParams dir = do
    (shelleyGenesis :: ShelleyGenesis (ShelleyEra Shelley.StandardCrypto))
        <- either fail pure
        =<< Aeson.eitherDecodeFileStrict
        (dir </> "bft" </> "shelley-genesis.json")

    let startTime = StartTime $ sgSystemStart shelleyGenesis

    let sp = slottingParameters genesisParams
    let byronSl = unSlotLength $ getSlotLength sp
    let byronEl = getEpochLength sp
    let byronSlotLength = unSlotLength $ getSlotLength sp
    let byronK = SecurityParam $ getQuantity $ stabilityWindowByron sp
    let byronParams = HF.defaultEraParams byronK (mkSlotLength byronSl)

    let shelleyEl = sgEpochLength shelleyGenesis
    let shelleyK = SecurityParam $ sgSecurityParam shelleyGenesis
    let shelleySl = sgSlotLength shelleyGenesis
    let shelleyParams = (HF.defaultEraParams shelleyK (mkSlotLength shelleySl))
            { HF.eraEpochSize = shelleyEl }

    let shelleyFork = HF.Bound
            (RelativeTime
                (fromRational (toRational $ unEpochLength byronEl)
                * byronSlotLength))
            (W.SlotNo $ fromIntegral $ unEpochLength byronEl)
            (Cardano.EpochNo 1)

    let summary = HF.summaryWithExactly $ exactlyTwo
             (HF.EraSummary HF.initBound (HF.EraEnd shelleyFork) byronParams)
             (HF.EraSummary shelleyFork (HF.EraUnbounded) shelleyParams)

    return $ hoistTimeInterpreter neverFails' $ mkTimeInterpreter
        nullTracer
        startTime
        (pure @Identity $ HF.mkInterpreter summary)
  where
    neverFails' =
        fmap (fromRight (error "timeInterpreterFromTestingConfig shouldn't fail"))
        . runExceptT
