{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Integration.Framework.Setup where

import Prelude

import Cardano.Address
    ( NetworkTag (..)
    )
import Cardano.Address.Style.Shelley
    ( shelleyTestnet
    )
import Cardano.BM.Data.Severity
    ( Severity (..)
    )
import Cardano.BM.Extra
    ( bracketTracer
    , stdoutTextTracer
    )
import Cardano.CLI
    ( Port (..)
    , getEKGURL
    , getPrometheusURL
    )
import Cardano.Launcher
    ( ProcessHasExited (..)
    )
import Cardano.Launcher.Node
    ( CardanoNodeConn
    )
import Cardano.Ledger.Shelley.API
    ( ShelleyGenesis (..)
    )
import Cardano.Mnemonic
    ( SomeMnemonic (..)
    )
import Cardano.Startup
    ( installSignalHandlersNoLogging
    , setDefaultFilePermissions
    )
import Cardano.Wallet.Api.Http.Shelley.Server
    ( walletListenFromEnv
    )
import Cardano.Wallet.Api.Types
    ( ApiEra (..)
    )
import Cardano.Wallet.Faucet
    ( FaucetM
    , runFaucetM
    )
import Cardano.Wallet.Launch.Cluster
    ( ClusterEra (..)
    , FaucetFunds (..)
    , FileOf (..)
    , LogFileConfig (..)
    , RunningNode (..)
    , clusterEraFromEnv
    , runClusterM
    , sendFaucetAssetsTo
    , withCluster
    , withFaucet
    , withSMASH
    )
import Cardano.Wallet.Launch.Cluster.ClusterEra
    ( nodeOutputFileFromEnv
    )
import Cardano.Wallet.Network.Implementation.Ouroboros
    ( tunedForMainnetPipeliningStrategy
    )
import Cardano.Wallet.Network.Ports
    ( portFromURL
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( fromGenesisData
    )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkId (..)
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance (..)
    )
import Cardano.Wallet.Primitive.Types
    ( NetworkParameters
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Shelley
    ( Tracers
    , serveWallet
    )
import Cardano.Wallet.Shelley.BlockchainSource
    ( BlockchainSource (..)
    )
import Cardano.Wallet.TokenMetadata.MockServer
    ( queryServerStatic
    , withMetadataServer
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Tracer
    ( Tracer (..)
    , contramap
    , traceWith
    )
import Data.Either.Combinators
    ( whenLeft
    )
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText (..)
    )
import Main.Utf8
    ( withUtf8
    )
import Network.HTTP.Client
    ( Manager
    , defaultManagerSettings
    , managerResponseTimeout
    , newManager
    , responseTimeoutMicro
    )
import Network.URI
    ( URI
    )
import Servant.Client
    ( ClientEnv
    )
import System.Directory
    ( createDirectory
    )
import System.Environment
    ( setEnv
    )
import System.Environment.Extended
    ( envFromText
    , isEnvSet
    )
import System.Exit
    ( ExitCode
    )
import System.FilePath
    ( (</>)
    )
import System.IO.Temp.Extra
    ( SkipCleanup (..)
    , withSystemTempDir
    )
import Test.Integration.Framework.Context
    ( Context (..)
    , PoolGarbageCollectionEvent (..)
    )
import Test.Integration.Framework.Logging
    ( TestsLog (..)
    , withTracers
    )
import UnliftIO.Async
    ( race
    )
import UnliftIO.Exception
    ( throwIO
    , withException
    )
import UnliftIO.MVar
    ( MVar
    , newEmptyMVar
    , newMVar
    , putMVar
    , takeMVar
    , withMVar
    )

import qualified Cardano.Address.Style.Shelley as Shelley
import qualified Cardano.CLI as CLI
import qualified Cardano.Faucet.Addresses as Addresses
import qualified Cardano.Pool.DB as Pool
import qualified Cardano.Pool.DB.Layer as Pool
import qualified Cardano.Wallet.Faucet as Faucet
import qualified Cardano.Wallet.Launch.Cluster as Cluster
import qualified Data.Text as T

-- | Do all the program setup required for integration tests, create a temporary
-- directory, and pass this info to the main hspec action.
withTestsSetup :: (FilePath -> (Tracer IO TestsLog, Tracers IO) -> IO a) -> IO a
withTestsSetup action = do
    -- Handle SIGTERM properly
    installSignalHandlersNoLogging
    -- Stop cardano-cli complaining about file permissions
    setDefaultFilePermissions
    -- Enables small test-specific workarounds, like timing out faster if wallet
    -- deletion fails.
    setEnv "CARDANO_WALLET_TEST_INTEGRATION" "1"

    skipCleanup <- SkipCleanup <$> isEnvSet "NO_CLEANUP"
    -- Flush test output as soon as a line is printed.
    -- Set UTF-8, regardless of user locale.
    withUtf8
        $
        -- This temporary directory will contain logs, and all other data
        -- produced by the integration tests.
        withSystemTempDir stdoutTextTracer "test" skipCleanup
        $ \testDir ->
            withTracers testDir $ action testDir

mkFaucetFunds :: Cluster.TestnetMagic -> FaucetM FaucetFunds
mkFaucetFunds testnetMagic = do
    let networkTag =
            NetworkTag . fromIntegral
                $ Cluster.testnetMagicToNatural testnetMagic
    shelleyFunds <- Faucet.shelleyFunds shelleyTestnet
    byronFunds <- Faucet.byronFunds networkTag
    icarusFunds <- Faucet.icarusFunds networkTag
    onlyDustWallet <- Faucet.onlyDustWallet shelleyTestnet
    bigDustWallet <- Faucet.bigDustWallet shelleyTestnet
    preregKeyWallet <- Faucet.preregKeyWallet shelleyTestnet
    instantaneousRewardFunds <- Faucet.mirFunds shelleyTestnet
    massiveWallet <- Faucet.massiveWalletFunds (Coin 0) 0 shelleyTestnet
    maryAllegraFunds <-
        Faucet.maryAllegraFunds
            (Coin 10__000_000)
            shelleyTestnet

    mirCredentials <- do
        mnemonics <- Faucet.mirMnemonics
        let oneMioAda = Coin 1_000_000__000_000
            mkRewardAccountCred (SomeMnemonic m) =
                let (xpub, _prv) = Addresses.shelleyRewardAccount m
                in  KeyCredential (Shelley.getKey xpub)
        pure [(mkRewardAccountCred m, oneMioAda) | m <- mnemonics]

    pure
        FaucetFunds
            { pureAdaFunds =
                mconcat
                    [ shelleyFunds
                    , byronFunds
                    , Faucet.byronIntegrationTestFunds networkTag
                    , Faucet.hwLedgerFunds networkTag
                    , icarusFunds
                    , onlyDustWallet
                    , bigDustWallet
                    , preregKeyWallet
                    , instantaneousRewardFunds
                    ]
            , maryAllegraFunds
            , mirCredentials
            , massiveWalletFunds = massiveWallet
            }

data TestingCtx = TestingCtx
    { testnetMagic :: Cluster.TestnetMagic
    , testDir :: FilePath
    , tr :: Tracer IO TestsLog
    , tracers :: Tracers IO
    , localClusterEra :: ClusterEra
    , testDataDir :: FileOf "test-data"
    }

-- A decorator for the pool database that records all calls to the
-- 'removeRetiredPools' operation.
--
-- The parameters and return value of each call are recorded by appending
-- a 'PoolGarbageCollectionEvent' value to the start of the given log.
--

recordPoolGarbageCollectionEvents
    :: TestingCtx
    -> IORef [PoolGarbageCollectionEvent]
    -> Pool.DBDecorator m
recordPoolGarbageCollectionEvents TestingCtx{..} eventsRef =
    Pool.DBDecorator decorate
  where
    decorate Pool.DBLayer{..} =
        Pool.DBLayer{removeRetiredPools = removeRetiredPoolsDecorated, ..}
      where
        removeRetiredPoolsDecorated epochNo = do
            certificates <- removeRetiredPools epochNo
            let event = PoolGarbageCollectionEvent epochNo certificates
            liftIO $ do
                traceWith tr $ MsgPoolGarbageCollectionEvent event
                atomicModifyIORef' eventsRef ((,()) . (event :))
            pure certificates

withServer
    :: TestingCtx
    -> FileOf "cluster-configs"
    -> FaucetFunds
    -> Pool.DBDecorator IO
    -> Maybe (FileOf "node-output")
    -> ( T.Text
         -> CardanoNodeConn
         -> NetworkParameters
         -> URI
         -> IO ()
       )
    -> IO ExitCode
withServer
    ctx@TestingCtx{..}
    clusterConfigs
    faucetFunds
    dbDecorator
    nodeOutputFile
    onReady =
        bracketTracer' tr "withServer" $ do
            let tr' = contramap MsgCluster tr
            era <- clusterEraFromEnv
            withSMASH tr' testDir $ \smashUrl -> do
                let clusterConfig =
                        Cluster.Config
                            { cfgStakePools = Cluster.defaultPoolConfigs
                            , cfgLastHardFork = era
                            , cfgNodeLogging = LogFileConfig Info Nothing Info
                            , cfgClusterDir = FileOf @"cluster" testDir
                            , cfgClusterConfigs = clusterConfigs
                            , cfgTestnetMagic = testnetMagic
                            , cfgShelleyGenesisMods = []
                            , cfgTracer = tr'
                            , cfgNodeOutputFile = nodeOutputFile
                            }
                withCluster clusterConfig faucetFunds
                    $ onClusterStart
                        ctx
                        (onReady (T.pack smashUrl))
                        dbDecorator

onClusterStart
    :: TestingCtx
    -> (CardanoNodeConn -> NetworkParameters -> URI -> IO ())
    -> Pool.DBDecorator IO
    -> RunningNode
    -> IO ExitCode
onClusterStart
    TestingCtx{..}
    callback
    dbDecorator
    (RunningNode nodeConnection genesisData vData) = do
        let (networkParameters, block0, genesisPools) =
                fromGenesisData genesisData
        let db = testDir </> "wallets"
        createDirectory db
        listen <- walletListenFromEnv envFromText
        let testMetadata = pathOf testDataDir </> "token-metadata.json"
        withMetadataServer (queryServerStatic testMetadata) $ \tokenMetaUrl -> do
            serveWallet
                (NodeSource nodeConnection vData (SyncTolerance 10))
                networkParameters
                tunedForMainnetPipeliningStrategy
                (NTestnet (fromIntegral (sgNetworkMagic genesisData)))
                genesisPools
                tracers
                (Just db)
                (Just dbDecorator)
                "127.0.0.1"
                listen
                Nothing
                Nothing
                (Just tokenMetaUrl)
                block0
                (callback nodeConnection networkParameters)
                `withException` (traceWith tr . MsgServerError)

-- threadDelay $ 3 * 60 * 1_000_000 -- Wait 3 minutes for the node to start
-- exitSuccess

-- | Convert @ClusterEra@ to a @ApiEra@.
clusterToApiEra :: ClusterEra -> ApiEra
clusterToApiEra = \case
    BabbageHardFork -> ApiBabbage
    ConwayHardFork -> ApiConway

httpManager :: IO Manager
httpManager = do
    let fiveMinutes = 300 * 1_000 * 1_000 -- 5 min in microseconds
    newManager
        $ defaultManagerSettings
            { managerResponseTimeout =
                responseTimeoutMicro fiveMinutes
            }

setupContext
    :: TestingCtx
    -> MVar Context
    -> ClientEnv
    -> IORef [PoolGarbageCollectionEvent]
    -> Maybe (FileOf "node-output")
    -> T.Text
    -> CardanoNodeConn
    -> NetworkParameters
    -> URI
    -> IO ()
setupContext
    TestingCtx{..}
    ctx
    faucetClientEnv
    poolGarbageCollectionEvents
    nodeOutputFile
    smashUrl
    nodeConnection
    networkParameters
    baseUrl =
        bracketTracer' tr "setupContext" $ do
            clusterConfigs <- Cluster.localClusterConfigsFromEnv
            faucet <- Faucet.initFaucet faucetClientEnv
            let tr' = contramap MsgCluster tr
            prometheusUrl <-
                let packPort (h, p) =
                        T.pack h <> ":" <> toText @(Port "Prometheus") p
                 in maybe "none" packPort <$> getPrometheusURL
            ekgUrl <-
                let packPort (h, p) =
                        T.pack h <> ":" <> toText @(Port "EKG") p
                 in maybe "none" packPort <$> getEKGURL
            traceWith tr $ MsgBaseUrl baseUrl ekgUrl prometheusUrl smashUrl
            manager <- httpManager
            mintSeaHorseAssetsLock <- newMVar ()

            let withConfig =
                    runClusterM
                        $ Cluster.Config
                            { cfgStakePools = error "cfgStakePools: unused"
                            , cfgLastHardFork = localClusterEra
                            , cfgNodeLogging = error "cfgNodeLogging: unused"
                            , cfgClusterDir = FileOf @"cluster" testDir
                            , cfgClusterConfigs = clusterConfigs
                            , cfgTestnetMagic = testnetMagic
                            , cfgShelleyGenesisMods = []
                            , cfgTracer = tr'
                            , cfgNodeOutputFile = nodeOutputFile
                            }

            putMVar
                ctx
                Context
                    { _cleanup = pure ()
                    , _manager = (baseUrl, manager)
                    , _walletPort = CLI.Port . fromIntegral $ portFromURL baseUrl
                    , _faucet = faucet
                    , _networkParameters = networkParameters
                    , _testnetMagic = testnetMagic
                    , _poolGarbageCollectionEvents = poolGarbageCollectionEvents
                    , _mainEra = clusterToApiEra localClusterEra
                    , _smashUrl = smashUrl
                    , _mintSeaHorseAssets = \nPerAddr batchSize c addrs ->
                        withMVar mintSeaHorseAssetsLock $ \() ->
                            withConfig
                                $ sendFaucetAssetsTo
                                    nodeConnection
                                    batchSize
                                    (Faucet.seaHorseTestAssets nPerAddr c addrs)
                    }

withContext :: TestingCtx -> (Context -> IO ()) -> IO ()
withContext testingCtx@TestingCtx{..} action = do
    bracketTracer' tr "withContext" $ withFaucet $ \faucetClientEnv -> do
        ctx <- newEmptyMVar
        nodeOutputFile <- nodeOutputFileFromEnv
        clusterConfigs <- Cluster.localClusterConfigsFromEnv
        poolGarbageCollectionEvents <- newIORef []
        faucetFunds <- runFaucetM faucetClientEnv $ mkFaucetFunds testnetMagic

        let dbEventRecorder =
                recordPoolGarbageCollectionEvents
                    testingCtx
                    poolGarbageCollectionEvents
            cluster =
                setupContext
                    testingCtx
                    ctx
                    faucetClientEnv
                    poolGarbageCollectionEvents
                    nodeOutputFile
        res <-
            race
                ( withServer
                    testingCtx
                    clusterConfigs
                    faucetFunds
                    dbEventRecorder
                    nodeOutputFile
                    cluster
                )
                (takeMVar ctx >>= bracketTracer' tr "spec" . action)
        whenLeft res (throwIO . ProcessHasExited "integration")

bracketTracer' :: Tracer IO TestsLog -> Text -> IO a -> IO a
bracketTracer' tr name = bracketTracer $ contramap (MsgBracket name) tr
