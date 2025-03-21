{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Prelude

import Cardano.Address.Style.Shelley
    ( shelleyTestnet
    )
import Cardano.BM.Data.Severity
    ( Severity (Error)
    )
import Cardano.BM.Data.Tracer
    ( filterSeverity
    )
import Cardano.BM.Extra
    ( stdoutTextTracer
    , trMessage
    )
import Cardano.BM.ToTextTracer
    ( ToTextTracer (..)
    , overToTextTracer
    , withToTextTracer
    )
import Cardano.BM.Trace
    ( traceInTVarIO
    )
import Cardano.Mnemonic
    ( SomeMnemonic
    )
import Cardano.Wallet.Api.Types
    ( AddressAmount (..)
    , ApiMnemonicT (..)
    , ApiT (..)
    , ApiTransaction
    , ApiTxId (..)
    , ApiWallet
    , ApiWalletMigrationPlanPostData (..)
    , ApiWalletMigrationPostData (..)
    , PostTransactionFeeOldData (..)
    , PostTransactionOldData (..)
    , WalletOrAccountPostData (..)
    , WalletPostData (..)
    )
import Cardano.Wallet.Api.Types.Amount
    ( ApiAmount (..)
    )
import Cardano.Wallet.Api.Types.Era
    ( ApiEra
    )
import Cardano.Wallet.Api.Types.WalletAssets
    ( ApiWalletAssets (..)
    )
import Cardano.Wallet.Application
    ( Tracers
    , Tracers' (..)
    , serveWallet
    )
import Cardano.Wallet.Application.CLI
    ( Port (..)
    )
import Cardano.Wallet.Application.Server
    ( Listen (ListenOnPort)
    )
import Cardano.Wallet.Benchmarks.Collect
    ( Benchmark (..)
    , Reporter (..)
    , Result (..)
    , Unit (Milliseconds)
    , mkSemantic
    , newReporterResourceTFromEnv
    , report
    )
import Cardano.Wallet.Benchmarks.Latency.BenchM
    ( BenchCtx (..)
    , BenchM
    , finallyDeleteWallet
    , fixtureMultiAssetWallet
    , fixtureWallet
    , fixtureWalletWith
    , request
    , requestWithError
    )
import Cardano.Wallet.Benchmarks.Latency.Measure
    ( meanAvg
    , withLatencyLogging
    )
import Cardano.Wallet.Faucet
    ( Faucet (massiveWalletMnemonic)
    )
import Cardano.Wallet.Launch.Cluster
    ( Config (..)
    , FaucetFunds (..)
    , RunningNode (..)
    , defaultPoolConfigs
    , testnetMagicToNatural
    , withCluster
    , withFaucet
    )
import Cardano.Wallet.Launch.Cluster.CommandLine
    ( clusterConfigsDirParser
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( OsNamedPipe (..)
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( DirOf (..)
    , FileOf (..)
    , mkRelDirOf
    , newAbsolutizer
    , toFilePath
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
    ( WalletId
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Shelley.BlockchainSource
    ( BlockchainSource (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromText
    )
import Control.Applicative
    ( (<**>)
    )
import Control.Concurrent
    ( threadDelay
    )
import Control.Monad
    ( replicateM
    , replicateM_
    , void
    )
import Control.Monad.Catch
    ( Exception
    , MonadThrow (..)
    )
import Control.Monad.Cont
    ( evalContT
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Monad.IO.Unlift
    ( toIO
    )
import Control.Monad.Reader
    ( MonadReader (..)
    , ReaderT (..)
    , lift
    )
import Data.Bifunctor
    ( bimap
    )
import Data.Functor.Contravariant
    ( (>$<)
    )
import Data.Generics.Internal.VL.Lens
    ( over
    , set
    , view
    , (^.)
    )
import Data.Generics.Labels
    ()
import Data.Generics.Wrapped
    ( _Unwrapped
    )
import Data.Time
    ( NominalDiffTime
    )
import Fmt
    ( Builder
    , build
    )
import Main.Utf8
    ( withUtf8
    )
import Network.HTTP.Client
    ( defaultManagerSettings
    , managerResponseTimeout
    , newManager
    , responseTimeoutMicro
    )
import Numeric.Natural
    ( Natural
    )
import Servant.Client
    ( ClientError
    , ClientM
    )
import System.Directory
    ( createDirectory
    )
import System.Environment.Extended
    ( isEnvSet
    )
import System.IO
    ( stdout
    )
import System.IO.Extra
    ( withTempFile
    )
import System.IO.Temp.Extra
    ( SkipCleanup (..)
    , withSystemTempDir
    )
import System.Path
    ( absDir
    , absFile
    , relDir
    , (</>)
    )
import Test.Integration.Framework.DSL
    ( Context (..)
    , eventually
    , faucetAmt
    , fixturePassphrase
    , minUTxOValue
    , pickAnAsset
    , runResourceT
    , shouldBe
    , utxoStatisticsFromCoins
    )
import UnliftIO.Async
    ( race_
    )
import UnliftIO.MVar
    ( newEmptyMVar
    , putMVar
    , takeMVar
    )

import qualified Cardano.Wallet.Api.Clients.Network as CN
import qualified Cardano.Wallet.Api.Clients.Testnet.Id as C
import qualified Cardano.Wallet.Api.Clients.Testnet.Shelley as C
import qualified Cardano.Wallet.Benchmarks.Latency.Measure as Measure
import qualified Cardano.Wallet.Faucet as Faucet
import qualified Cardano.Wallet.Launch.Cluster as Cluster
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Options.Applicative as O

main :: IO ()
main = withUtf8 $ evalContT $ do
    tr <- withToTextTracer (Left stdout) Nothing
    let ToTextTracer onlyErrors =
            overToTextTracer (filterSeverity (const $ pure Error)) tr
        setupTracers tvar =
            Tracers
                { apiServerTracer = trMessage $ snd >$< traceInTVarIO tvar
                , applicationTracer = onlyErrors
                , tokenMetadataTracer = onlyErrors
                , walletEngineTracer = onlyErrors
                , walletDbTracer = onlyErrors
                , poolsEngineTracer = onlyErrors
                , poolsDbTracer = onlyErrors
                , ntpClientTracer = onlyErrors
                , networkTracer = onlyErrors
                }
    (tracers, capture) <- withLatencyLogging setupTracers
    liftIO
        $ withShelleyServer tracers
        $ \massiveWalletMnemonic' ctx ->
            runReaderT (runResourceT $ walletApiBench tr massiveWalletMnemonic')
                $ BenchCtx ctx capture

-- Creates n fixture wallets and return 3 of them

walletApiBench :: ToTextTracer -> SomeMnemonic -> BenchM ()
walletApiBench (ToTextTracer tr) massiveMnemonic = do

    let semantic = mkSemantic ["latency"]
    reporter <- newReporterResourceTFromEnv tr semantic

    let runScenarioR semSeg scen = do
            let sem = mkSemantic [T.pack semSeg]
            runScenario (addSemantic reporter sem) scen

    fmtTitle "Non-cached run"
    runWarmUpScenario

    fmtTitle "Latencies for 2 fixture wallets scenarioR"
    runScenarioR "2-fixture" (nFixtureWallet 2)

    fmtTitle "Latencies for 10 fixture wallets scenarioR"
    runScenarioR "10-fixture" (nFixtureWallet 10)

    fmtTitle "Latencies for 100 fixture wallets"
    runScenarioR "100-fixture" (nFixtureWallet 100)

    fmtTitle "Latencies for 2 fixture wallets with 10 txs scenarioR"
    runScenarioR "2-fixture-10-txs" (nFixtureWalletWithTxs 2 10)

    fmtTitle "Latencies for 2 fixture wallets with 20 txs scenarioR"
    runScenarioR "2-fixture-20-txs" (nFixtureWalletWithTxs 2 20)

    fmtTitle "Latencies for 2 fixture wallets with 100 txs scenarioR"
    runScenarioR "2-fixture-100-txs" (nFixtureWalletWithTxs 2 100)

    fmtTitle "Latencies for 10 fixture wallets with 10 txs scenarioR"
    runScenarioR "10-fixture-10-txs" (nFixtureWalletWithTxs 10 10)

    fmtTitle "Latencies for 10 fixture wallets with 20 txs scenarioR"
    runScenarioR "10-fixture-20-txs" (nFixtureWalletWithTxs 10 20)

    fmtTitle "Latencies for 10 fixture wallets with 100 txs scenarioR"
    runScenarioR "10-fixture-100-txs" (nFixtureWalletWithTxs 10 100)

    fmtTitle "Latencies for 2 fixture wallets with 100 utxos scenarioR"
    runScenarioR "2-fixture-100-utxos" (nFixtureWalletWithUTxOs 2 100)

    fmtTitle "Latencies for 2 fixture wallets with 200 utxos scenarioR"
    runScenarioR "2-fixture-200-utxos" (nFixtureWalletWithUTxOs 2 200)

    fmtTitle "Latencies for 2 fixture wallets with 500 utxos scenarioR"
    runScenarioR "2-fixture-500-utxos" (nFixtureWalletWithUTxOs 2 500)

    fmtTitle "Latencies for 2 fixture wallets with 1000 utxos scenarioR"
    runScenarioR "2-fixture-1000-utxos" (nFixtureWalletWithUTxOs 2 1_000)

    fmtTitle
        $ "Latencies for 2 fixture wallets with "
            <> build massiveWalletUTxOSize
            <> " utxos scenario"
    runScenarioR "2-fixture-massive-utxos"
        $ massiveFixtureWallet massiveMnemonic

nFixtureWallet
    :: Int
    -> BenchM (ApiWallet, ApiWallet, ApiWallet, ApiWallet)
nFixtureWallet n = do
    wal1 : wal2 : _ <- replicateM n fixtureWallet
    walMA <- fixtureMultiAssetWallet
    maWalletToMigrate <- fixtureMultiAssetWallet
    pure (wal1, wal2, walMA, maWalletToMigrate)

-- Creates n fixture wallets and send 1-ada transactions to one of them
-- (m times). The money is sent in batches (see batchSize below) from
-- additionally created source fixture wallet. Then we wait for the money
-- to be accommodated in recipient wallet. After that the source fixture
-- wallet is removed.
nFixtureWalletWithTxs
    :: Int
    -> Int
    -> BenchM (ApiWallet, ApiWallet, ApiWallet, ApiWallet)
nFixtureWalletWithTxs n m = do
    (wal1, wal2, walMA, maWalletToMigrate) <- nFixtureWallet n

    let amt = minUTxOValue era
    let batchSize = 10
    let whole10Rounds = div m batchSize
    let lastBit = mod m batchSize
    let amtExp val = ((amt * fromIntegral val) + faucetAmt) :: Natural
    let expInflows =
            if whole10Rounds > 0
                then [x * batchSize | x <- [1 .. whole10Rounds]] ++ [lastBit]
                else [lastBit]
    let expInflows' = filter (/= 0) expInflows

    mapM_ (repeatPostTx wal1 amt batchSize . amtExp) expInflows'
    pure (wal1, wal2, walMA, maWalletToMigrate)

nFixtureWalletWithUTxOs
    :: Int
    -> Int
    -> BenchM (ApiWallet, ApiWallet, ApiWallet, ApiWallet)
nFixtureWalletWithUTxOs n utxoNumber = do
    let utxoExp = replicate utxoNumber (minUTxOValue era)
    wal1 <- fixtureWalletWith utxoExp
    (_, wal2, walMA, maWalletToMigrate) <- nFixtureWallet n

    eventually "Wallet balance is as expected" $ do
        rWal1 <- request $ C.getWallet (wal1 ^. #id)
        rWal1 ^. #balance . #available . #toNatural `shouldBe` sum utxoExp

    rStat <- request $ C.getWalletUtxoStatistics (wal1 ^. #id)
    utxoStatisticsFromCoins (fromIntegral <$> utxoExp) `shouldBe` rStat
    pure (wal1, wal2, walMA, maWalletToMigrate)

massiveFixtureWallet :: SomeMnemonic -> BenchM (ApiWallet, ApiWallet, ApiWallet, ApiWallet)
massiveFixtureWallet massiveMnemonic = do
    (_, wal2, walMA, maWalletToMigrate) <- nFixtureWallet 2

    wal1 <-
        request
            $ C.postWallet
            $ WalletOrAccountPostData
            $ Left
            $ WalletPostData
                { addressPoolGap = Just $ ApiT $ toEnum 10_001
                , mnemonicSentence = ApiMnemonicT massiveMnemonic
                , mnemonicSecondFactor = Nothing
                , name = ApiT $ unsafeFromText "Massive wallet"
                , passphrase = ApiT $ unsafeFromText fixturePassphrase
                , oneChangeAddressMode = Nothing
                , restorationMode = Nothing
                }
    finallyDeleteWallet wal1
    wal1
        ^. #balance
            . #available
            . #toNatural
            `shouldBe` (fromIntegral massiveWalletUTxOSize * unCoin massiveWalletAmt)

    pure (wal1, wal2, walMA, maWalletToMigrate)

repeatPostTx :: ApiWallet -> Natural -> Int -> Natural -> BenchM ()
repeatPostTx wDest amtToSend batchSize amtExp = do
    wSrcId <- view #id <$> fixtureWallet
    replicateM_ batchSize
        $ do
            addrs <- request $ C.listAddresses (wDest ^. #id) Nothing
            let destination = addrs !! 1 ^. #id
                amount =
                    AddressAmount
                        { address = destination
                        , amount = ApiAmount amtToSend
                        , assets = ApiWalletAssets []
                        }
                payload =
                    PostTransactionOldData
                        { payments = pure amount
                        , passphrase = ApiT $ unsafeFromText fixturePassphrase
                        , withdrawal = Nothing
                        , metadata = Nothing
                        , timeToLive = Nothing
                        }
            request $ C.postTransaction wSrcId payload

    eventually "repeatPostTx: wallet balance is as expected" $ do
        rWal1 <- request $ C.getWallet $ wDest ^. #id
        rWal1 ^. #balance . #available . #toNatural `shouldBe` amtExp

    void $ request $ C.deleteWallet wSrcId

iterations :: Int
iterations = 10

scene :: Reporter IO -> String -> BenchM (Either ClientError a) -> BenchM ()
scene reporter title scenario = do
    ts <- measureApiLogs iterations scenario
    let avg = meanAvg ts
        semantic = mkSemantic [T.pack title]
    liftIO
        $ report reporter
        $ pure
        $ Benchmark semantic
        $ Result avg Milliseconds iterations
    fmtResult title ts

sceneOfClientM :: Reporter IO -> String -> ClientM a -> BenchM ()
sceneOfClientM reporter title action =
    scene reporter title $ requestWithError action

listAllTransactions :: ApiT WalletId -> ClientM [ApiTransaction C.Testnet42]
listAllTransactions walId =
    C.listTransactions
        walId
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        False

pend :: Applicative m => m () -> m ()
pend = const $ pure ()

runScenario
    :: Reporter IO
    -> BenchM (ApiWallet, ApiWallet, ApiWallet, ApiWallet)
    -> BenchM ()
runScenario reporter scenario = lift . runResourceT $ do
    let sceneOfClientMR :: String -> ClientM a -> BenchM ()
        sceneOfClientMR = do sceneOfClientM reporter

        -- | Decrease likelihood of failing with `no_utxos_available`
        -- by calling this short delay after creating txs.
        waitForChange :: BenchM ()
        waitForChange = liftIO $ threadDelay 5_000_000

    (wal1, wal2, walMA, maWalletToMigrate) <- scenario
    let wal1Id = wal1 ^. #id
        wal2Id = wal2 ^. #id
        walMAId = walMA ^. #id
        maWalletToMigrateId = maWalletToMigrate ^. #id
        amt = minUTxOValue era
    sceneOfClientMR "listWallets" C.listWallets
    sceneOfClientMR "getWallet" $ C.getWallet wal1Id
    sceneOfClientMR "getUTxOsStatistics" $ C.getWalletUtxoStatistics wal1Id
    sceneOfClientMR "listAddresses" $ C.listAddresses wal1Id Nothing
    sceneOfClientMR "listTransactions" $ listAllTransactions wal1Id

    txs <- request $ listAllTransactions wal1Id
    sceneOfClientMR "getTransaction"
        $ C.getTransaction wal1Id (ApiTxId $ txs !! 1 ^. #id) False
    waitForChange

    addrs <- request $ C.listAddresses wal2Id Nothing
    let destination = addrs !! 1 ^. #id
        amount =
            AddressAmount
                { address = destination
                , amount = ApiAmount amt
                , assets = ApiWalletAssets []
                }
        payload =
            PostTransactionFeeOldData
                { payments = pure amount
                , withdrawal = Nothing
                , metadata = Nothing
                , timeToLive = Nothing
                }
    sceneOfClientMR "postTransactionFee" $ C.postTransactionFee wal1Id payload
    waitForChange

    let payloadTx =
            PostTransactionOldData
                { payments = pure amount
                , passphrase = ApiT $ unsafeFromText fixturePassphrase
                , withdrawal = Nothing
                , metadata = Nothing
                , timeToLive = Nothing
                }
    sceneOfClientMR "postTransaction" $ C.postTransaction wal1Id payloadTx
    waitForChange

    let payments =
            replicate 5
                $ AddressAmount
                    { address = destination
                    , amount = ApiAmount amt
                    , assets = ApiWalletAssets []
                    }
        payloadTxTo5Addr =
            PostTransactionOldData
                { payments = NE.fromList payments
                , passphrase = ApiT $ unsafeFromText fixturePassphrase
                , withdrawal = Nothing
                , metadata = Nothing
                , timeToLive = Nothing
                }
    sceneOfClientMR "postTransactionTo5Addrs"
        $ C.postTransaction wal1Id payloadTxTo5Addr
    waitForChange

    let
        assetToSend = over _Unwrapped pick $ walMA ^. #assets . #total
        pick (x : _xs) = pure $ set #quantity (minUTxOValue era) x
        pick [] = error "No assets to pick from"
        paymentsMA =
            AddressAmount
                { address = destination
                , amount = ApiAmount amt
                , assets = assetToSend
                }
        payloadMA =
            PostTransactionOldData
                { payments = pure paymentsMA
                , passphrase = ApiT $ unsafeFromText fixturePassphrase
                , withdrawal = Nothing
                , metadata = Nothing
                , timeToLive = Nothing
                }
    -- Todo ADP-3293
    pend
        $ sceneOfClientMR "postTransactionMA"
        $ C.postTransaction walMAId payloadMA

    sceneOfClientMR "listStakePools" $ C.listPools $ ApiT <$> arbitraryStake

    sceneOfClientMR "getNetworkInfo" CN.networkInformation

    sceneOfClientMR "listAssets" $ C.getAssets walMAId

    let assetsSrc = walMA ^. #assets . #total
        (polId, assName) =
            bimap unsafeFromText unsafeFromText
                $ fst
                $ pickAnAsset assetsSrc
    sceneOfClientMR "getAsset" $ C.getAsset walMAId (ApiT polId) (ApiT assName)

    let addresses = replicate 5 destination
        migrationPlanPayload =
            ApiWalletMigrationPlanPostData $ NE.fromList addresses

    sceneOfClientMR "postMRigrationPlan"
        $ C.planMigration maWalletToMigrateId migrationPlanPayload

    let migrationPayload =
            ApiWalletMigrationPostData
                { addresses = NE.fromList addresses
                , passphrase = ApiT $ unsafeFromText fixturePassphrase
                }
    -- Todo ADP-3293
    pend
        $ sceneOfClientMR "postMRigration"
        $ C.migrate maWalletToMigrateId migrationPayload

fmtResult :: String -> [NominalDiffTime] -> BenchM ()
fmtResult title ts = liftIO $ Measure.fmtResult title ts

fmtTitle :: Builder -> BenchM ()
fmtTitle = liftIO . Measure.fmtTitle

arbitraryStake :: Maybe Coin
arbitraryStake = Just $ ada 10_000
  where
    ada = Coin . (1_000_000 *)

measureApiLogs :: Exception e => Int -> BenchM (Either e a) -> BenchM [NominalDiffTime]
measureApiLogs count action = do
    BenchCtx _ctx capture <- ask
    run <- toIO $ do
        r <- action
        case r of
            Left e -> throwM e
            Right q -> pure q
    liftIO $ Measure.measureApiLogs count capture run

runWarmUpScenario :: BenchM ()
runWarmUpScenario = do
    -- this one is to have comparable results from first to last measurement
    -- in runScenario
    t <- measureApiLogs iterations $ requestWithError CN.networkInformation
    fmtResult "getNetworkInfo     " t

withShelleyServer :: Tracers IO -> (SomeMnemonic -> Context -> IO ()) -> IO ()
withShelleyServer tracers action = withFaucet $ \faucetClientEnv -> do
    faucetFunds <- Faucet.runFaucetM faucetClientEnv mkFaucetFunds

    ctx <- newEmptyMVar
    faucet <- Faucet.initFaucet faucetClientEnv
    massiveWalletMnemonic' <- massiveWalletMnemonic faucet
    let testnetMagic = Cluster.TestnetMagic 42
    let setupContext np baseUrl = do
            let sixtySeconds = 60 * 1_000_000 -- 60s in microseconds
            manager <-
                newManager
                    defaultManagerSettings
                        { managerResponseTimeout = responseTimeoutMicro sixtySeconds
                        }
            putMVar
                ctx
                Context
                    { _manager = (baseUrl, manager)
                    , _walletPort = Port . fromIntegral $ portFromURL baseUrl
                    , _faucet = faucet
                    , _networkParameters = np
                    , _testnetMagic = testnetMagic
                    , _poolGarbageCollectionEvents =
                        error "poolGarbageCollectionEvents not available"
                    , _smashUrl = ""
                    , _mainEra = maxBound
                    , _mintSeaHorseAssets = error "mintSeaHorseAssets not available"
                    , _preprodWallets = []
                    }
    race_
        (takeMVar ctx >>= action massiveWalletMnemonic')
        (withServer testnetMagic faucetFunds setupContext)
  where
    mkFaucetFunds = do
        shelleyFunds <- Faucet.shelleyFunds shelleyTestnet
        massiveFunds <-
            Faucet.massiveWalletFunds massiveWalletAmt 10_000 shelleyTestnet
        maryAllegraFunds <-
            Faucet.maryAllegraFunds (Coin 10_000_000) shelleyTestnet
        pure
            FaucetFunds
                { pureAdaFunds = shelleyFunds
                , maryAllegraFunds
                , massiveWalletFunds = massiveFunds
                }

    withServer cfgTestnetMagic faucetFunds setupAction = do
        skipCleanup <- SkipCleanup <$> isEnvSet "NO_CLEANUP"
        withSystemTempDir stdoutTextTracer "latency" skipCleanup $ \dir -> do
            let testDir = absDir dir
                db = testDir </> relDir "wallets"
            createDirectory $ toFilePath db
            CommandLineOptions{clusterConfigsDir} <- parseCommandLineOptions
            clusterEra <- Cluster.clusterEraFromEnv
            cfgNodeLogging <-
                Cluster.logFileConfigFromEnv
                    $ Just
                    $ mkRelDirOf
                    $ Cluster.clusterEraToString clusterEra
            withTempFile $ \socket -> do
                let clusterConfig =
                        Cluster.Config
                            { cfgStakePools = pure (NE.head defaultPoolConfigs)
                            , cfgLastHardFork = clusterEra
                            , cfgNodeLogging
                            , cfgClusterDir = DirOf testDir
                            , cfgClusterConfigs = clusterConfigsDir
                            , cfgTestnetMagic
                            , cfgShelleyGenesisMods =
                                [ over #sgSlotLength (const 0.2)
                                , -- to avoid "PastHorizonException" errors, as wallet
                                  -- doesn't keep up with retrieving fresh time interpreter.
                                  over #sgSecurityParam (const 100)
                                  -- when it low then cluster is not making blocks;
                                ]
                            , cfgTracer = stdoutTextTracer
                            , cfgNodeOutputFile = Nothing
                            , cfgRelayNodePath = mkRelDirOf "relay"
                            , cfgClusterLogFile = Nothing
                            , cfgNodeToClientSocket = UnixPipe
                                $ FileOf $ absFile socket
                            }
                withCluster
                    clusterConfig
                    faucetFunds
                    (onClusterStart cfgTestnetMagic setupAction db)

    onClusterStart testnetMagic setupAction db node = do
        let (RunningNode conn genesisData vData) = node
        let (networkParameters, block0, _gp) = fromGenesisData genesisData
        serveWallet
            (NodeSource conn vData (SyncTolerance 10))
            networkParameters
            tunedForMainnetPipeliningStrategy
            (NTestnet . fromIntegral $ testnetMagicToNatural testnetMagic)
            [] -- pool certificates
            tracers
            (Just $ toFilePath db)
            Nothing -- db decorator
            "127.0.0.1"
            (ListenOnPort 8_090)
            Nothing
            Nothing -- tls configuration
            Nothing -- settings
            Nothing -- token metadata server
            block0
            (setupAction networkParameters)

massiveWalletUTxOSize :: Int
massiveWalletUTxOSize = 10_000

massiveWalletAmt :: Coin
massiveWalletAmt = ada 1_000
  where
    ada x = Coin $ x * 1000_000

era :: ApiEra
era = maxBound

--------------------------------------------------------------------------------
-- Command line options --------------------------------------------------------

newtype CommandLineOptions = CommandLineOptions
    {clusterConfigsDir :: DirOf "cluster-configs"}
    deriving stock (Show)

parseCommandLineOptions :: IO CommandLineOptions
parseCommandLineOptions = do
    absolutizer <- newAbsolutizer
    O.execParser
        $ O.info
            ( fmap CommandLineOptions (clusterConfigsDirParser absolutizer)
                <**> O.helper
            )
            (O.progDesc "Cardano Wallet's Latency Benchmark")
