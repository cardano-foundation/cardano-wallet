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
import Cardano.BM.Data.LogItem
    ( LogObject
    )
import Cardano.BM.Data.Severity
    ( Severity (Error)
    )
import Cardano.BM.Data.Tracer
    ( HasSeverityAnnotation
    , Tracer (..)
    , filterSeverity
    )
import Cardano.BM.Extra
    ( stdoutTextTracer
    , trMessage
    )
import Cardano.BM.Trace
    ( traceInTVarIO
    )
import Cardano.CLI
    ( Port (..)
    )
import Cardano.Mnemonic
    ( SomeMnemonic
    )
import Cardano.Wallet.Api.Http.Shelley.Server
    ( Listen (ListenOnPort)
    )
import Cardano.Wallet.Api.Types
    ( AddressAmount (..)
    , ApiEra
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
import Cardano.Wallet.Api.Types.WalletAssets
    ( ApiWalletAssets (..)
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
    ( withLatencyLogging
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
import Cardano.Wallet.Launch.Cluster.FileOf
    ( DirOf (..)
    , mkRelDirOf
    , newAbsolutizer
    , toFilePath
    )
import Cardano.Wallet.LocalCluster
    ( clusterConfigsDirParser
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
import Cardano.Wallet.Shelley
    ( Tracers
    , Tracers' (..)
    , serveWallet
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
import Control.Monad
    ( replicateM
    , replicateM_
    , void
    )
import Control.Monad.Catch
    ( Exception
    , MonadThrow (..)
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
import Data.Text.Class.Extended
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
import Network.Wai.Middleware.Logging
    ( ApiLog (..)
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
import System.IO.Temp.Extra
    ( SkipCleanup (..)
    , withSystemTempDir
    )
import System.Path
    ( absDir
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
import UnliftIO.STM
    ( TVar
    )

import qualified Cardano.Wallet.Api.Clients.Network as CN
import qualified Cardano.Wallet.Api.Clients.Testnet.Id as C
import qualified Cardano.Wallet.Api.Clients.Testnet.Shelley as C
import qualified Cardano.Wallet.Benchmarks.Latency.Measure as Measure
import qualified Cardano.Wallet.Faucet as Faucet
import qualified Cardano.Wallet.Launch.Cluster as Cluster
import qualified Data.List.NonEmpty as NE
import qualified Options.Applicative as O

main :: IO ()
main = withUtf8
    $ withLatencyLogging setupTracers
    $ \tracers capture ->
        withShelleyServer tracers $ \massiveWalletMnemonic' ctx ->
            runReaderT (runResourceT $ walletApiBench massiveWalletMnemonic')
                $ BenchCtx ctx capture
  where
    onlyErrors :: (HasSeverityAnnotation a, ToText a) => Tracer IO a
    onlyErrors = filterSeverity (const $ pure Error) stdoutTextTracer

    setupTracers :: TVar [LogObject ApiLog] -> Tracers IO
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

-- Creates n fixture wallets and return 3 of them

walletApiBench :: SomeMnemonic -> BenchM ()
walletApiBench massiveMnemonic = do
    fmtTitle "Non-cached run"
    runWarmUpScenario

    fmtTitle "Latencies for 2 fixture wallets scenario"
    runScenario (nFixtureWallet 2)

    fmtTitle "Latencies for 10 fixture wallets scenario"
    runScenario (nFixtureWallet 10)

    fmtTitle "Latencies for 100 fixture wallets"
    runScenario (nFixtureWallet 100)

    fmtTitle "Latencies for 2 fixture wallets with 10 txs scenario"
    runScenario (nFixtureWalletWithTxs 2 10)

    fmtTitle "Latencies for 2 fixture wallets with 20 txs scenario"
    runScenario (nFixtureWalletWithTxs 2 20)

    fmtTitle "Latencies for 2 fixture wallets with 100 txs scenario"
    runScenario (nFixtureWalletWithTxs 2 100)

    fmtTitle "Latencies for 10 fixture wallets with 10 txs scenario"
    runScenario (nFixtureWalletWithTxs 10 10)

    fmtTitle "Latencies for 10 fixture wallets with 20 txs scenario"
    runScenario (nFixtureWalletWithTxs 10 20)

    fmtTitle "Latencies for 10 fixture wallets with 100 txs scenario"
    runScenario (nFixtureWalletWithTxs 10 100)

    fmtTitle "Latencies for 2 fixture wallets with 100 utxos scenario"
    runScenario (nFixtureWalletWithUTxOs 2 100)

    fmtTitle "Latencies for 2 fixture wallets with 200 utxos scenario"
    runScenario (nFixtureWalletWithUTxOs 2 200)

    fmtTitle "Latencies for 2 fixture wallets with 500 utxos scenario"
    runScenario (nFixtureWalletWithUTxOs 2 500)

    fmtTitle "Latencies for 2 fixture wallets with 1000 utxos scenario"
    runScenario (nFixtureWalletWithUTxOs 2 1_000)

    fmtTitle
        $ "Latencies for 2 fixture wallets with "
            <> build massiveWalletUTxOSize
            <> " utxos scenario"
    runScenario $ massiveFixtureWallet massiveMnemonic

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

scene :: String -> BenchM (Either ClientError a) -> BenchM ()
scene title scenario = measureApiLogs scenario >>= fmtResult title

sceneOfClientM :: String -> ClientM a -> BenchM ()
sceneOfClientM title action = scene title $ requestWithError action

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

runScenario :: BenchM (ApiWallet, ApiWallet, ApiWallet, ApiWallet) -> BenchM ()
runScenario scenario = lift . runResourceT $ do
    (wal1, wal2, walMA, maWalletToMigrate) <- scenario
    let wal1Id = wal1 ^. #id
        wal2Id = wal2 ^. #id
        walMAId = walMA ^. #id
        maWalletToMigrateId = maWalletToMigrate ^. #id
        amt = minUTxOValue era
    sceneOfClientM "listWallets" C.listWallets
    sceneOfClientM "getWallet" $ C.getWallet wal1Id
    sceneOfClientM "getUTxOsStatistics" $ C.getWalletUtxoStatistics wal1Id
    sceneOfClientM "listAddresses" $ C.listAddresses wal1Id Nothing
    sceneOfClientM "listTransactions" $ listAllTransactions wal1Id

    txs <- request $ listAllTransactions wal1Id
    sceneOfClientM "getTransaction"
        $ C.getTransaction wal1Id (ApiTxId $ txs !! 1 ^. #id) False

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
    sceneOfClientM "postTransactionFee" $ C.postTransactionFee wal1Id payload

    let payloadTx =
            PostTransactionOldData
                { payments = pure amount
                , passphrase = ApiT $ unsafeFromText fixturePassphrase
                , withdrawal = Nothing
                , metadata = Nothing
                , timeToLive = Nothing
                }
    sceneOfClientM "postTransaction" $ C.postTransaction wal1Id payloadTx

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
    sceneOfClientM "postTransactionTo5Addrs"
        $ C.postTransaction wal1Id payloadTxTo5Addr

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
        $ sceneOfClientM "postTransactionMA"
        $ C.postTransaction walMAId payloadMA

    sceneOfClientM "listStakePools" $ C.listPools $ ApiT <$> arbitraryStake

    sceneOfClientM "getNetworkInfo" CN.networkInformation

    sceneOfClientM "listAssets" $ C.getAssets walMAId

    let assetsSrc = walMA ^. #assets . #total
        (polId, assName) =
            bimap unsafeFromText unsafeFromText
                $ fst
                $ pickAnAsset assetsSrc
    sceneOfClientM "getAsset" $ C.getAsset walMAId (ApiT polId) (ApiT assName)

    let addresses = replicate 5 destination
        migrationPlanPayload =
            ApiWalletMigrationPlanPostData $ NE.fromList addresses

    sceneOfClientM "postMigrationPlan"
        $ C.planMigration maWalletToMigrateId migrationPlanPayload

    let migrationPayload =
            ApiWalletMigrationPostData
                { addresses = NE.fromList addresses
                , passphrase = ApiT $ unsafeFromText fixturePassphrase
                }
    -- Todo ADP-3293
    pend
        $ sceneOfClientM "postMigration"
        $ C.migrate maWalletToMigrateId migrationPayload

fmtResult :: String -> [NominalDiffTime] -> BenchM ()
fmtResult title ts = liftIO $ Measure.fmtResult title ts

fmtTitle :: Builder -> BenchM ()
fmtTitle = liftIO . Measure.fmtTitle

arbitraryStake :: Maybe Coin
arbitraryStake = Just $ ada 10_000
  where
    ada = Coin . (1_000_000 *)

measureApiLogs :: Exception e => BenchM (Either e a) -> BenchM [NominalDiffTime]
measureApiLogs action = do
    BenchCtx _ctx capture <- ask
    run <- toIO $ do
        r <- action
        case r of
            Left e -> throwM e
            Right q -> pure q
    liftIO $ Measure.measureApiLogs capture run

runWarmUpScenario :: BenchM ()
runWarmUpScenario = do
    -- this one is to have comparable results from first to last measurement
    -- in runScenario
    t <- measureApiLogs $ requestWithError CN.networkInformation
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
                    { _cleanup = pure ()
                    , _manager = (baseUrl, manager)
                    , _walletPort = Port . fromIntegral $ portFromURL baseUrl
                    , _faucet = faucet
                    , _networkParameters = np
                    , _testnetMagic = testnetMagic
                    , _poolGarbageCollectionEvents =
                        error "poolGarbageCollectionEvents not available"
                    , _smashUrl = ""
                    , _mainEra = maxBound
                    , _mintSeaHorseAssets = error "mintSeaHorseAssets not available"
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
            (fmap CommandLineOptions (clusterConfigsDirParser absolutizer)
                <**> O.helper
            )
            (O.progDesc "Cardano Wallet's Latency Benchmark")
