{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude

import Cardano.Address
    ( Address
    )
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
    ( MkSomeMnemonic (..)
    , Mnemonic
    , mnemonicToText
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
    , ApiWalletMigrationPlan (..)
    , PostTransactionFeeOldData (..)
    , PostTransactionOldData (..)
    , WalletOrAccountPostData (..)
    , WalletPostData (..)
    , WalletStyle (..)
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
    , requestC
    , requestWithError
    )
import Cardano.Wallet.Benchmarks.Latency.Measure
    ( withLatencyLogging
    )
import Cardano.Wallet.Launch.Cluster
    ( Config (..)
    , FaucetFunds (..)
    , FileOf (..)
    , RunningNode (..)
    , defaultPoolConfigs
    , testnetMagicToNatural
    , withCluster
    , withFaucet
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
    ( NetworkDiscriminant (..)
    , NetworkId (..)
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance (..)
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
    , unsafeMkMnemonic
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
    , view
    , (^.)
    )
import Data.Generics.Labels
    ()
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
import System.FilePath
    ( (</>)
    )
import System.IO.Temp.Extra
    ( SkipCleanup (..)
    , withSystemTempDir
    )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , eventually
    , faucetAmt
    , fixturePassphrase
    , json
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

import qualified Cardano.Faucet.Addresses as Addresses
import qualified Cardano.Wallet.Api.Clients.Testnet.Network as CN
import qualified Cardano.Wallet.Api.Clients.Testnet.Shelley as C
import qualified Cardano.Wallet.Api.Link as Link
import qualified Cardano.Wallet.Benchmarks.Latency.Measure as Measure
import qualified Cardano.Wallet.Faucet as Faucet
import qualified Cardano.Wallet.Launch.Cluster as Cluster
import Cardano.Wallet.Primitive.Types
    ( WalletId
    )
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Options.Applicative as O

-- will go away once we have all implemented in terms of servant-client code
type A = 'Testnet 42

main :: IO ()
main = withUtf8
    $ withLatencyLogging setupTracers
    $ \tracers capture ->
        withShelleyServer tracers $ \ctx ->
            runReaderT (runResourceT walletApiBench) $ BenchCtx ctx capture
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

walletApiBench :: BenchM ()
walletApiBench = do
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
    runScenario massiveFixtureWallet

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
        rWal1 <- requestC $ C.getWallet (wal1 ^. #id)
        rWal1 ^. #balance . #available . #toNatural `shouldBe` sum utxoExp

    rStat <- requestC $ C.getWalletUtxoStatistics (wal1 ^. #id)
    utxoStatisticsFromCoins (fromIntegral <$> utxoExp) `shouldBe` rStat
    pure (wal1, wal2, walMA, maWalletToMigrate)

massiveFixtureWallet :: BenchM (ApiWallet, ApiWallet, ApiWallet, ApiWallet)
massiveFixtureWallet = do
    (_, wal2, walMA, maWalletToMigrate) <- nFixtureWallet 2
    Right massiveMnemonic <-
        pure
            $ mkSomeMnemonic @'[15]
            $ mnemonicToText massiveWallet
    wal1 <-
        requestC
            $ C.postWallet
            $ WalletOrAccountPostData
            $ Left
            $ WalletPostData
                { addressPoolGap = Nothing
                , mnemonicSentence = ApiMnemonicT massiveMnemonic
                , mnemonicSecondFactor = Nothing
                , name = ApiT $ unsafeFromText "Massive wallet"
                , passphrase = ApiT $ unsafeFromText fixturePassphrase
                , oneChangeAddressMode = Nothing
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
            addrs <- requestC $ C.listAddresses (wDest ^. #id) Nothing
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
            requestC $ C.postTransaction wSrcId payload

    eventually "repeatPostTx: wallet balance is as expected" $ do
        rWal1 <- requestC $ C.getWallet $ wDest ^. #id
        rWal1 ^. #balance . #available . #toNatural `shouldBe` amtExp

    void $ requestC $ C.deleteWallet wSrcId

scene :: String -> BenchM (Either ClientError a) -> BenchM ()
scene title scenario = measureApiLogs scenario >>= fmtResult title

sceneOfClientM :: String -> ClientM a -> BenchM ()
sceneOfClientM title action = scene title $ requestWithError action

listAllTransactions :: ApiT WalletId -> ClientM [ApiTransaction C.A]
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

runScenario :: BenchM (ApiWallet, ApiWallet, ApiWallet, ApiWallet) -> BenchM ()
runScenario scenario = lift . runResourceT $ do
    (wal1, wal2, walMA, maWalletToMigrate) <- scenario
    let wal1Id = wal1 ^. #id
        wal2Id = wal2 ^. #id
        walMAId = walMA ^. #id
        amt = minUTxOValue era
    sceneOfClientM "listWallets" C.listWallets
    sceneOfClientM "getWallet" $ C.getWallet wal1Id
    sceneOfClientM "getUTxOsStatistics" $ C.getWalletUtxoStatistics wal1Id
    sceneOfClientM "listAddresses" $ C.listAddresses wal1Id Nothing
    sceneOfClientM "listTransactions" $ listAllTransactions wal1Id

    txs <- requestC $ listAllTransactions wal1Id
    sceneOfClientM "getTransaction"
        $ C.getTransaction wal1Id (ApiTxId $ txs !! 1 ^. #id) False

    addrs <- requestC $ C.listAddresses wal2Id Nothing
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

    -- let assetsToSend = walMA ^. #assets . #total
    -- let val = minUTxOValue era <$ pickAnAsset assetsToSend
    -- payloadMA <- mkTxPayloadMA @A destination (2 * minUTxOValue era) [val] fixturePassphrase
    -- t7b <-
    --     measureApiLogs
    --         $ request @(ApiTransaction A)
    --             (Link.createTransactionOld @'Shelley walMA)
    --             Default
    --             payloadMA
    -- fmtResult "postTransactionMA  " t7b

    sceneOfClientM "listStakePools" $ C.listPools $ ApiT <$> arbitraryStake

    sceneOfClientM "getNetworkInfo" CN.networkInformation

    sceneOfClientM "listAssets" $ C.getAssets walMAId

    let assetsSrc = walMA ^. #assets . #total
    let (polId, assName) =
            bimap unsafeFromText unsafeFromText
                $ fst
                $ pickAnAsset assetsSrc
    t11 <-
        measureApiLogs
            $ requestWithError
            $ C.getAsset (walMA ^. #id) (ApiT polId) (ApiT assName)
    fmtResult "getMultiAsset      " t11

    -- Create a migration plan:
    let endpointPlan = (Link.createMigrationPlan @'Shelley maWalletToMigrate)
        addresses = replicate 5 destination
    t12a <-
        measureApiLogs
            $ request @(ApiWalletMigrationPlan A)
                endpointPlan
                Default
            $ Json [json|{addresses: #{addresses}}|]
    fmtResult "postMigrationPlan  " t12a

-- -- Perform a migration:
-- let endpointMigrate = Link.migrateWallet @'Shelley maWalletToMigrate
-- t12b <-
--     measureApiLogs
--         $ request @[ApiTransaction A]
--             endpointMigrate
--             Default
--         $ Json
--             [json|
--         { passphrase: #{fixturePassphrase}
--         , addresses: #{addresses}
--         }|]
-- fmtResult "postMigration      " t12b

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

withShelleyServer :: Tracers IO -> (Context -> IO ()) -> IO ()
withShelleyServer tracers action = withFaucet $ \faucetClientEnv -> do
    faucetFunds <- Faucet.runFaucetM faucetClientEnv mkFaucetFunds
    ctx <- newEmptyMVar
    let testnetMagic = Cluster.TestnetMagic 42
    let setupContext np baseUrl = do
            let sixtySeconds = 60 * 1_000_000 -- 60s in microseconds
            manager <-
                newManager
                    defaultManagerSettings
                        { managerResponseTimeout = responseTimeoutMicro sixtySeconds
                        }
            faucet <- Faucet.initFaucet faucetClientEnv
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
                    , _moveRewardsToScript =
                        error "moveRewardsToScript not available"
                    }
    race_
        (takeMVar ctx >>= action)
        (withServer testnetMagic faucetFunds setupContext)
  where
    mkFaucetFunds = do
        shelleyFunds <- Faucet.shelleyFunds shelleyTestnet
        maryAllegraFunds <-
            Faucet.maryAllegraFunds (Coin 10_000_000) shelleyTestnet
        pure
            FaucetFunds
                { pureAdaFunds = shelleyFunds
                , maryAllegraFunds
                , mirCredentials = []
                }

    withServer cfgTestnetMagic faucetFunds setupAction = do
        skipCleanup <- SkipCleanup <$> isEnvSet "NO_CLEANUP"
        withSystemTempDir stdoutTextTracer "latency" skipCleanup $ \dir -> do
            let db = dir </> "wallets"
            createDirectory db
            CommandLineOptions{clusterConfigsDir} <- parseCommandLineOptions
            clusterEra <- Cluster.clusterEraFromEnv
            cfgNodeLogging <-
                Cluster.logFileConfigFromEnv
                    (Just (Cluster.clusterEraToString clusterEra))
            let clusterConfig =
                    Cluster.Config
                        { cfgStakePools = pure (NE.head defaultPoolConfigs)
                        , cfgLastHardFork = clusterEra
                        , cfgNodeLogging
                        , cfgClusterDir = FileOf @"cluster" dir
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
            (Just db)
            Nothing -- db decorator
            "127.0.0.1"
            (ListenOnPort 8_090)
            Nothing -- tls configuration
            Nothing -- settings
            Nothing -- token metadata server
            block0
            (setupAction networkParameters)

-- | A special Shelley Wallet with a massive UTxO set
massiveWallet :: Mnemonic 15
massiveWallet =
    unsafeMkMnemonic
        $ T.words
            "interest ready music wet trophy ten boss topple fitness fold \
            \saddle finish update someone pause"

massiveWalletUTxOSize :: Int
massiveWalletUTxOSize = 10_000

massiveWalletFunds :: [(Address, Coin)]
massiveWalletFunds =
    take massiveWalletUTxOSize
        $ map (,massiveWalletAmt)
        $ Addresses.shelley shelleyTestnet massiveWallet

massiveWalletAmt :: Coin
massiveWalletAmt = ada 1_000
  where
    ada x = Coin $ x * 1000_000

era :: ApiEra
era = maxBound

--------------------------------------------------------------------------------
-- Command line options --------------------------------------------------------

newtype CommandLineOptions = CommandLineOptions
    {clusterConfigsDir :: FileOf "cluster-configs"}
    deriving stock (Show)

parseCommandLineOptions :: IO CommandLineOptions
parseCommandLineOptions =
    O.execParser
        $ O.info
            (fmap CommandLineOptions clusterConfigsDirParser <**> O.helper)
            (O.progDesc "Cardano Wallet's Latency Benchmark")
