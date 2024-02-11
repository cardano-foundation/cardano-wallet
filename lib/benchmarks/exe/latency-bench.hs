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
    ( Mnemonic
    , mnemonicToText
    )
import Cardano.Wallet.Api.Http.Shelley.Server
    ( Listen (ListenOnPort)
    )
import Cardano.Wallet.Api.Types
    ( AddressAmount (..)
    , ApiAddressWithPath
    , ApiAsset (..)
    , ApiEra
    , ApiFee
    , ApiNetworkInformation
    , ApiT (..)
    , ApiTransaction
    , ApiTxId (..)
    , ApiUtxoStatistics
    , ApiWallet
    , ApiWalletMigrationPlan (..)
    , PostTransactionOldData (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Api.Types.Amount
    ( ApiAmount (..)
    )
import Cardano.Wallet.Api.Types.WalletAssets
    ( ApiWalletAssets (..)
    )
import Cardano.Wallet.LatencyBenchShared
    ( LogCaptureFunc
    , fmtResult
    , fmtTitle
    , measureApiLogs
    , withLatencyLogging
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
import Cardano.Wallet.Pools
    ( StakePool
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
    )
import Control.Monad.IO.Class
    ( MonadIO
    , liftIO
    )
import Data.Aeson
    ( Value
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
import Data.Text.Class.Extended
    ( ToText
    )
import Data.Time
    ( NominalDiffTime
    )
import Fmt
    ( build
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
    , mkClientEnv
    , parseBaseUrl
    , runClientM
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
import Test.Hspec
    ( HasCallStack
    , shouldBe
    )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , ResourceT
    , eventually
    , expectField
    , expectResponseCode
    , expectSuccess
    , expectWalletUTxO
    , faucetAmt
    , fixtureMultiAssetWallet
    , fixturePassphrase
    , fixturePassphraseLenient
    , fixtureWallet
    , fixtureWalletWith
    , getFromResponse
    , json
    , minUTxOValue
    , mkTxPayloadMA
    , pickAnAsset
    , postWallet
    , request
    , runResourceT
    , unsafeRequest
    , verify
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
import qualified Cardano.Wallet.Api.Clients.Testnet.Shelley as C
import qualified Cardano.Wallet.Api.Link as Link
import qualified Cardano.Wallet.Faucet as Faucet
import qualified Cardano.Wallet.Launch.Cluster as Cluster
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP
import qualified Options.Applicative as O

-- will go away once we have all implemented in terms of servant-client code
type A = 'Testnet 42

main :: IO ()
main = withUtf8
    $ withLatencyLogging setupTracers
    $ \tracers capture ->
        withShelleyServer tracers $ \ctx ->
            walletApiBench capture ctx
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

walletApiBench
    :: LogCaptureFunc ApiLog ()
    -> Context
    -> IO ()
walletApiBench capture ctx = do
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
  where
    -- one day we will export the manager from the context
    cenv = case parseBaseUrl $ show (fst $ ctx ^. #_manager) of
        Left _ -> error "Invalid base URL"
        Right bu -> mkClientEnv (snd $ ctx ^. #_manager) bu
    req :: MonadIO m => ClientM a -> m (Either ClientError a)
    req x = liftIO $ runClientM x cenv
    rightReq :: MonadIO m => ClientM a -> m a
    rightReq x =
        req x >>= \case
            Right a -> pure a
            Left e -> error $ show e
    -- Creates n fixture wallets and return 3 of them
    nFixtureWallet
        :: HasCallStack
        => Int
        -> ResourceT IO (ApiWallet, ApiWallet, ApiWallet, ApiWallet)
    nFixtureWallet n = do
        wal1 : wal2 : _ <- replicateM n (fixtureWallet ctx)
        walMA <- fixtureMultiAssetWallet ctx
        maWalletToMigrate <- fixtureMultiAssetWallet ctx
        pure (wal1, wal2, walMA, maWalletToMigrate)

    -- Creates n fixture wallets and send 1-ada transactions to one of them
    -- (m times). The money is sent in batches (see batchSize below) from
    -- additionally created source fixture wallet. Then we wait for the money
    -- to be accommodated in recipient wallet. After that the source fixture
    -- wallet is removed.
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

    nFixtureWalletWithUTxOs n utxoNumber = do
        let utxoExp = replicate utxoNumber (minUTxOValue era)
        wal1 <- fixtureWalletWith @A ctx utxoExp
        (_, wal2, walMA, maWalletToMigrate) <- nFixtureWallet n

        eventually "Wallet balance is as expected" $ do
            rWal1 <-
                request @ApiWallet
                    ctx
                    (Link.getWallet @'Shelley wal1)
                    Default
                    Empty
            verify
                rWal1
                [ expectSuccess
                , expectField
                    (#balance . #available . #toNatural)
                    ( `shouldBe`
                        ((minUTxOValue era) * (fromIntegral utxoNumber))
                    )
                ]

        rStat <-
            request @ApiUtxoStatistics
                ctx
                (Link.getUTxOsStatistics @'Shelley wal1)
                Default
                Empty
        expectResponseCode HTTP.status200 rStat
        expectWalletUTxO (fromIntegral <$> utxoExp) (snd rStat)
        pure (wal1, wal2, walMA, maWalletToMigrate)

    massiveFixtureWallet = do
        (_, wal2, walMA, maWalletToMigrate) <- nFixtureWallet 2
        let payload =
                Json
                    [json| {
                "name": "Massive wallet",
                "mnemonic_sentence": #{mnemonicToText massiveWallet},
                "passphrase": #{fixturePassphrase},
                "address_pool_gap": #{massiveWalletUTxOSize}
                } |]
        -- Because the funds for the wallet is located in the genesis file, it
        -- will be discovered as part of the postWallet call, and we don't need
        -- to wait with 'eventually'.
        rWal <- postWallet ctx payload
        verify
            rWal
            [ expectSuccess
            , expectField
                (#balance . #available . #toNatural)
                ( `shouldBe`
                    ( fromIntegral massiveWalletUTxOSize
                        * unCoin massiveWalletAmt
                    )
                )
            ]
        let wal1 = getFromResponse Prelude.id rWal
        pure (wal1, wal2, walMA, maWalletToMigrate)

    repeatPostTx wDest amtToSend batchSize amtExp = do
        wSrcId <- view #id <$> fixtureWallet ctx
        replicateM_ batchSize
            $ do
                addrs <- rightReq $ C.listAddresses (wDest ^. #id) Nothing
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
                            , passphrase = ApiT fixturePassphraseLenient
                            , withdrawal = Nothing
                            , metadata = Nothing
                            , timeToLive = Nothing
                            }
                rightReq $ C.postTransaction wSrcId payload

        eventually "repeatPostTx: wallet balance is as expected" $ do
            rWal1 <- rightReq $ C.getWallet $ wDest ^. #id
            -- request @ApiWallet ctx (Link.getWallet @'Shelley wDest) Default Empty
            rWal1 ^. #balance . #available . #toNatural `shouldBe` amtExp

        rightReq $ C.deleteWallet wSrcId

    measure :: IO (Either ClientError a) -> IO [NominalDiffTime]
    measure = measureApiLogs capture

    scene :: String -> IO (Either ClientError a) -> IO ()
    scene title scenario = measure scenario >>= fmtResult title

    runScenario scenario = runResourceT $ do
            (wal1, wal2, walMA, maWalletToMigrate) <- scenario
            let wal1Id = wal1 ^. #id
            liftIO $ do
                scene "listWallets" $ req C.listWallets
                scene "getWallet" $ req $ C.getWallet wal1Id
                scene "getUTxOsStatistics" $ req $ C.getWalletUtxoStatistics wal1Id
                scene "listAddresses"
                    $ req
                    $ C.listAddresses wal1Id Nothing
                scene "listTransactions"
                    $ req
                    $ C.listTransactions
                        wal1Id
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        False
                Right (tx : _) <-
                    req
                        $ C.listTransactions
                            wal1Id
                            Nothing
                            Nothing
                            Nothing
                            Nothing
                            Nothing
                            Nothing
                            False
                let txid = tx ^. #id
                scene "getTransaction"
                    $ req
                    $ C.getTransaction wal1Id (ApiTxId txid) False

                (_, addrs) <- unsafeRequest @[ApiAddressWithPath A] ctx (Link.listAddresses @'Shelley wal2) Empty
                let amt = minUTxOValue era
                let destination = (addrs !! 1) ^. #id
                let payload =
                        Json
                            [json|{
                    "payments": [{
                        "address": #{destination},
                        "amount": {
                            "quantity": #{amt},
                            "unit": "lovelace"
                        }
                    }]
                }|]
                t6 <-
                    measureApiLogs capture
                        $ request @ApiFee
                            ctx
                            (Link.getTransactionFeeOld @'Shelley wal1)
                            Default
                            payload
                fmtResult "postTransactionFee " t6

                let payloadTx =
                        Json
                            [json|{
                    "payments": [{
                        "address": #{destination},
                        "amount": {
                            "quantity": #{amt},
                            "unit": "lovelace"
                        }
                    }],
                    "passphrase": #{fixturePassphrase}
                }|]
                t7 <-
                    measureApiLogs capture
                        $ request @(ApiTransaction A)
                            ctx
                            (Link.createTransactionOld @'Shelley wal1)
                            Default
                            payloadTx
                fmtResult "postTransaction    " t7

                let addresses = replicate 5 destination
                let coins = replicate 5 amt
                let payments = flip map (zip coins addresses) $ \(amount, address) ->
                        [json|{
                    "address": #{address},
                    "amount": {
                        "quantity": #{amount},
                        "unit": "lovelace"
                    }
                }|]
                let payloadTxTo5Addr =
                        Json
                            [json|{
                    "payments": #{payments :: [Value]},
                    "passphrase": #{fixturePassphrase}
                }|]

                t7a <-
                    measureApiLogs capture
                        $ request @(ApiTransaction A)
                            ctx
                            (Link.createTransactionOld @'Shelley wal2)
                            Default
                            payloadTxTo5Addr
                fmtResult "postTransTo5Addrs  " t7a

                let assetsToSend = walMA ^. #assets . #total
                let val = minUTxOValue era <$ pickAnAsset assetsToSend
                payloadMA <- mkTxPayloadMA @A destination (2 * minUTxOValue era) [val] fixturePassphrase
                t7b <-
                    measureApiLogs capture
                        $ request @(ApiTransaction A)
                            ctx
                            (Link.createTransactionOld @'Shelley walMA)
                            Default
                            payloadMA
                fmtResult "postTransactionMA  " t7b

                t8 <-
                    measureApiLogs capture
                        $ request @[ApiT StakePool]
                            ctx
                            (Link.listStakePools arbitraryStake)
                            Default
                            Empty
                fmtResult "listStakePools     " t8

                t9 <-
                    measureApiLogs capture
                        $ request @ApiNetworkInformation
                            ctx
                            Link.getNetworkInfo
                            Default
                            Empty
                fmtResult "getNetworkInfo     " t9

                t10 <-
                    measureApiLogs capture
                        $ request @([ApiAsset])
                            ctx
                            (Link.listAssets walMA)
                            Default
                            Empty
                fmtResult "listMultiAssets    " t10

                let assetsSrc = walMA ^. #assets . #total
                let (polId, assName) =
                        bimap unsafeFromText unsafeFromText
                            $ fst
                            $ pickAnAsset assetsSrc
                t11 <-
                    measureApiLogs capture
                        $ request @([ApiAsset])
                            ctx
                            (Link.getAsset walMA polId assName)
                            Default
                            Empty
                fmtResult "getMultiAsset      " t11

                -- Create a migration plan:
                let endpointPlan = (Link.createMigrationPlan @'Shelley maWalletToMigrate)
                t12a <-
                    measureApiLogs capture
                        $ request @(ApiWalletMigrationPlan A)
                            ctx
                            endpointPlan
                            Default
                        $ Json [json|{addresses: #{addresses}}|]
                fmtResult "postMigrationPlan  " t12a

                -- Perform a migration:
                let endpointMigrate = Link.migrateWallet @'Shelley maWalletToMigrate
                t12b <-
                    measureApiLogs capture
                        $ request @[ApiTransaction A]
                            ctx
                            endpointMigrate
                            Default
                        $ Json
                            [json|
                    { passphrase: #{fixturePassphrase}
                    , addresses: #{addresses}
                    }|]
                fmtResult "postMigration      " t12b
      where
        arbitraryStake :: Maybe Coin
        arbitraryStake = Just $ ada 10_000
          where
            ada = Coin . (1_000_000 *)

    runWarmUpScenario = do
        -- this one is to have comparable results from first to last measurement
        -- in runScenario
        t <-
            measureApiLogs capture
                $ request @ApiNetworkInformation
                    ctx
                    Link.getNetworkInfo
                    Default
                    Empty
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
