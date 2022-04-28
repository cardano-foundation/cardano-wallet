{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude

import Cardano.BM.Data.LogItem
    ( LogObject )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( contramap, nullTracer )
import Cardano.BM.Trace
    ( traceInTVarIO )
import Cardano.CLI
    ( Port (..) )
import Cardano.Startup
    ( withUtf8Encoding )
import Cardano.Wallet.Api.Types
    ( ApiAddress
    , ApiAsset (..)
    , ApiEra
    , ApiFee
    , ApiNetworkInformation
    , ApiStakePool
    , ApiTransaction
    , ApiTxId (..)
    , ApiUtxoStatistics
    , ApiWallet
    , ApiWalletMigrationPlan (..)
    , EncodeAddress (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.LatencyBenchShared
    ( LogCaptureFunc, fmtResult, fmtTitle, measureApiLogs, withLatencyLogging )
import Cardano.Wallet.Logging
    ( trMessage )
import Cardano.Wallet.Network.Ports
    ( portFromURL )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Shelley
    ( SomeNetworkDiscriminant (..)
    , Tracers
    , Tracers' (..)
    , nullTracers
    , serveWallet
    )
import Cardano.Wallet.Shelley.BlockchainSource
    ( BlockchainSource (..) )
import Cardano.Wallet.Shelley.Faucet
    ( initFaucet )
import Cardano.Wallet.Shelley.Launch
    ( withSystemTempDir )
import Cardano.Wallet.Shelley.Launch.Cluster
    ( LocalClusterConfig (..)
    , LogFileConfig (..)
    , RunningNode (..)
    , sendFaucetAssetsTo
    , sendFaucetFundsTo
    , walletListenFromEnv
    , withCluster
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromText )
import Control.Arrow
    ( first )
import Control.Monad
    ( replicateM, replicateM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Data.Aeson
    ( Value )
import Data.Bifunctor
    ( bimap )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Proxy
    ( Proxy (..) )
import Network.HTTP.Client
    ( defaultManagerSettings
    , managerResponseTimeout
    , newManager
    , responseTimeoutMicro
    )
import Network.Wai.Middleware.Logging
    ( ApiLog (..) )
import Numeric.Natural
    ( Natural )
import System.Directory
    ( createDirectory )
import System.FilePath
    ( (</>) )
import Test.Hspec
    ( shouldBe )
import Test.Integration.Faucet
    ( maryIntegrationTestAssets, shelleyIntegrationTestFunds )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , eventually
    , expectField
    , expectResponseCode
    , expectSuccess
    , expectWalletUTxO
    , faucetAmt
    , fixtureMultiAssetWallet
    , fixturePassphrase
    , fixtureWallet
    , fixtureWalletWith
    , json
    , minUTxOValue
    , mkTxPayloadMA
    , pickAnAsset
    , request
    , runResourceT
    , unsafeRequest
    , verify
    )
import UnliftIO.Async
    ( race_ )
import UnliftIO.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import UnliftIO.STM
    ( TVar )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

main :: forall n. (n ~ 'Mainnet) => IO ()
main = withUtf8Encoding $
    withLatencyLogging setupTracers $ \tracers capture ->
        withShelleyServer tracers $ \ctx -> do
            walletApiBench @n capture ctx
  where
    setupTracers :: TVar [LogObject ApiLog] -> Tracers IO
    setupTracers tvar = nullTracers
        { apiServerTracer = trMessage $ contramap snd (traceInTVarIO tvar) }

walletApiBench
    :: forall (n :: NetworkDiscriminant). (n ~ 'Mainnet)
    => LogCaptureFunc ApiLog ()
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
    runScenario (nFixtureWalletWithUTxOs 2 1000)
  where

    -- Creates n fixture wallets and return 3 of them
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
                if whole10Rounds > 0 then
                    [x*batchSize | x<-[1..whole10Rounds]] ++ [lastBit]
                else
                    [lastBit]
        let expInflows' = filter (/=0) expInflows

        mapM_ (repeatPostTx wal1 amt batchSize . amtExp) expInflows'
        pure (wal1, wal2, walMA, maWalletToMigrate)

    nFixtureWalletWithUTxOs n utxoNumber = do
        let utxoExp = replicate utxoNumber (minUTxOValue era)
        wal1 <- fixtureWalletWith @n ctx utxoExp
        (_, wal2, walMA, maWalletToMigrate) <- nFixtureWallet n

        eventually "Wallet balance is as expected" $ do
            rWal1 <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wal1) Default Empty
            verify rWal1
                [ expectSuccess
                , expectField
                        (#balance . #available . #getQuantity)
                        (`shouldBe` ((minUTxOValue era) * (fromIntegral utxoNumber)))
                ]

        rStat <- request @ApiUtxoStatistics ctx
                (Link.getUTxOsStatistics @'Shelley wal1) Default Empty
        expectResponseCode HTTP.status200 rStat
        expectWalletUTxO (fromIntegral <$> utxoExp) (snd rStat)
        pure (wal1, wal2, walMA, maWalletToMigrate)

    repeatPostTx wDest amtToSend batchSize amtExp = do
        wSrc <- fixtureWallet ctx
        replicateM_ batchSize
            (postTx (wSrc, Link.createTransactionOld @'Shelley, fixturePassphrase) wDest amtToSend)
        eventually "repeatPostTx: wallet balance is as expected" $ do
            rWal1 <- request @ApiWallet  ctx (Link.getWallet @'Shelley wDest) Default Empty
            verify rWal1
                [ expectSuccess
                , expectField
                    (#balance . #available . #getQuantity)
                    (`shouldBe` amtExp)
                ]
        rDel <- request @ApiWallet  ctx (Link.deleteWallet @'Shelley wSrc) Default Empty
        expectResponseCode HTTP.status204 rDel
        pure ()

    postTx (wSrc, postTxEndp, pass) wDest amt = do
        (_, addrs) <- unsafeRequest @[ApiAddress n] ctx
            (Link.listAddresses @'Shelley wDest) Empty
        let destination = (addrs !! 1) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": #{pass}
            }|]
        r <- request @(ApiTransaction n) ctx (postTxEndp wSrc) Default payload
        expectResponseCode HTTP.status202 r
        return r

    runScenario scenario = runResourceT $ scenario >>= \(wal1, wal2, walMA, maWalletToMigrate) -> liftIO $ do
        t1 <- measureApiLogs capture
            (request @[ApiWallet] ctx (Link.listWallets @'Shelley) Default Empty)
        fmtResult "listWallets        " t1

        t2 <- measureApiLogs capture
            (request @ApiWallet  ctx (Link.getWallet @'Shelley wal1) Default Empty)
        fmtResult "getWallet          " t2

        t3 <- measureApiLogs capture
            (request @ApiUtxoStatistics ctx (Link.getUTxOsStatistics @'Shelley wal1) Default Empty)
        fmtResult "getUTxOsStatistics " t3

        t4 <- measureApiLogs capture
            (request @[ApiAddress n] ctx (Link.listAddresses @'Shelley wal1) Default Empty)
        fmtResult "listAddresses      " t4

        t5 <- measureApiLogs capture
            (request @[ApiTransaction n] ctx (Link.listTransactions @'Shelley wal1) Default Empty)
        fmtResult "listTransactions   " t5

        (_, txs) <- unsafeRequest @[ApiTransaction n] ctx (Link.listTransactions @'Shelley wal1) Empty
        let txid = (head txs) ^. #id
        t5a <- measureApiLogs capture $
            request @[ApiTransaction n]
                ctx (Link.getTransaction @'Shelley wal1 (ApiTxId txid) False)
                Default
                Empty
        fmtResult "getTransaction     " t5a

        (_, addrs) <- unsafeRequest @[ApiAddress n] ctx (Link.listAddresses @'Shelley wal2) Empty
        let amt = minUTxOValue era
        let destination = (addrs !! 1) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }]
            }|]
        t6 <- measureApiLogs capture $ request @ApiFee ctx
            (Link.getTransactionFeeOld @'Shelley wal1) Default payload
        fmtResult "postTransactionFee " t6

        let payloadTx = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": #{fixturePassphrase}
            }|]
        t7 <- measureApiLogs capture $ request @(ApiTransaction n) ctx
            (Link.createTransactionOld @'Shelley wal1) Default payloadTx
        fmtResult "postTransaction    " t7

        let addresses = replicate 5 destination
        let coins = replicate 5 amt
        let payments = flip map (zip coins addresses) $ \(amount, address) -> [json|{
                "address": #{address},
                "amount": {
                    "quantity": #{amount},
                    "unit": "lovelace"
                }
            }|]
        let payloadTxTo5Addr = Json [json|{
                "payments": #{payments :: [Value]},
                "passphrase": #{fixturePassphrase}
            }|]

        t7a <- measureApiLogs capture $ request @(ApiTransaction n) ctx
            (Link.createTransactionOld @'Shelley wal2) Default payloadTxTo5Addr
        fmtResult "postTransTo5Addrs  " t7a

        let assetsToSend = walMA ^. #assets . #total . #getApiT
        let val = minUTxOValue era <$ pickAnAsset assetsToSend
        payloadMA <- mkTxPayloadMA @n destination (2 * minUTxOValue era) [val] fixturePassphrase
        t7b <- measureApiLogs capture $ request @(ApiTransaction n) ctx
            (Link.createTransactionOld @'Shelley walMA) Default payloadMA
        fmtResult "postTransactionMA  " t7b

        t8 <- measureApiLogs capture $ request @[ApiStakePool] ctx
            (Link.listStakePools arbitraryStake) Default Empty
        fmtResult "listStakePools     " t8

        t9 <- measureApiLogs capture $ request @ApiNetworkInformation ctx
            Link.getNetworkInfo Default Empty
        fmtResult "getNetworkInfo     " t9

        t10 <- measureApiLogs capture $ request @([ApiAsset]) ctx
            (Link.listAssets walMA) Default Empty
        fmtResult "listMultiAssets    " t10

        let assetsSrc = walMA ^. #assets . #total . #getApiT
        let (polId, assName) = bimap unsafeFromText unsafeFromText $ fst $
                pickAnAsset assetsSrc
        t11 <- measureApiLogs capture $ request @([ApiAsset]) ctx
            (Link.getAsset walMA polId assName) Default Empty
        fmtResult "getMultiAsset      " t11

        -- Create a migration plan:
        let endpointPlan = (Link.createMigrationPlan @'Shelley maWalletToMigrate)
        t12a <- measureApiLogs capture $ request @(ApiWalletMigrationPlan n)
            ctx endpointPlan Default $
            Json [json|{addresses: #{addresses}}|]
        fmtResult "postMigrationPlan  " t12a

        -- Perform a migration:
        let endpointMigrate = Link.migrateWallet @'Shelley maWalletToMigrate
        t12b <- measureApiLogs capture $ request @[ApiTransaction n]
            ctx endpointMigrate Default $
            Json [json|
                { passphrase: #{fixturePassphrase}
                , addresses: #{addresses}
                }|]
        fmtResult "postMigration      " t12b

        pure ()
     where
        arbitraryStake :: Maybe Coin
        arbitraryStake = Just $ ada 10000
            where ada = Coin . (1000*1000*)

    runWarmUpScenario = do
        -- this one is to have comparable results from first to last measurement
        -- in runScenario
        t <- measureApiLogs capture $ request @ApiNetworkInformation ctx
            Link.getNetworkInfo Default Empty
        fmtResult "getNetworkInfo     " t
        pure ()

withShelleyServer
    :: Tracers IO
    -> (Context -> IO ())
    -> IO ()
withShelleyServer tracers action = do
    ctx <- newEmptyMVar
    let setupContext np baseUrl = do
            let sixtySeconds = 60*1000*1000 -- 60s in microseconds
            manager <- (baseUrl,) <$> newManager (defaultManagerSettings
                { managerResponseTimeout =
                    responseTimeoutMicro sixtySeconds
                })
            faucet <- initFaucet
            putMVar ctx $ Context
                { _cleanup = pure ()
                , _manager = manager
                , _walletPort = Port . fromIntegral $ portFromURL baseUrl
                , _faucet = faucet
                , _feeEstimator = \_ -> error "feeEstimator not available"
                , _networkParameters = np
                , _poolGarbageCollectionEvents =
                    error "poolGarbageCollectionEvents not available"
                , _smashUrl = ""
                , _mainEra = maxBound
                , _mintSeaHorseAssets = error "mintSeaHorseAssets not available"
                , _moveRewardsToScript =
                    error "moveRewardsToScript not available"
                }
    race_ (takeMVar ctx >>= action) (withServer setupContext)

  where
    withServer act = withSystemTempDir nullTracer "latency" $ \dir -> do
            let db = dir </> "wallets"
            createDirectory db
            let logCfg = LogFileConfig Error Nothing Error
            let clusterCfg = LocalClusterConfig [] maxBound logCfg
            withCluster nullTracer dir clusterCfg (setupFaucet dir) $
                onClusterStart act db

    setupFaucet dir (RunningNode conn _ _) = do
        let encodeAddr = T.unpack . encodeAddress @'Mainnet
        let addresses = map (first encodeAddr) shelleyIntegrationTestFunds
        let addressesMA = map (first encodeAddr) (maryIntegrationTestAssets (Coin 10_000_000))
        sendFaucetFundsTo nullTracer conn dir addresses
        sendFaucetAssetsTo nullTracer conn dir 20 addressesMA

    onClusterStart act db (RunningNode conn block0 (np, vData)) = do
        listen <- walletListenFromEnv
        serveWallet
            (NodeSource conn vData)
            np
            (SomeNetworkDiscriminant $ Proxy @'Mainnet)
            tracers
            (SyncTolerance 10)
            (Just db)
            Nothing
            "127.0.0.1"
            listen
            Nothing
            Nothing
            Nothing
            block0
            (act np)

era :: ApiEra
era = maxBound
