{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main
    ( main
    ) where

import Prelude

import Cardano.BM.Data.LogItem
    ( LogObject (..) )
import Cardano.BM.Data.Tracer
    ( contramap )
import Cardano.BM.Trace
    ( traceInTVarIO )
import Cardano.CLI
    ( Port (..) )
import Cardano.Launcher
    ( ProcessHasExited (..) )
import Cardano.Startup
    ( withUtf8Encoding )
import Cardano.Wallet.Api.Server
    ( Listen (..) )
import Cardano.Wallet.Api.Types
    ( ApiAddress
    , ApiFee
    , ApiNetworkInformation
    , ApiStakePool
    , ApiTransaction
    , ApiUtxoStatistics
    , ApiWallet
    , WalletStyle (..)
    )
import Cardano.Wallet.Jormungandr
    ( Tracers, Tracers' (..), nullTracers, serveWallet )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Jormungandr.Faucet
    ( initFaucet )
import Cardano.Wallet.Jormungandr.Launch
    ( withConfig )
import Cardano.Wallet.Jormungandr.Network
    ( JormungandrBackend (..) )
import Cardano.Wallet.LatencyBenchShared
    ( LogCaptureFunc, fmtResult, measureApiLogs, withLatencyLogging )
import Cardano.Wallet.Logging
    ( trMessage )
import Cardano.Wallet.Network.Ports
    ( unsafePortNumber )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance (..) )
import Cardano.Wallet.Primitive.Types
    ( NetworkParameters (..), ProtocolParameters (..), TxParameters (..) )
import Control.Concurrent.Async
    ( race_ )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Concurrent.STM.TVar
    ( TVar )
import Control.Exception
    ( throwIO )
import Control.Monad
    ( mapM_, replicateM, replicateM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Data.Aeson
    ( Value )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Fmt
    ( fmtLn )
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
import Test.Hspec
    ( shouldBe )
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
    , fixturePassphrase
    , fixtureWallet
    , fixtureWalletWith
    , json
    , listAddresses
    , request
    , runResourceT
    , runResourceT
    , verify
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

main :: forall t n. (t ~ Jormungandr, n ~ 'Testnet 0) => IO ()
main = withUtf8Encoding $
    withLatencyLogging setupTracers $ \tracers capture ->
        walletApiBench @t @n capture (benchWithJormServer tracers)
  where
    setupTracers :: TVar [LogObject ApiLog] -> Tracers IO
    setupTracers tvar = nullTracers
        { apiServerTracer = trMessage $ contramap snd (traceInTVarIO tvar) }

walletApiBench
    :: forall t (n :: NetworkDiscriminant). (t ~ Jormungandr, n ~ 'Testnet 0)
    => LogCaptureFunc ApiLog ()
    -> ((Context t -> IO ()) -> IO ())
    -> IO ()
walletApiBench capture benchWithServer = do
    fmtLn "Non-cached run"
    runWarmUpScenario

    fmtLn "Latencies for 2 fixture wallets scenario"
    runScenario (nFixtureWallet 2)

    fmtLn "Latencies for 10 fixture wallets scenario"
    runScenario (nFixtureWallet 10)

    fmtLn "Latencies for 100 fixture wallets scenario"
    runScenario (nFixtureWallet 100)

    fmtLn "Latencies for 2 fixture wallets with 10 txs scenario"
    runScenario (nFixtureWalletWithTxs 2 10)

    fmtLn "Latencies for 2 fixture wallets with 20 txs scenario"
    runScenario (nFixtureWalletWithTxs 2 20)

    fmtLn "Latencies for 2 fixture wallets with 100 txs scenario"
    runScenario (nFixtureWalletWithTxs 2 100)

    fmtLn "Latencies for 10 fixture wallets with 10 txs scenario"
    runScenario (nFixtureWalletWithTxs 10 10)

    fmtLn "Latencies for 10 fixture wallets with 20 txs scenario"
    runScenario (nFixtureWalletWithTxs 10 20)

    fmtLn "Latencies for 10 fixture wallets with 100 txs scenario"
    runScenario (nFixtureWalletWithTxs 10 100)

    fmtLn "Latencies for 2 fixture wallets with 100 utxos scenario"
    runScenario (nFixtureWalletWithUTxOs 2 1)

    fmtLn "Latencies for 2 fixture wallets with 200 utxos scenario"
    runScenario (nFixtureWalletWithUTxOs 2 2)

    fmtLn "Latencies for 2 fixture wallets with 500 utxos scenario"
    runScenario (nFixtureWalletWithUTxOs 2 5)

    fmtLn "Latencies for 2 fixture wallets with 1000 utxos scenario"
    runScenario (nFixtureWalletWithUTxOs 2 10)
  where
    -- Creates n fixture wallets and return two of them
    nFixtureWallet n ctx = do
        wal1 : wal2 : _ <- replicateM n (fixtureWallet ctx)
        pure (wal1, wal2)

    -- Creates n fixture wallets and send 1-lovelace transactions to one of them
    -- (m times). The money is sent in batches (see batchSize below) from
    -- additionally created source fixture wallet. Then we wait for the money
    -- to be accommodated in recipient wallet. After that the source fixture
    -- wallet is removed.
    nFixtureWalletWithTxs n m ctx = do
        (wal1, wal2) <- nFixtureWallet n ctx

        let amt = (1 :: Natural)
        let batchSize = 10
        let whole10Rounds = div m batchSize
        let lastBit = mod m batchSize
        let amtExp val = fromIntegral ((fromIntegral val) + faucetAmt) :: Natural
        let expInflows =
                if whole10Rounds > 0 then
                    [x*batchSize | x<-[1..whole10Rounds]] ++ [lastBit]
                else
                    [lastBit]
        let expInflows' = filter (/=0) expInflows
        mapM_ (repeatPostTx ctx wal1 amt batchSize . amtExp) expInflows'

        pure (wal1, wal2)

    -- Creates n fixture wallets and send 10-output transactions
    -- to one of them (10*m times). The money is sent in batches (see batchSize
    -- below) from additionally created source fixture wallet. So m=1 means the
    -- recipient wallet should assume 100 additional utxos. Then we wait for
    -- the money to be accommodated in recipient wallet. After that the
    -- source fixture wallet is removed.
    nFixtureWalletWithUTxOs n m ctx = do
        (wal1, wal2) <- nFixtureWallet n ctx

        let amt = (1 :: Natural)
        let batchSize = 100
        let fixtureUtxos = replicate 10 100000000000
        let expAddedUtxos = [replicate (batchSize*x) 1 | x <- [1..m]]
        let expUtxos = map (++ fixtureUtxos) expAddedUtxos
        let expInflows = [(fromIntegral faucetAmt) + batchSize*x | x <- [1..m]]
        let expPair = zip expInflows expUtxos

        mapM_ (repeatPostMultiTx ctx wal1 amt batchSize) expPair

        pure (wal1, wal2)

    repeatPostTx ctx wDest amtToSend batchSize amtExp = do
        wSrc <- fixtureWallet ctx
        let pass = "cardano-wallet" :: Text

        replicateM_ batchSize
            (postTx ctx (wSrc, Link.createTransaction @'Shelley, pass) wDest amtToSend)
        liftIO $ eventually "repeatPostTx: wallet balance is as expected" $ do
            rWal1 <- request @ApiWallet ctx (Link.getWallet @'Shelley wDest) Default Empty
            verify rWal1
                [ expectSuccess
                , expectField
                    (#balance . #getApiT . #available . #getQuantity)
                    (`shouldBe` amtExp)
                ]

        rDel <- request @ApiWallet ctx (Link.deleteWallet @'Shelley wSrc) Default Empty
        expectResponseCode HTTP.status204 rDel

        pure ()

    postTx ctx (wSrc, postTxEndp, pass) wDest amt = do
        addrs <- listAddresses @n ctx wDest
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

    repeatPostMultiTx ctx wDest amtToSend batchSize (amtExp, utxoExp) = do
        wSrc <- fixtureWalletWith @n ctx (replicate batchSize 1000)

        postMultiTx ctx
            (wSrc, Link.createTransaction @'Shelley, fixturePassphrase)
            wDest amtToSend batchSize
        liftIO $ eventually "repeatPostMultiTx: wallet balance is as expected" $ do
            rWal1 <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            verify rWal1
                [ expectSuccess
                , expectField
                        (#balance . #getApiT . #available . #getQuantity)
                        (`shouldBe` fromIntegral amtExp)
                ]

        rStat <- request @ApiUtxoStatistics ctx (Link.getUTxOsStatistics @'Shelley  wDest) Default Empty
        expectResponseCode HTTP.status200 rStat
        expectWalletUTxO utxoExp (snd rStat)

        rDel <- request @ApiWallet ctx (Link.deleteWallet @'Shelley wSrc) Default Empty
        expectResponseCode HTTP.status204 rDel

        pure ()

    postMultiTx ctx (wSrc, postTxEndp, pass) wDest amt nOuts = do
        addrs <- listAddresses @n ctx wDest
        let destinations = replicate nOuts $ (addrs !! 1) ^. #id
        let amounts = take nOuts [amt, amt .. ]
        let payments = flip map (zip amounts destinations) $ \(coin, addr) ->
                [aesonQQ|{
                        "address": #{addr},
                        "amount": {
                            "quantity": #{coin},
                            "unit": "lovelace"
                        }
                        }|]
        let payload = Json [aesonQQ|{
                "payments": #{payments :: [Value]},
                "passphrase": #{pass}
            }|]
        r <- request @(ApiTransaction n) ctx (postTxEndp wSrc) Default payload

        expectResponseCode HTTP.status202 r
        return ()

    runScenario scenario = benchWithServer $ \ctx -> runResourceT $ scenario ctx >>= \(wal1, wal2) -> liftIO $ do
        t1 <- measureApiLogs capture
            (request @[ApiWallet] ctx (Link.listWallets @'Shelley) Default Empty)

        fmtResult "listWallets        " t1

        t2 <- measureApiLogs capture
            (request @ApiWallet ctx (Link.getWallet @'Shelley wal1) Default Empty)

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

        addrs <- listAddresses @n ctx wal2
        let amt = 1 :: Natural
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
            (Link.getTransactionFee @'Shelley wal1) Default payload

        fmtResult "postTransactionFee " t6

        t7 <- measureApiLogs capture $ request @[ApiStakePool] ctx
            Link.listJormungandrStakePools Default Empty

        fmtResult "listStakePools     " t7

        t8 <- measureApiLogs capture $ request @ApiNetworkInformation ctx
            Link.getNetworkInfo Default Empty

        fmtResult "getNetworkInfo     " t8

        pure ()

    runWarmUpScenario = benchWithServer $ \ctx -> do
        -- this one is to have comparable results from first to last measurement
        -- in runScenario
        t <- measureApiLogs capture $ request @ApiNetworkInformation ctx
            Link.getNetworkInfo Default Empty
        fmtResult "getNetworkInfo     " t
        pure ()

benchWithJormServer
    :: Tracers IO
    -> (Context Jormungandr -> IO ())
    -> IO ()
benchWithJormServer tracers action = withConfig $ \jmCfg -> do
    ctx <- newEmptyMVar
    race_ (takeMVar ctx >>= action) $ do
        res <- serveWallet @('Testnet 0)
            tracers (SyncTolerance 10)
            Nothing "127.0.0.1"
            ListenOnRandomPort (Launch jmCfg) $ \wAddr _ np -> do
                let baseUrl = "http://" <> T.pack (show wAddr) <> "/"
                let sixtySeconds = 60*1000*1000 -- 60s in microseconds
                manager <- (baseUrl,) <$> newManager (defaultManagerSettings
                    { managerResponseTimeout =
                        responseTimeoutMicro sixtySeconds
                    })
                faucet <- initFaucet
                    $ getFeePolicy
                    $ txParameters
                    $ protocolParameters np
                putMVar ctx $ Context
                    { _cleanup = pure ()
                    , _manager = manager
                    , _walletPort = Port . fromIntegral $ unsafePortNumber wAddr
                    , _faucet = faucet
                    , _networkParameters = np
                    , _feeEstimator = \_ -> error "feeEstimator not available"
                    , _target = Proxy
                    , _poolGarbageCollectionEvents =
                        error "poolGarbageCollectionEvents not available"
                    }
        throwIO $ ProcessHasExited "Server has unexpectedly exited" res
