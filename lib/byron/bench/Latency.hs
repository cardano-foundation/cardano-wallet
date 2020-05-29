{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
import Cardano.Launcher
    ( ProcessHasExited (..) )
import Cardano.Startup
    ( withUtf8Encoding )
import Cardano.Wallet.Api.Server
    ( Listen (..) )
import Cardano.Wallet.Api.Types
    ( ApiAddress
    , ApiByronWallet
    , ApiFee
    , ApiNetworkInformation
    , ApiTransaction
    , ApiUtxoStatistics
    , WalletStyle (..)
    )
import Cardano.Wallet.Byron
    ( SomeNetworkDiscriminant (..)
    , Tracers
    , Tracers' (..)
    , nullTracers
    , serveWallet
    )
import Cardano.Wallet.Byron.Compatibility
    ( Byron )
import Cardano.Wallet.Byron.Faucet
    ( initFaucet )
import Cardano.Wallet.Byron.Launch
    ( withCardanoNode, withCardanoSelfNode )
import Cardano.Wallet.LatencyBenchShared
    ( LogCaptureFunc, fmtResult, fmtTitle, measureApiLogs, withLatencyLogging )
import Cardano.Wallet.Logging
    ( trMessage )
import Cardano.Wallet.Network.Ports
    ( unsafePortNumber )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance (..) )
import Control.Concurrent.Async
    ( race )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Concurrent.STM.TVar
    ( TVar )
import Control.Exception
    ( throwIO )
import Control.Monad
    ( mapM_, replicateM, replicateM_ )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Fmt
    ( build, fmtLn )
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
import System.IO.Temp
    ( withSystemTempDirectory )
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
    , fixtureIcarusWallet
    , fixtureIcarusWalletWith
    , fixturePassphrase
    , fixtureRandomWallet
    , fixtureRandomWalletWith
    , json
    , request
    , unsafeRequest
    , verify
    )
import Test.Utils.Paths
    ( getTestData )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

main :: forall t n. (t ~ Byron, n ~ 'Mainnet) => IO ()
main = withUtf8Encoding $
    withLatencyLogging setupTracers $ \tracers capture -> do
        let srv = benchWithByronServer tracers
        mapM_ (walletApiBench @t @n capture srv)
            [ RandomWallets
            , IcarusWallets
            ]

  where
    setupTracers :: TVar [LogObject ApiLog] -> Tracers IO
    setupTracers tvar = nullTracers
        { apiServerTracer = trMessage $ contramap snd (traceInTVarIO tvar) }

data FixtureWallets
    = RandomWallets
    | IcarusWallets
    deriving (Show, Eq, Ord)

fixtureWalletTitle :: FixtureWallets -> Text
fixtureWalletTitle RandomWallets = "Random wallets"
fixtureWalletTitle IcarusWallets = "Icarus wallets"

type MakeWalletFixture t = Context t -> IO ApiByronWallet

walletApiBench
    :: forall t (n :: NetworkDiscriminant). (t ~ Byron, n ~ 'Mainnet)
    => LogCaptureFunc ApiLog ()
    -> ((Context t -> IO ()) -> IO ())
    -> FixtureWallets
    -> IO ()
walletApiBench capture benchWithServer walletName = do
    fmtLn "\n"
    fmtLn $ build $ fixtureWalletTitle walletName

    fmtTitle "Non-cached run"
    runWarmUpScenario

    fmtTitle "Latencies for 2 fixture wallets scenario"
    runScenario (nFixtureWallet 2)

    fmtTitle "Latencies for 10 fixture wallets scenario"
    runScenario (nFixtureWallet 10)

    fmtTitle "Latencies for 100 fixture wallets scenario"
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
    fixtureWallet = case walletName of
        RandomWallets -> fixtureRandomWallet
        IcarusWallets -> fixtureIcarusWallet

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

    nFixtureWalletWithUTxOs n utxoNumber ctx = do
        let utxoExp = replicate utxoNumber 1
        wal1 <- case walletName of
            RandomWallets ->
                fixtureRandomWalletWith @n ctx utxoExp
            IcarusWallets ->
                fixtureIcarusWalletWith @n ctx utxoExp
        (_, wal2) <- nFixtureWallet n ctx

        eventually "Wallet balance is as expected" $ do
            rWal1 <- request @ApiByronWallet  ctx
                (Link.getWallet @'Byron wal1) Default Empty
            verify rWal1
                [ expectSuccess
                , expectField
                        (#balance . #available . #getQuantity)
                        (`shouldBe` fromIntegral utxoNumber)
                ]

        rStat <- request @ApiUtxoStatistics ctx
                (Link.getUTxOsStatistics @'Byron wal1) Default Empty
        expectResponseCode @IO HTTP.status200 rStat
        expectWalletUTxO (fromIntegral <$> utxoExp) (snd rStat)
        pure (wal1, wal2)

    repeatPostTx ctx wDest amtToSend batchSize amtExp = do
        wSrc <- fixtureRandomWallet ctx
        replicateM_ batchSize
            (postTx ctx (wSrc, Link.createTransaction @'Byron, fixturePassphrase) wDest amtToSend)
        eventually "repeatPostTx: wallet balance is as expected" $ do
            rWal1 <- request @ApiByronWallet  ctx (Link.getWallet @'Byron wDest) Default Empty
            verify rWal1
                [ expectSuccess
                , expectField
                    (#balance . #available . #getQuantity)
                    (`shouldBe` amtExp)
                ]
        rDel <- request @ApiByronWallet  ctx (Link.deleteWallet @'Byron wSrc) Default Empty
        expectResponseCode @IO HTTP.status204 rDel
        pure ()

    postTx ctx (wSrc, postTxEndp, pass) wDest amt = do
        (_, addrs) <- unsafeRequest @[ApiAddress n] ctx
                      (Link.listAddresses @'Byron wDest) Empty
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

    runScenario scenario = benchWithServer $ \ctx -> do
        (wal1, wal2) <- scenario ctx

        t1 <- measureApiLogs capture
            (request @[ApiByronWallet] ctx (Link.listWallets @'Byron) Default Empty)
        fmtResult "listWallets        " t1

        t2 <- measureApiLogs capture
            (request @ApiByronWallet  ctx (Link.getWallet @'Byron wal1) Default Empty)
        fmtResult "getWallet          " t2

        t3 <- measureApiLogs capture
            (request @ApiUtxoStatistics ctx (Link.getUTxOsStatistics @'Byron wal1) Default Empty)
        fmtResult "getUTxOsStatistics " t3

        t4 <- measureApiLogs capture
            (request @[ApiAddress n] ctx (Link.listAddresses @'Byron wal1) Default Empty)
        fmtResult "listAddresses      " t4

        t5 <- measureApiLogs capture
            (request @[ApiTransaction n] ctx (Link.listTransactions @'Byron wal1) Default Empty)
        fmtResult "listTransactions   " t5

        (_, addrs) <- unsafeRequest @[ApiAddress n] ctx (Link.listAddresses @'Byron wal2) Empty
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
            (Link.getTransactionFee @'Byron wal1) Default payload
        fmtResult "postTransactionFee " t6

        t7 <- measureApiLogs capture $ request @ApiNetworkInformation ctx
            Link.getNetworkInfo Default Empty
        fmtResult "getNetworkInfo     " t7

        pure ()

    runWarmUpScenario = benchWithServer $ \ctx -> do
        -- this one is to have comparable results from first to last measurement
        -- in runScenario
        t <- measureApiLogs capture $ request @ApiNetworkInformation ctx
            Link.getNetworkInfo Default Empty
        fmtResult "getNetworkInfo     " t
        pure ()

benchWithByronServer
    :: Tracers IO
    -> (Context Byron -> IO ())
    -> IO ()
benchWithByronServer tracers action = do
    ctx <- newEmptyMVar
    let setupContext np wAddr = do
            let baseUrl = "http://" <> T.pack (show wAddr) <> "/"
            let sixtySeconds = 60*1000*1000 -- 60s in microseconds
            manager <- (baseUrl,) <$> newManager (defaultManagerSettings
                { managerResponseTimeout =
                    responseTimeoutMicro sixtySeconds
                })
            faucet <- initFaucet
            putMVar ctx $ Context
                { _cleanup = pure ()
                , _manager = manager
                , _walletPort = Port . fromIntegral $ unsafePortNumber wAddr
                , _faucet = faucet
                , _feeEstimator = \_ -> error "feeEstimator not available"
                , _networkParameters = np
                , _target = Proxy
                }
    race (takeMVar ctx >>= action) (withServer setupContext) >>=
        either pure (throwIO . ProcessHasExited "integration")
  where
    withServer act =
        withCardanoSelfNode nullTracer $(getTestData) Error $ \socketPath block0 (gp,vData) ->
        withSystemTempDirectory "cardano-wallet-databases" $ \db -> do
            serveWallet
                (SomeNetworkDiscriminant $ Proxy @'Mainnet)
                tracers
                (SyncTolerance 10)
                (Just db)
                "127.0.0.1"
                ListenOnRandomPort
                Nothing
                socketPath
                block0
                (gp, vData)
                (act gp)
