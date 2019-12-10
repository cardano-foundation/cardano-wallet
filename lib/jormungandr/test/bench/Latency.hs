{-# LANGUAGE BangPatterns #-}
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

module Main where

import Prelude

import Cardano.BM.Configuration.Static
    ( defaultConfigStdout )
import Cardano.BM.Data.LogItem
    ( LOContent (..), LOMeta (..), LogObject (..) )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Trace
    ( Trace, traceInTVarIO )
import Cardano.Faucet
    ( initFaucet, sockAddrPort )
import Cardano.Launcher
    ( ProcessHasExited (..), withUtf8Encoding )
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
    )
import Cardano.Wallet.Jormungandr
    ( ServerLog (..), serveWallet )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Jormungandr.Launch
    ( withConfig )
import Cardano.Wallet.Jormungandr.Network
    ( JormungandrBackend (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Model
    ( BlockchainParameters (..) )
import Cardano.Wallet.Primitive.Types
    ( SyncTolerance (..) )
import Control.Concurrent.Async
    ( race )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Concurrent.STM.TVar
    ( TVar, newTVarIO, readTVarIO, writeTVar )
import Control.Exception
    ( throwIO )
import Control.Monad
    ( mapM_, replicateM, replicateM_ )
import Control.Monad.STM
    ( atomically )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Maybe
    ( mapMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Time
    ( NominalDiffTime )
import Data.Time.Clock
    ( diffUTCTime )
import Data.Time.Clock.System
    ( SystemTime (..), getSystemTime )
import Fmt
    ( Builder, build, fixedF, fmtLn, (+|), (|+) )
import Network.HTTP.Client
    ( defaultManagerSettings
    , managerResponseTimeout
    , newManager
    , responseTimeoutMicro
    )
import Network.Wai.Middleware.Logging
    ( ApiLog (..), HandlerLog (..) )
import Numeric.Natural
    ( Natural )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , balanceAvailable
    , deleteWalletEp
    , expectEventually
    , expectResponseCode
    , expectSuccess
    , faucetAmt
    , fixtureWallet
    , getAddressesEp
    , getWalletEp
    , getWalletUtxoEp
    , json
    , listAddresses
    , listStakePoolsEp
    , listTxEp
    , listWalletsEp
    , networkInfoEp
    , postTxEp
    , postTxFeeEp
    , request
    , verify
    )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

main :: forall t n. (t ~ Jormungandr, n ~ 'Testnet) => IO ()
main = do
    tvar <- newTVarIO []
    logging <- setupLatencyLogging tvar

    fmtLn "Latencies for 2 fixture wallets scenario"
    runScenario logging tvar (nFixtureWallet 2)

    fmtLn "Latencies for 10 fixture wallets scenario"
    runScenario logging tvar (nFixtureWallet 10)

    fmtLn "Latencies for 100 fixture wallets scenario"
    runScenario logging tvar (nFixtureWallet 100)

    fmtLn "Latencies for 2 fixture wallets with 10 txs scenario"
    runScenario logging tvar (nFixtureWalletWithTxs 2 10)

    fmtLn "Latencies for 2 fixture wallets with 20 txs scenario"
    runScenario logging tvar (nFixtureWalletWithTxs 2 20)

    fmtLn "Latencies for 2 fixture wallets with 100 txs scenario"
    runScenario logging tvar (nFixtureWalletWithTxs 2 100)

    fmtLn "Latencies for 10 fixture wallets with 10 txs scenario"
    runScenario logging tvar (nFixtureWalletWithTxs 10 10)

    fmtLn "Latencies for 10 fixture wallets with 20 txs scenario"
    runScenario logging tvar (nFixtureWalletWithTxs 10 20)

    fmtLn "Latencies for 10 fixture wallets with 100 txs scenario"
    runScenario logging tvar (nFixtureWalletWithTxs 10 100)
  where
    nFixtureWallet n ctx = do
        wal1 : wal2 : _ <- replicateM n (fixtureWallet ctx)
        pure (wal1, wal2)

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
        mapM_ (repeatPostTx ctx wal1 amt batchSize . amtExp) expInflows

        pure (wal1, wal2)

    repeatPostTx ctx wDest amtToSend batchSize amtExp = do
        wSrc <- fixtureWallet ctx
        let pass = "cardano-wallet" :: Text

        replicateM_ batchSize (postTx ctx (wSrc, postTxEp, pass) wDest amtToSend)

        rWal1 <- request @ApiWallet ctx (getWalletEp wDest) Default Empty
        verify rWal1
            [ expectSuccess
            , expectEventually ctx getWalletEp balanceAvailable amtExp
            ]

        rDel <- request @ApiWallet ctx (deleteWalletEp wSrc) Default Empty
        expectResponseCode @IO HTTP.status204 rDel

        pure ()

    postTx ctx (wSrc, postTxEndp, pass) wDest amt = do
        addrs <- listAddresses ctx wDest
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

    runScenario logging tvar scenario =
        withUtf8Encoding $ benchWithServer logging $ \ctx -> do
        (wal1, wal2) <- scenario ctx

        t1 <- measureApiLogs tvar
            (request @[ApiWallet] ctx listWalletsEp Default Empty)

        fmtResult "listWallets        " t1

        t2 <- measureApiLogs tvar
            (request @ApiWallet ctx (getWalletEp wal1) Default Empty)

        fmtResult "getWallet          " t2

        t3 <- measureApiLogs tvar
            (request @ApiUtxoStatistics ctx (getWalletUtxoEp wal1) Default Empty)

        fmtResult "getUTxOsStatistics " t3

        t4 <- measureApiLogs tvar
            (request @[ApiAddress n] ctx (getAddressesEp wal1 "") Default Empty)

        fmtResult "listAddresses      " t4

        t5 <- measureApiLogs tvar
            (request @[ApiTransaction n] ctx (listTxEp wal1 "") Default Empty)

        fmtResult "listTransactions   " t5

        addrs <- listAddresses ctx wal2
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
        t6 <- measureApiLogs tvar
            (request @ApiFee ctx (postTxFeeEp wal1) Default payload)

        fmtResult "postTransactionFee " t6

        t7 <- measureApiLogs tvar
            (request @[ApiStakePool] ctx listStakePoolsEp Default Empty)

        fmtResult "listStakePools     " t7

        t8 <- measureApiLogs tvar
            (request @ApiNetworkInformation ctx networkInfoEp Default Empty)

        fmtResult "getNetworkInfo     " t8

        pure ()

meanAvg :: [NominalDiffTime] -> Double
meanAvg ts = sum (map realToFrac ts) * 1000 / fromIntegral (length ts)

buildResult :: [NominalDiffTime] -> Builder
buildResult [] = "ERR"
buildResult ts = build $ fixedF 1 $ meanAvg ts

fmtResult :: String -> [NominalDiffTime] -> IO ()
fmtResult title ts = fmtLn ("    "+|title|+" - "+|buildResult ts|+" ms")

isLogRequestStart :: ServerLog -> Bool
isLogRequestStart = \case
    LogApiServerMsg (ApiLog _ LogRequestStart) -> True
    _ -> False

isLogRequestFinish :: ServerLog -> Bool
isLogRequestFinish = \case
    LogApiServerMsg (ApiLog _ LogRequestFinish) -> True
    _ -> False

measureApiLogs
    :: TVar [LogObject ServerLog] -- ^ Log message variable.
    -> IO a -- ^ Action to run
    -> IO [NominalDiffTime]
measureApiLogs = measureLatency isLogRequestStart isLogRequestFinish

-- | Run tests for at least this long to get accurate timings.
sampleTimeSeconds :: Int
sampleTimeSeconds = 5

-- | Measure how long an action takes based on trace points and taking an
-- average of results over a short time period.
measureLatency
    :: (msg -> Bool) -- ^ Predicate for start message
    -> (msg -> Bool) -- ^ Predicate for end message
    -> TVar [LogObject msg] -- ^ Log message variable.
    -> IO a -- ^ Action to run
    -> IO [NominalDiffTime]
measureLatency start finish tvar action = do
    atomically $ writeTVar tvar []
    _res <- repeatFor sampleTimeSeconds action
    extractTimings start finish . reverse <$> readTVarIO tvar

-- | Scan through iohk-monitoring logs and extract time differences between
-- start and end messages.
extractTimings
    :: (a -> Bool) -- ^ Predicate for start message
    -> (a -> Bool) -- ^ Predicate for end message
    -> [LogObject a] -- ^ Log messages
    -> [NominalDiffTime]
extractTimings isStart isFinish msgs = map2 mkDiff filtered
  where
    map2 _ [] = []
    map2 f (a:b:xs) = (f a b:map2 f xs)
    map2 _ _ = error "start trace without matching finish trace"

    mkDiff (False, start) (True, finish) = diffUTCTime finish start
    mkDiff (False, _) _ = error "missing finish trace"
    mkDiff (True, _) _ = error "missing start trace"

    filtered = mapMaybe filterMsg msgs
    filterMsg logObj = case loContent logObj of
        LogMessage msg | isStart msg -> Just (False, getTimestamp logObj)
        LogMessage msg | isFinish msg -> Just (True, getTimestamp logObj)
        _ -> Nothing
    getTimestamp = tstamp . loMeta

-- | Repeatedly run an action, until total elapsed time in seconds is greater
-- than the given amount.
repeatFor :: Int -> IO a -> IO [a]
repeatFor nSeconds action = do
    start <- getSystemTime
    let repeater rs = do
            now <- getSystemTime
            if finished start now
                then pure rs
                else do
                    !r <- action
                    repeater (r:rs)
    reverse <$> repeater []
  where
    finished a b = systemSeconds b - systemSeconds a > fromIntegral nSeconds

setupLatencyLogging
    :: TVar [LogObject ServerLog]
    -> IO (CM.Configuration, Trace IO ServerLog)
setupLatencyLogging tvar = do
    cfg <- do
        cfg' <- defaultConfigStdout
        CM.setMinSeverity cfg' Debug
        pure cfg'
    pure (cfg, traceInTVarIO tvar)

benchWithServer
    :: (CM.Configuration, Trace IO ServerLog)
    -> (Context Jormungandr -> IO ())
    -> IO ()
benchWithServer logCfg = withContext
  where
    withContext :: (Context Jormungandr -> IO ()) -> IO ()
    withContext action = do
        ctx <- newEmptyMVar
        let setupContext wAddr nPort bp = do
                let baseUrl = "http://" <> T.pack (show wAddr) <> "/"
                let sixtySeconds = 60*1000*1000 -- 60s in microseconds
                manager <- (baseUrl,) <$> newManager (defaultManagerSettings
                    { managerResponseTimeout =
                        responseTimeoutMicro sixtySeconds
                    })
                faucet <- initFaucet (getFeePolicy bp)
                putMVar ctx $ Context
                    { _cleanup = pure ()
                    , _manager = manager
                    , _nodePort = nPort
                    , _walletPort = sockAddrPort wAddr
                    , _faucet = faucet
                    , _feeEstimator = \_ -> error "feeEstimator not available"
                    , _target = Proxy
                    }
        race (takeMVar ctx >>= action) (withServer setupContext) >>=
            either pure (throwIO . ProcessHasExited "latency benchmark")

    withServer setup = withConfig $ \jmCfg ->
        serveWallet @'Testnet logCfg (SyncTolerance 10) Nothing "127.0.0.1"
            ListenOnRandomPort (Launch jmCfg) setup
