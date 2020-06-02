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

import Cardano.BM.Backend.Switchboard
    ( effectuate )
import Cardano.BM.Configuration.Static
    ( defaultConfigStdout )
import Cardano.BM.Data.LogItem
    ( LOContent (..), LOMeta (..), LogObject (..) )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( ToObject (..), contramap )
import Cardano.BM.Setup
    ( setupTrace_, shutdown )
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
    , ApiJormungandrStakePool
    , ApiNetworkInformation
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
import Cardano.Wallet.Logging
    ( trMessage )
import Cardano.Wallet.Network.Ports
    ( unsafePortNumber )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( NetworkParameters (..)
    , ProtocolParameters (..)
    , SyncTolerance (..)
    , TxParameters (..)
    )
import Control.Concurrent.Async
    ( race_ )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Concurrent.STM.TVar
    ( TVar, newTVarIO, readTVarIO, writeTVar )
import Control.Exception
    ( bracket, onException, throwIO )
import Control.Monad
    ( mapM_, replicateM, replicateM_ )
import Control.Monad.STM
    ( atomically )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), Value )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Maybe
    ( mapMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( toText )
import Data.Time
    ( NominalDiffTime )
import Data.Time.Clock
    ( diffUTCTime )
import Fmt
    ( Builder, build, fixedF, fmtLn, padLeftF, (+|), (|+) )
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
    , fixtureWallet
    , fixtureWalletWith
    , json
    , listAddresses
    , request
    , verify
    )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

main :: forall t n. (t ~ Jormungandr, n ~ 'Testnet 0) => IO ()
main = withUtf8Encoding $ withLatencyLogging $ \logging tvar -> do

    fmtLn "Non-cached run"
    runBareScenario logging tvar

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

    fmtLn "Latencies for 2 fixture wallets with 100 utxos scenario"
    runScenario logging tvar (nFixtureWalletWithUTxOs 2 1)

    fmtLn "Latencies for 2 fixture wallets with 200 utxos scenario"
    runScenario logging tvar (nFixtureWalletWithUTxOs 2 2)

    fmtLn "Latencies for 2 fixture wallets with 500 utxos scenario"
    runScenario logging tvar (nFixtureWalletWithUTxOs 2 5)

    fmtLn "Latencies for 2 fixture wallets with 1000 utxos scenario"
    runScenario logging tvar (nFixtureWalletWithUTxOs 2 10)
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
        eventually "repeatPostTx: wallet balance is as expected" $ do
            rWal1 <- request @ApiWallet ctx (Link.getWallet @'Shelley wDest) Default Empty
            verify rWal1
                [ expectSuccess
                , expectField
                    (#balance . #getApiT . #available . #getQuantity)
                    (`shouldBe` amtExp)
                ]

        rDel <- request @ApiWallet ctx (Link.deleteWallet @'Shelley wSrc) Default Empty
        expectResponseCode @IO HTTP.status204 rDel

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
        let pass = "Secure Passphrase" :: Text

        postMultiTx ctx
            (wSrc, Link.createTransaction @'Shelley, pass) wDest amtToSend batchSize
        eventually "repeatPostMultiTx: wallet balance is as expected" $ do
            rWal1 <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            verify rWal1
                [ expectSuccess
                , expectField
                        (#balance . #getApiT . #available . #getQuantity)
                        (`shouldBe` fromIntegral amtExp)
                ]

        rStat <- request @ApiUtxoStatistics ctx (Link.getUTxOsStatistics @'Shelley  wDest) Default Empty
        expectResponseCode @IO HTTP.status200 rStat
        expectWalletUTxO utxoExp (snd rStat)

        rDel <- request @ApiWallet ctx (Link.deleteWallet @'Shelley wSrc) Default Empty
        expectResponseCode @IO HTTP.status204 rDel

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

    runScenario logging tvar scenario = benchWithServer logging $ \ctx -> do
        (wal1, wal2) <- scenario ctx

        t1 <- measureApiLogs tvar
            (request @[ApiWallet] ctx (Link.listWallets @'Shelley) Default Empty)

        fmtResult "listWallets        " t1

        t2 <- measureApiLogs tvar
            (request @ApiWallet ctx (Link.getWallet @'Shelley wal1) Default Empty)

        fmtResult "getWallet          " t2

        t3 <- measureApiLogs tvar
            (request @ApiUtxoStatistics ctx (Link.getUTxOsStatistics @'Shelley wal1) Default Empty)

        fmtResult "getUTxOsStatistics " t3

        t4 <- measureApiLogs tvar
            (request @[ApiAddress n] ctx (Link.listAddresses @'Shelley wal1) Default Empty)

        fmtResult "listAddresses      " t4

        t5 <- measureApiLogs tvar
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
        t6 <- measureApiLogs tvar $ request @ApiFee ctx
            (Link.getTransactionFee @'Shelley wal1) Default payload

        fmtResult "postTransactionFee " t6

        t7 <- measureApiLogs tvar $ request @[ApiJormungandrStakePool] ctx
            Link.listJormungandrStakePools Default Empty

        fmtResult "listStakePools     " t7

        t8 <- measureApiLogs tvar $ request @ApiNetworkInformation ctx
            Link.getNetworkInfo Default Empty

        fmtResult "getNetworkInfo     " t8

        pure ()

    runBareScenario logging tvar  = benchWithServer logging $ \ctx -> do
        -- this one is to have comparable results from first to last measurement
        -- in runScenario
        t <- measureApiLogs tvar $ request @ApiNetworkInformation ctx
            Link.getNetworkInfo Default Empty
        fmtResult "getNetworkInfo     " t
        pure ()

meanAvg :: [NominalDiffTime] -> Double
meanAvg ts = sum (map realToFrac ts) * 1000 / fromIntegral (length ts)

buildResult :: [NominalDiffTime] -> Builder
buildResult [] = "ERR"
buildResult ts = build $ fixedF 1 $ meanAvg ts

fmtResult :: String -> [NominalDiffTime] -> IO ()
fmtResult title ts =
    let titleExt = title|+" - " :: String
        titleF = padLeftF 25 ' ' titleExt
    in fmtLn (titleF+|buildResult ts|+" ms")

isLogRequestStart :: ApiLog -> Bool
isLogRequestStart = \case
    ApiLog _ LogRequestStart -> True
    _ -> False

isLogRequestFinish :: ApiLog -> Bool
isLogRequestFinish = \case
    ApiLog _ LogRequestFinish -> True
    _ -> False

measureApiLogs
    :: TVar [LogObject ApiLog] -- ^ Log message variable.
    -> IO a -- ^ Action to run
    -> IO [NominalDiffTime]
measureApiLogs = measureLatency isLogRequestStart isLogRequestFinish

-- | Run tests for at least this long to get accurate timings.
sampleTimeSeconds :: Int
sampleTimeSeconds = 5

-- | Run tests for at least this long to get accurate timings.
sampleNTimes :: Int
sampleNTimes = 10

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
    replicateM_ sampleNTimes action
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

withLatencyLogging
    ::(Tracers IO -> TVar [LogObject ApiLog] -> IO a)
    -> IO a
withLatencyLogging action = do
    tvar <- newTVarIO []
    cfg <- defaultConfigStdout
    CM.setMinSeverity cfg Debug
    bracket (setupTrace_ cfg "bench-latency") (shutdown . snd) $ \(_, sb) -> do
        action (setupTracers tvar) tvar `onException` do
            fmtLn "Action failed. Here are the captured logs:"
            readTVarIO tvar >>= mapM_ (effectuate sb) . reverse

setupTracers :: TVar [LogObject ApiLog] -> Tracers IO
setupTracers tvar = nullTracers
    { apiServerTracer = trMessage $ contramap snd (traceInTVarIO tvar) }

benchWithServer
    :: Tracers IO
    -> (Context Jormungandr -> IO ())
    -> IO ()
benchWithServer tracers action = withConfig $ \jmCfg -> do
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
                    }
        throwIO $ ProcessHasExited "Server has unexpectedly exited" res

instance ToJSON ApiLog where
    toJSON = toJSON . toText

instance FromJSON ApiLog where
    parseJSON _ = fail "FromJSON ApiLog stub"

instance ToObject ApiLog
