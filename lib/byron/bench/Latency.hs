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
    ( ToObject (..), contramap, nullTracer )
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
    ( withCardanoNode )
import Cardano.Wallet.Logging
    ( trMessage )
import Cardano.Wallet.Network.Ports
    ( unsafePortNumber )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( SyncTolerance (..) )
import Control.Concurrent.Async
    ( race )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Concurrent.STM.TVar
    ( TVar, newTVarIO, readTVarIO, writeTVar )
import Control.Exception
    ( bracket, onException, throwIO )
import Control.Monad
    ( forM_, mapM_, replicateM, replicateM_ )
import Control.Monad.STM
    ( atomically )
import Data.Aeson
    ( FromJSON (..), ToJSON (..) )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Maybe
    ( mapMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( toText )
import Data.Time
    ( NominalDiffTime )
import Data.Time.Clock
    ( diffUTCTime )
import Fmt
    ( Builder, build, fixedF, fmt, fmtLn, indentF, padLeftF, (+|), (|+) )
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
    , faucetAmt
    , fixtureIcarusWallet
    , fixturePassphrase
    , fixtureRandomWallet
    , json
    , request
    , unsafeRequest
    , verify
    )
import Test.Utils.Paths
    ( getTestData )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

main :: forall t n. (t ~ Byron, n ~ 'Mainnet) => IO ()
main = withUtf8Encoding $ withLatencyLogging $ \logging tvar ->
    forM_ [ (fixtureRandomWallet, "Random wallets")
          , (fixtureIcarusWallet, "Icarus wallets")] $
    \(fixtureByronWallet, walletName) -> do
             fmtLn "\n"
             fmtLn walletName

             fmtTitle "Latencies for 2 fixture wallets scenario"
             runScenario logging tvar (nFixtureWallet 2 fixtureByronWallet)

             fmtTitle "Latencies for 10 fixture wallets scenario"
             runScenario logging tvar (nFixtureWallet 10 fixtureByronWallet)

             fmtTitle "Latencies for 100 fixture wallets scenario"
             runScenario logging tvar (nFixtureWallet 100 fixtureByronWallet)

             fmtTitle "Latencies for 2 fixture wallets with 10 txs scenario"
             runScenario logging tvar (nFixtureWalletWithTxs 2 10 fixtureByronWallet)

             fmtTitle "Latencies for 2 fixture wallets with 20 txs scenario"
             runScenario logging tvar (nFixtureWalletWithTxs 2 20 fixtureByronWallet)

             fmtTitle "Latencies for 2 fixture wallets with 100 txs scenario"
             runScenario logging tvar (nFixtureWalletWithTxs 2 100 fixtureByronWallet)

             fmtTitle "Latencies for 10 fixture wallets with 10 txs scenario"
             runScenario logging tvar (nFixtureWalletWithTxs 10 10 fixtureByronWallet)

             fmtTitle "Latencies for 10 fixture wallets with 20 txs scenario"
             runScenario logging tvar (nFixtureWalletWithTxs 10 20 fixtureByronWallet)

             fmtTitle "Latencies for 10 fixture wallets with 100 txs scenario"
             runScenario logging tvar (nFixtureWalletWithTxs 10 100 fixtureByronWallet)
  where
    -- Creates n fixture wallets and return two of them
    nFixtureWallet n fixtureWallet ctx = do
        wal1 : wal2 : _ <- replicateM n (fixtureWallet ctx)
        pure (wal1, wal2)

    -- Creates n fixture wallets and send 1-lovelace transactions to one of them
    -- (m times). The money is sent in batches (see batchSize below) from
    -- additionally created source fixture wallet. Then we wait for the money
    -- to be accommodated in recipient wallet. After that the source fixture
    -- wallet is removed.
    nFixtureWalletWithTxs n m fixtureByronWallet ctx = do
        (wal1, wal2) <- nFixtureWallet n fixtureByronWallet ctx

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

    repeatPostTx ctx wDest amtToSend batchSize amtExp = do
        wSrc <- fixtureRandomWallet ctx
        replicateM_ batchSize
            (postTx ctx (wSrc, Link.createTransaction @'Byron, fixturePassphrase) wDest amtToSend)
        eventually "repeatPostTx: wallet balance is as expected" $ do
            rWal1 <- request @ApiByronWallet ctx (Link.getWallet @'Byron wDest) Default Empty
            verify rWal1
                [ expectSuccess
                , expectField
                    (#balance . #available . #getQuantity)
                    (`shouldBe` amtExp)
                ]
        rDel <- request @ApiByronWallet ctx (Link.deleteWallet @'Byron wSrc) Default Empty
        expectResponseCode @IO HTTP.status204 rDel
        pure ()

    postTx ctx (wSrc, postTxEndp, pass) wDest amt = do
        (_, addrs) <-
            unsafeRequest @[ApiAddress n] ctx (Link.listAddresses @'Byron wDest) Empty
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

    runScenario logging tvar scenario = benchWithServer logging $ \ctx -> do
        (wal1, wal2) <- scenario ctx

        t1 <- measureApiLogs tvar
            (request @[ApiByronWallet] ctx (Link.listWallets @'Byron) Default Empty)
        fmtResult "listWallets        " t1

        t2 <- measureApiLogs tvar
            (request @ApiByronWallet ctx (Link.getWallet @'Byron wal1) Default Empty)
        fmtResult "getWallet          " t2

        t3 <- measureApiLogs tvar
            (request @ApiUtxoStatistics ctx (Link.getUTxOsStatistics @'Byron wal1) Default Empty)
        fmtResult "getUTxOsStatistics " t3

        t4 <- measureApiLogs tvar
            (request @[ApiAddress n] ctx (Link.listAddresses @'Byron wal1) Default Empty)
        fmtResult "listAddresses      " t4

        t5 <- measureApiLogs tvar
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
        t6 <- measureApiLogs tvar $ request @ApiFee ctx
            (Link.getTransactionFee @'Byron wal1) Default payload
        fmtResult "postTransactionFee " t6

        t7 <- measureApiLogs tvar $ request @ApiNetworkInformation ctx
            Link.getNetworkInfo Default Empty
        fmtResult "getNetworkInfo     " t7

        pure ()

meanAvg :: [NominalDiffTime] -> Double
meanAvg ts = sum (map realToFrac ts) * 1000 / fromIntegral (length ts)

buildResult :: [NominalDiffTime] -> Builder
buildResult [] = "ERR"
buildResult ts = build $ fixedF 1 $ meanAvg ts

fmtTitle :: Builder -> IO ()
fmtTitle title = fmt (indentF 4 title)

fmtResult :: String -> [NominalDiffTime] -> IO ()
fmtResult title ts =
    let titleExt = title|+" - " :: String
        titleF = padLeftF 30 ' ' titleExt
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
    -> (Context Byron -> IO ())
    -> IO ()
benchWithServer tracers action = do
    ctx <- newEmptyMVar
    let setupContext bp wAddr = do
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
                , _blockchainParameters = bp
                , _target = Proxy
                }
    race (takeMVar ctx >>= action) (withServer setupContext) >>=
        either pure (throwIO . ProcessHasExited "integration")
  where
    withServer act =
        withCardanoNode nullTracer $(getTestData) $ \socketPath block0 (bp,vData) ->
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
                (bp, vData)
                (act bp)

instance ToJSON ApiLog where
    toJSON = toJSON . toText

instance FromJSON ApiLog where
    parseJSON _ = fail "FromJSON ApiLog stub"

instance ToObject ApiLog
