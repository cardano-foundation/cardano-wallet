{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
    ( initFaucet, mkFeeEstimator, sockAddrPort )
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
    ( serveWallet )
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
import Control.Monad.STM
    ( atomically )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Maybe
    ( mapMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Time
    ( UTCTime )
import Data.Time.Clock
    ( diffUTCTime )
import Fmt
    ( Builder, build, fixedF, fmtLn, (+|) )
import Network.HTTP.Client
    ( defaultManagerSettings
    , managerResponseTimeout
    , newManager
    , responseTimeoutMicro
    )
import Network.Wai.Middleware.Logging
    ( ApiLog (..), ServerLog (..), WithRequestId (..) )
import Numeric.Natural
    ( Natural )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
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
    , postTxFeeEp
    , request
    )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Data.Text as T

main :: forall t n. (t ~ Jormungandr, n ~ 'Testnet) => IO ()
main = do
    tvar <- newTVarIO []
    logging <- setupLatencyLogging tvar
    let toDiff :: UTCTime -> UTCTime -> Double
        toDiff t t' = (1000 * ) $ realToFrac $ diffUTCTime t t'
    let toMilliseconds :: UTCTime -> UTCTime -> Builder
        toMilliseconds t t' = build $ fixedF 1 $ toDiff t t'
    withUtf8Encoding $ benchWithServer logging $ \ctx -> do
        (wal1, wal2) <- twoFixtureWallet ctx

        (_res1, [t1, t1']) <- measureLatency tvar
            (request @[ApiWallet] ctx listWalletsEp Default Empty)

        (_res2, [t2, t2']) <- measureLatency tvar
            (request @ApiWallet ctx (getWalletEp wal1) Default Empty)

        (_res3, [t3, t3']) <- measureLatency tvar
            (request @ApiUtxoStatistics ctx (getWalletUtxoEp wal1) Default Empty)

        (_res4, [t4, t4']) <- measureLatency tvar
            (request @[ApiAddress n] ctx (getAddressesEp wal1 "") Default Empty)

        (_res5, [t5, t5']) <- measureLatency tvar
            (request @[ApiTransaction n] ctx (listTxEp wal1 "") Default Empty)

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
        (_res6, [t6, t6']) <- measureLatency tvar
            (request @ApiFee ctx (postTxFeeEp wal1) Default payload)

        (_res7, [t7, t7']) <- measureLatency tvar
            (request @[ApiStakePool] ctx listStakePoolsEp Default Empty)

        (_res8, [t8, t8']) <- measureLatency tvar
            (request @ApiNetworkInformation ctx networkInfoEp Default Empty)

        fmtLn "Latencies for two fixture wallets scenario"
        fmtLn ("    listWallets        - "+|(toMilliseconds t1 t1')+|" ms")
        fmtLn ("    getWallet          - "+|(toMilliseconds t2 t2')+|" ms")
        fmtLn ("    getUTxOsStatistics - "+|(toMilliseconds t3 t3')+|" ms")
        fmtLn ("    listAddresses      - "+|(toMilliseconds t4 t4')+|" ms")
        fmtLn ("    listTransactions   - "+|(toMilliseconds t5 t5')+|" ms")
        fmtLn ("    postTransactionFee - "+|(toMilliseconds t6 t6')+|" ms")
        fmtLn ("    listStakePools     - "+|(toMilliseconds t7 t7')+|" ms")
        fmtLn ("    getNetworkInfo     - "+|(toMilliseconds t8 t8')+|" ms")

        pure ()
  where
    twoFixtureWallet ctx =
        (,) <$> fixtureWallet ctx <*> fixtureWallet ctx

measureLatency
    :: TVar [LogObject ServerLog]
    -> IO a
    -> IO (a, [UTCTime])
measureLatency tvar action = do
    atomically $ writeTVar tvar []
    res <- action
    let getTimestamp = tstamp . loMeta
    let filterMsg logObj = case loContent logObj of
            LogMessage (LogApiMsg (WithRequestId _ LogRequestStart)) ->
                Just $ getTimestamp logObj
            LogMessage (LogApiMsg (WithRequestId _ LogRequestFinish)) ->
                Just $ getTimestamp logObj
            _ -> Nothing
    getTimes <- mapMaybe filterMsg <$> readTVarIO tvar
    pure (res, getTimes)

setupLatencyLogging
    :: TVar [LogObject ServerLog]
    -> IO (CM.Configuration, Trace IO ServerLog)
setupLatencyLogging tvar = do
    cfg <- do
        cfg' <-defaultConfigStdout
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
                faucet <- initFaucet
                putMVar ctx $ Context
                    { _cleanup = pure ()
                    , _manager = manager
                    , _nodePort = nPort
                    , _walletPort = sockAddrPort wAddr
                    , _faucet = faucet
                    , _feeEstimator = mkFeeEstimator (getFeePolicy bp)
                    , _target = Proxy
                    }
        race (takeMVar ctx >>= action) (withServer setupContext) >>=
            either pure (throwIO . ProcessHasExited "latency benchmark")

    withServer setup = withConfig $ \jmCfg ->
        serveWallet @'Testnet logCfg (SyncTolerance 10) Nothing "127.0.0.1"
            ListenOnRandomPort (Launch jmCfg) setup
