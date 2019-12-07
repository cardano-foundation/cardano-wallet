{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Cardano.CLI
    ( Port (..) )
import Cardano.Faucet
    ( initFaucet )
import Cardano.Launcher
    ( ProcessHasExited (..), withUtf8Encoding )
import Cardano.Wallet.Api.Server
    ( Listen (..) )
import Cardano.Wallet.Api.Types
    ( ApiWallet )
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
import Cardano.Wallet.Primitive.Fee
    ( FeePolicy (..) )
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
import Data.Maybe
    ( mapMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Time
    ( UTCTime )
import Data.Time.Clock
    ( diffUTCTime )
import Fmt
    ( build, fixedF, fmtLn, (+|) )
import Network.HTTP.Client
    ( defaultManagerSettings
    , managerResponseTimeout
    , newManager
    , responseTimeoutMicro
    )
import Network.Socket
    ( SockAddr (..) )
import Network.Wai.Middleware.Logging
    ( ApiLog (..), ServerLog (..), WithRequestId (..) )
import Numeric.Natural
    ( Natural )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , TxDescription (..)
    , fixtureWallet
    , listWalletsEp
    , request
    )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Data.Text as T

main :: forall t. (t ~ Jormungandr) => IO ()
main = do
    tvar <- newTVarIO []
    logging <- setupLatencyLogging tvar
    let toMilliseconds :: UTCTime -> UTCTime -> Double
        toMilliseconds t t' =
            (1000 * ) $ realToFrac $ diffUTCTime t t'
    withUtf8Encoding $ benchWithServer logging $ \ctx -> do
        (_wal1, _wal2) <- twoFixtureWallet ctx

        (_res1, [t1,t1']) <- measureLatency tvar
            (request @[ApiWallet] ctx listWalletsEp Default Empty)

        fmtLn "Latencies for two fixture wallets scenario"
        fmtLn ("    listWallets - "+|(build $ fixedF 1 $ toMilliseconds t1 t1')+|" ms")

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

sockAddrPort :: SockAddr -> Port a
sockAddrPort addr = Port . fromIntegral $ case addr of
    SockAddrInet p _ -> p
    SockAddrInet6 p _ _ _ -> p
    _ -> 0

mkFeeEstimator :: FeePolicy -> TxDescription -> (Natural, Natural)
mkFeeEstimator policy (TxDescription nInps nOuts) =
    let
        LinearFee (Quantity a) (Quantity b) (Quantity _c) = policy
        nChanges = nOuts
        -- NOTE¹
        -- We safely round BEFORE the multiplication because we know that
        -- Jormungandr' fee are necessarily naturals constants. We carry doubles
        -- here because of the legacy with Byron. In the end, it matters not
        -- because in the spectrum of numbers we're going to deal with, naturals
        -- can be represented without any rounding issue using 'Double' (or,
        -- transactions have suddenly become overly expensive o_O)
        fee = fromIntegral $ (round a) + (nInps + nOuts + nChanges) * (round b)
    in
        -- NOTE²
        -- We use a range (min, max) and call it an "estimator" because for the
        -- bridge (and probably cardano-node on Shelley), it's not possible to
        -- compute the fee precisely by only knowing the number of inputs and
        -- ouputs since the exact fee cost depends on the values of the
        -- outputs and the values of the input indexes.
        (fee, fee)

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
