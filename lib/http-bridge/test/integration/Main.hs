{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude

import Cardano.Environment.HttpBridge
    ( Network (..), network )
import Cardano.Launcher
    ( Command (..), StdStream (..), launch )
import Cardano.Wallet
    ( mkWalletLayer )
import Cardano.Wallet.Api
    ( Api )
import Cardano.Wallet.Api.Server
    ( server )
import Cardano.Wallet.Compatibility.HttpBridge
    ( HttpBridge )
import Control.Concurrent
    ( forkIO, threadDelay )
import Control.Concurrent.Async
    ( async, cancel, link )
import Control.Monad
    ( void )
import Data.Function
    ( (&) )
import Data.Proxy
    ( Proxy (..) )
import Data.Time
    ( addUTCTime, defaultTimeLocale, formatTime, getCurrentTime )
import Network.HTTP.Client
    ( defaultManagerSettings, newManager )
import Servant
    ( (:>), serve )
import System.Directory
    ( removePathForcibly )
import System.IO
    ( IOMode (..), hClose, openFile )
import Test.Hspec
    ( after, afterAll, beforeAll, describe, hspec )
import Test.Integration.Faucet
    ( initFaucet )
import Test.Integration.Framework.DSL
    ( Context (..), tearDown )

import qualified Cardano.LauncherSpec as Launcher
import qualified Cardano.Wallet.DB.MVar as MVar
import qualified Cardano.Wallet.Network.HttpBridge as HttpBridge
import qualified Cardano.Wallet.Network.HttpBridgeSpec as HttpBridge
import qualified Cardano.Wallet.Transaction.HttpBridge as HttpBridge
import qualified Cardano.WalletSpec as Wallet
import qualified Network.Wai.Handler.Warp as Warp
import qualified Test.Integration.Scenario.Transactions as Transactions
import qualified Test.Integration.Scenario.Wallets as Wallets

main :: IO ()
main = do
    case network of
        Testnet ->
            return ()
        _ ->
            fail $ "unsupported integration environment: " <> show network
    hspec $ do
        describe "Cardano.LauncherSpec" Launcher.spec
        describe "Cardano.WalletSpec" Wallet.spec
        describe "Cardano.Wallet.Network.HttpBridgeSpec" HttpBridge.spec
        beforeAll startCluster $ afterAll killCluster $ after tearDown $ do
            describe "Wallets API endpoint tests" Wallets.spec
            describe "Transactions API endpoint tests" Transactions.spec
  where
    clusterWarmUpDelay :: Int
    clusterWarmUpDelay = 20 * 1000 * 1000 -- 20 seconds in microseconds

    bridgeWarmUpDelay :: Int
    bridgeWarmUpDelay = 1 * 1000 * 1000 -- 1 second in microseconds

    walletWarmUpDelay :: Int
    walletWarmUpDelay = 1 * 1000 * 1000 -- 1 second in microseconds

    humanReadable :: Int -> String
    humanReadable d =
        show (d `div` (1000 * 1000)) <> "s"

    wait :: (String, Int) -> IO ()
    wait (component, delay) = do
        putStrLn $ "Waiting " <> humanReadable delay
            <> " for " <> component <> " to warm-up..."
        threadDelay delay

    -- Run a local cluster of cardano-sl nodes, a cardano-http-bridge on top and
    -- a cardano wallet server connected to the bridge.
    startCluster :: IO Context
    startCluster = do
        let stateDir = "./test/data/cardano-node-simple"
        let networkDir = "/tmp/cardano-http-bridge/networks"
        let bridgePort = 8080
        removePathForcibly (networkDir <> "/local")
        handle <-
            openFile "/tmp/cardano-wallet-launcher" WriteMode
        systemStart <-
            formatTime defaultTimeLocale "%s" . addUTCTime 5 <$> getCurrentTime
        cluster <- async $ void $ launch
            [ cardanoNodeSimple stateDir systemStart ("core0", "127.0.0.1:3000")
            , cardanoNodeSimple stateDir systemStart ("core1", "127.0.0.1:3001")
            , cardanoNodeSimple stateDir systemStart ("core2", "127.0.0.1:3002")
            , cardanoNodeSimple stateDir systemStart ("relay", "127.0.0.1:3100")
            , cardanoHttpBridge bridgePort"local" networkDir handle
            ]
        link cluster
        wait ("cluster", clusterWarmUpDelay)
        wait ("cardano-http-bridge", bridgeWarmUpDelay)
        nl <- HttpBridge.newNetworkLayer bridgePort
        cardanoWalletServer nl 1337
        wait ("cardano-wallet", walletWarmUpDelay)
        let baseURL = "http://localhost:1337/"
        manager <- newManager defaultManagerSettings
        faucet <- putStrLn "Creating money out of thin air..." *> initFaucet nl
        return $ Context cluster (baseURL, manager) handle faucet

    killCluster :: Context -> IO ()
    killCluster (Context cluster _ handle _) = do
        cancel cluster
        hClose handle

    cardanoNodeSimple stateDir systemStart (nodeId, nodeAddr) = Command
        "cardano-node-simple"
        [ "--system-start", systemStart
        , "--node-id", nodeId
        , "--keyfile", stateDir <> "/keys/" <> nodeId <> ".sk"
        , "--configuration-file", stateDir <> "/configuration.yaml"
        , "--configuration-key", "default"
        , "--topology", stateDir <> "/topology.json"
        , "--db-path", "/tmp/cardano-node-simple/db/" <> nodeId
        , "--listen", nodeAddr
        , "--log-config", stateDir <> "/logs/" <> nodeId <> "/config.json"
        , "--rebuild-db"
        ] (pure ())
        NoStream

    cardanoHttpBridge port template dir handle = Command
        "cardano-http-bridge"
        [ "start"
        , "--template", template
        , "--port", show port
        , "--networks-dir", dir
        ] (threadDelay clusterWarmUpDelay)
        (UseHandle handle)

    -- NOTE
    -- We start the wallet server in the same process such that we get
    -- code coverage measures from running the scenarios on top of it!
    cardanoWalletServer nl serverPort = void $ forkIO $ do
        db <- MVar.newDBLayer
        let tl = HttpBridge.newTransactionLayer
        let wallet = mkWalletLayer @_ @HttpBridge db nl tl
        let settings = Warp.defaultSettings & Warp.setPort serverPort
        Warp.runSettings settings (serve (Proxy @("v2" :> Api)) (server wallet))
