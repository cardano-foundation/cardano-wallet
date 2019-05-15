{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude

import Cardano.Environment.HttpBridge
    ( Network (..), network )
import Cardano.Launcher
    ( Command (..), StdStream (..), launch )
import Cardano.Wallet
    ( newWalletLayer )
import Cardano.Wallet.Api
    ( Api )
import Cardano.Wallet.Api.Server
    ( server )
import Cardano.Wallet.Compatibility.HttpBridge
    ( HttpBridge )
import Control.Concurrent
    ( forkIO, threadDelay )
import Control.Concurrent.Async
    ( async, cancel, link, race )
import Control.Monad
    ( void )
import Data.Aeson
    ( Value (..), (.:) )
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
import Test.Integration.Framework.Request
    ( Headers (Default), Payload (Empty), request )

import qualified Cardano.LauncherSpec as Launcher
import qualified Cardano.Wallet.DB.MVar as MVar
import qualified Cardano.Wallet.Network.HttpBridge as HttpBridge
import qualified Cardano.Wallet.Network.HttpBridgeSpec as HttpBridge
import qualified Cardano.Wallet.Transaction.HttpBridge as HttpBridge
import qualified Cardano.WalletSpec as Wallet
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as T
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
    oneSecond :: Int
    oneSecond = 1 * 1000 * 1000 -- 1 second in microseconds

    wait :: String -> IO () -> IO ()
    wait component action = do
        putStrLn $ "Waiting for " <> component <> " to warm-up..."
        race (threadDelay (60*oneSecond)) action >>= \case
            Left _ ->
                fail $ "Waited too long for " <> component <> " to start."
            Right _ ->
                return ()

    -- Run a local cluster of cardano-sl nodes, a cardano-http-bridge on top and
    -- a cardano wallet server connected to the bridge.
    startCluster :: IO Context
    startCluster = do
        let stateDir = "./test/data/cardano-node-simple"
        let networkDir = "/tmp/cardano-http-bridge/networks"
        let bridgePort = 8080
        let nodeApiAddress = "127.0.0.1:3101"
        removePathForcibly (networkDir <> "/local")
        handle <-
            openFile "/tmp/cardano-wallet-launcher" WriteMode
        sysStart <-
            formatTime defaultTimeLocale "%s" . addUTCTime 2 <$> getCurrentTime
        cluster <- async $ void $ launch
            [ cardanoNodeSimple stateDir sysStart ("core0", "127.0.0.1:3000") []
            , cardanoNodeSimple stateDir sysStart ("core1", "127.0.0.1:3001") []
            , cardanoNodeSimple stateDir sysStart ("core2", "127.0.0.1:3002") []
            , cardanoNodeSimple stateDir sysStart ("relay", "127.0.0.1:3100")
                [ "--node-api-address", nodeApiAddress
                , "--node-doc-address", "127.0.0.1:3102"
                , "--tlscert", "/dev/null"
                , "--tlskey", "/dev/null"
                , "--tlsca", "/dev/null"
                , "--no-tls"
                ]
            , cardanoHttpBridge bridgePort "local" networkDir handle
                (waitForCluster nodeApiAddress)
            ]
        link cluster
        wait "cardano-node-simple" (waitForCluster nodeApiAddress)
        wait "cardano-http-bridge" (threadDelay oneSecond)
        nl <- HttpBridge.newNetworkLayer bridgePort
        cardanoWalletServer nl 1337
        wait "cardano-wallet" (threadDelay oneSecond)
        let baseURL = "http://localhost:1337/"
        manager <- newManager defaultManagerSettings
        faucet <- putStrLn "Creating money out of thin air..." *> initFaucet nl
        return $ Context cluster (baseURL, manager) handle faucet

    killCluster :: Context -> IO ()
    killCluster (Context cluster _ handle _) = do
        cancel cluster
        hClose handle

    cardanoNodeSimple stateDir systemStart (nodeId, nodeAddr) extra = Command
        "cardano-node-simple"
        ([ "--system-start", systemStart
        , "--node-id", nodeId
        , "--keyfile", stateDir <> "/keys/" <> nodeId <> ".sk"
        , "--configuration-file", stateDir <> "/configuration.yaml"
        , "--configuration-key", "default"
        , "--topology", stateDir <> "/topology.json"
        , "--db-path", "/tmp/cardano-node-simple/db/" <> nodeId
        , "--listen", nodeAddr
        , "--log-config", stateDir <> "/logs/" <> nodeId <> "/config.json"
        , "--rebuild-db"
        ] ++ extra) (pure ())
        NoStream

    cardanoHttpBridge port template dir handle before = Command
        "cardano-http-bridge"
        [ "start"
        , "--template", template
        , "--port", show port
        , "--networks-dir", dir
        ] before
        (UseHandle handle)

    -- NOTE
    -- We start the wallet server in the same process such that we get
    -- code coverage measures from running the scenarios on top of it!
    cardanoWalletServer nl serverPort = void $ forkIO $ do
        db <- MVar.newDBLayer
        let tl = HttpBridge.newTransactionLayer
        wallet <- newWalletLayer @_ @HttpBridge db nl tl
        let settings = Warp.defaultSettings & Warp.setPort serverPort
        Warp.runSettings settings (serve (Proxy @("v2" :> Api)) (server wallet))

    waitForCluster :: String -> IO ()
    waitForCluster addr = do
        manager <- newManager defaultManagerSettings
        let ctx = Context
                { _cluster = undefined
                , _logs = undefined
                , _faucet = undefined
                , _manager = ("http://" <> T.pack addr, manager)
                }
        let err =  "waitForCluster: unexpected positive response from Api"
        request @Value ctx ("GET", "/api/v1/node-info") Default Empty >>= \case
            (_, Left _) ->
                threadDelay oneSecond *> waitForCluster addr
            (_, Right (Object m)) -> do
                let parseHeight m0 = do
                        m1 <- m0 .: "data"
                        m2 <- m1 .: "localBlockchainHeight"
                        m2 .: "quantity"
                case Aeson.parseMaybe @_ @Int parseHeight m of
                    Just q | q > 0 -> return ()
                    Just _ -> threadDelay oneSecond *> waitForCluster addr
                    Nothing -> fail err
            (_, Right _) ->
                fail err
