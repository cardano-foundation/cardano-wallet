{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude

import Cardano.Launcher
    ( Command (..), launch )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( async, cancel, link )
import Control.Monad
    ( void )
import Data.Aeson
    ( Value )
import Data.Time
    ( addUTCTime, defaultTimeLocale, formatTime, getCurrentTime )
import Network.HTTP.Client
    ( defaultManagerSettings, newManager )
import Network.HTTP.Types.Status
    ( status200, status404, status405 )
import Test.Hspec
    ( SpecWith, afterAll, beforeAll, describe, hspec, it, shouldBe )
import Test.Integration.Framework.DSL
    ( Context (..), expectResponseCode, request )

import qualified Cardano.Wallet.Network.HttpBridgeSpec as HttpBridge
import qualified Cardano.WalletSpec as Wallet
import qualified Data.Text as T

main :: IO ()
main = do
    hspec $ do
        describe "Cardano.WalletSpec" Wallet.spec
        describe "Cardano.Wallet.Network.HttpBridge" HttpBridge.spec

        beforeAll startCluster $ afterAll killCluster $ do
            describe "Integration test framework" dummySpec

        beforeAll dummySetup $ do
            describe "Test response codes" respCodesSpec
  where
    -- Run a local cluster of cardano-sl nodes, a cardano-http-bridge on top and
    -- a cardano wallet server connected to the bridge.
    startCluster :: IO Context
    startCluster = do
        let stateDir = "./test/data/cardano-node-simple"
        systemStart <-
            formatTime defaultTimeLocale "%s" . addUTCTime 10 <$> getCurrentTime
        cluster <- async $ void $ launch
            [ cardanoNodeSimple stateDir systemStart ("core0", "127.0.0.1:3000")
            , cardanoNodeSimple stateDir systemStart ("core1", "127.0.0.1:3001")
            , cardanoNodeSimple stateDir systemStart ("core2", "127.0.0.1:3002")
            , cardanoNodeSimple stateDir systemStart ("relay", "127.0.0.1:3100")
            , cardanoWalletLauncher "1337" "8080" "local"
            ]
        link cluster
        let baseURL = "http://localhost:1337/"
        manager <- newManager defaultManagerSettings
        return $ Context cluster (baseURL, manager)

    killCluster :: Context -> IO ()
    killCluster (Context cluster _) = cancel cluster

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

    cardanoWalletLauncher serverPort bridgePort network = Command
        "cardano-wallet-launcher"
        [ "--wallet-server-port", serverPort
        , "--http-bridge-port", bridgePort
        , "--network", network
        ] (threadDelay 6000000)


-- Exercise the request functions, which just fail at the moment.
dummySpec :: SpecWith Context
dummySpec = do
    it "dummy spec" $ \(Context _ (url, _))  -> do
        url `shouldBe` "http://localhost:1337/"

-- Temporary test setup for testing response codes
dummySetup :: IO Context
dummySetup = do
    cluster <- async (return ())
    let baseURL = T.pack ("http://httpbin.org")
    manager <- newManager defaultManagerSettings
    return $ Context cluster (baseURL, manager)

-- Exercise response codes
respCodesSpec :: SpecWith Context
respCodesSpec = do
    it "GET; Response code 200" $ \ctx -> do
        response <- request @Value ctx ("GET", "/get?my=arg") Nothing Nothing
        expectResponseCode @IO status200 response

    it "GET; Response code 404" $ \ctx -> do
        response <- request @Value ctx ("GET", "/get/nothing") Nothing Nothing
        expectResponseCode @IO status404 response

    it "POST; Response code 200" $ \ctx -> do
        let header = [("dummy", "header")]
        response <- request @Value ctx ("POST", "/post") (Just header) Nothing
        expectResponseCode @IO status200 response

    it "POST; Response code 405" $ \ctx -> do
        response <- request @Value ctx ("POST", "/get") Nothing Nothing
        expectResponseCode @IO status405 response
