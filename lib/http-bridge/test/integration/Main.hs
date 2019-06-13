{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude

import Cardano.BM.Trace
    ( nullTracer )
import Cardano.Faucet
    ( initFaucet )
import Cardano.Launcher
    ( Command (..), StdStream (..), launch )
import Cardano.Wallet
    ( newWalletLayer )
import Cardano.Wallet.Api.Server
    ( Listen (..) )
import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge, block0 )
import Cardano.Wallet.HttpBridge.Environment
    ( Network (..) )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Control.Concurrent
    ( ThreadId, forkIO, killThread, threadDelay )
import Control.Concurrent.Async
    ( async, cancel, link, race )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Exception
    ( throwIO )
import Control.Monad
    ( forM )
import Data.Aeson
    ( Value (..), (.:) )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Product.Typed
    ( HasType, typed )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( ToText (..) )
import Data.Time
    ( addUTCTime, defaultTimeLocale, formatTime, getCurrentTime )
import Database.Persist.Sql
    ( SqlBackend, close' )
import Network.HTTP.Client
    ( defaultManagerSettings, newManager )
import System.Directory
    ( createDirectoryIfMissing, removePathForcibly )
import System.IO
    ( IOMode (..), hClose, openFile )
import Test.Hspec
    ( after, afterAll, beforeAll, describe, hspec )
import Test.Integration.Framework.DSL
    ( Context (..), tearDown )
import Test.Integration.Framework.Request
    ( Headers (Default), Payload (Empty), request )

import qualified Cardano.LauncherSpec as Launcher
import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Cardano.Wallet.HttpBridge.Network as HttpBridge
import qualified Cardano.Wallet.HttpBridge.NetworkSpec as HttpBridge
import qualified Cardano.Wallet.HttpBridge.Transaction as HttpBridge
import qualified Cardano.WalletSpec as Wallet
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as T
import qualified Test.Integration.Scenario.API.Addresses as Addresses
import qualified Test.Integration.Scenario.API.Transactions as Transactions
import qualified Test.Integration.Scenario.API.Wallets as Wallets
import qualified Test.Integration.Scenario.CLI.Addresses as AddressesCLI
import qualified Test.Integration.Scenario.CLI.Mnemonics as MnemonicsCLI
import qualified Test.Integration.Scenario.CLI.Port as PortCLI
import qualified Test.Integration.Scenario.CLI.Transactions as TransactionsCLI
import qualified Test.Integration.Scenario.CLI.Wallets as WalletsCLI

main :: IO ()
main = hspec $ do
    describe "Cardano.LauncherSpec" Launcher.spec
    describe "Cardano.WalletSpec" Wallet.spec
    describe "Cardano.Wallet.HttpBridge.NetworkSpec" HttpBridge.spec
    describe "CLI commands not requiring bridge" $ do
        describe "Mnemonics CLI tests" MnemonicsCLI.spec
        describe "--port CLI tests" $ do
            cardanoWalletServer Nothing
                & beforeAll
                $ afterAll killServer
                $ describe "with default port" $ do
                    PortCLI.specCommon
                    PortCLI.specWithDefaultPort
            cardanoWalletServer (Just $ ListenOnPort defaultPort)
                & beforeAll
                $ afterAll killServer
                $ describe "with specified port" $ do
                    PortCLI.specCommon
            cardanoWalletServer (Just ListenOnRandomPort)
                & beforeAll
                $ afterAll killServer
                $ describe "with random port" $ do
                    PortCLI.specCommon
                    PortCLI.specWithRandomPort defaultPort

    beforeAll startCluster $
        afterAll killCluster $ after tearDown $ do
        describe "Wallets API endpoint tests" Wallets.spec
        describe "Transactions API endpoint tests" Transactions.spec
        describe "Addresses API endpoint tests" Addresses.spec
        describe "Wallets CLI tests" WalletsCLI.spec
        describe "Transactions CLI tests" TransactionsCLI.spec
        describe "Addresses CLI tests" AddressesCLI.spec
  where
    oneSecond :: Int
    oneSecond = 1 * 1000 * 1000 -- 1 second in microseconds

    bridgePort :: Int
    bridgePort = 8080

    defaultPort :: Int
    defaultPort = 8090

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
    startCluster :: IO (Context (HttpBridge 'Testnet))
    startCluster = do
        let stateDir = "./test/data/cardano-node-simple"
        let networkDir = "/tmp/cardano-http-bridge/networks"
        let nodeApiAddress = "127.0.0.1:3101"
        removePathForcibly (networkDir <> "/local")
        createDirectoryIfMissing True "/tmp/cardano-node-simple"
        handle <-
            openFile "/tmp/cardano-wallet-launcher" WriteMode
        start <-
            formatTime defaultTimeLocale "%s" . addUTCTime 2 <$> getCurrentTime
        [h0, h1, h2, h3] <- forM ["core0", "core1", "core2", "relay"] $ \x -> do
            openFile ("/tmp/cardano-node-simple/" <> x) WriteMode
        cluster <- async $ throwIO =<< launch
            [ cardanoNodeSimple stateDir start ("core0", "127.0.0.1:3000") h0 []
            , cardanoNodeSimple stateDir start ("core1", "127.0.0.1:3001") h1 []
            , cardanoNodeSimple stateDir start ("core2", "127.0.0.1:3002") h2 []
            , cardanoNodeSimple stateDir start ("relay", "127.0.0.1:3100") h3
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
        (_, port, db, nl) <- cardanoWalletServer (Just ListenOnRandomPort)
        wait "cardano-wallet" (threadDelay oneSecond)
        let baseURL = mkBaseUrl port
        manager <- newManager defaultManagerSettings
        faucet <- putStrLn "Creating money out of thin air..." *> initFaucet nl
        return $ Context cluster (baseURL, manager) port handle faucet db Proxy

    killCluster :: Context t -> IO ()
    killCluster ctx = do
        cancel (_cluster ctx)
        hClose (_logs ctx)
        close' (_db ctx)

    killServer :: (HasType ThreadId s, HasType SqlBackend s) => s -> IO ()
    killServer ctx = do
        close' (ctx ^. typed @SqlBackend)
        killThread (ctx ^. typed @ThreadId)

    cardanoNodeSimple stateDir sysStart (nodeId, nodeAddr) h extra = Command
        "cardano-node-simple"
        ([ "--system-start", sysStart
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
        -- NOTE Ideally, we would give `NoStream` as a handle but if we do, the
        -- process never terminates on failure (probably because of some
        -- internal handler waiting for the stdout or stderr to be closed even
        -- though they're already closed... So, we just redirect the output to
        -- some places where it's less annoying.
        (UseHandle h)

    mkBaseUrl port = "http://localhost:" <> toText port <> "/"

    cardanoHttpBridge port template dir h before = Command
        "cardano-http-bridge"
        [ "start"
        , "--template", template
        , "--port", show port
        , "--networks-dir", dir
        ] before
        (UseHandle h)

    -- NOTE
    -- We start the wallet server in the same process such that we get
    -- code coverage measures from running the scenarios on top of it!
    cardanoWalletServer
        :: (network ~ HttpBridge 'Testnet)
        => Maybe Listen
        -> IO (ThreadId, Int, SqlBackend, NetworkLayer network IO)
    cardanoWalletServer mlisten = do
        nl <- HttpBridge.newNetworkLayer bridgePort
        (conn, db) <- Sqlite.newDBLayer Nothing
        port <- newEmptyMVar
        thread <- forkIO $ do
            let tl = HttpBridge.newTransactionLayer
            wallet <- newWalletLayer nullTracer block0 db nl tl
            let listen = fromMaybe (ListenOnPort defaultPort) mlisten
            Server.start (putMVar port) listen wallet
        (thread,,conn,nl) <$> takeMVar port

    waitForCluster :: String -> IO ()
    waitForCluster addr = do
        manager <- newManager defaultManagerSettings
        let ctx = Context
                { _cluster = undefined
                , _logs = undefined
                , _faucet = undefined
                , _target = undefined
                , _db = undefined
                , _port = undefined
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
