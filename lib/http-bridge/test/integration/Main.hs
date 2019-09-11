{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Prelude

import Cardano.BM.Trace
    ( nullTracer )
import Cardano.CLI
    ( Port (..) )
import Cardano.Faucet
    ( initFaucet )
import Cardano.Launcher
    ( Command (..), StdStream (..), launch )
import Cardano.Wallet
    ( newWalletLayer )
import Cardano.Wallet.Api.Server
    ( Listen (..) )
import Cardano.Wallet.DB.Sqlite
    ( SqliteContext )
import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge, byronBlockchainParameters, byronFeePolicy )
import Cardano.Wallet.HttpBridge.Environment
    ( Network (..) )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.Fee
    ( FeePolicy (..) )
import Control.Concurrent
    ( ThreadId, forkIO, killThread, threadDelay )
import Control.Concurrent.Async
    ( async, cancel, link, race )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Exception
    ( throwIO )
import Data.Aeson
    ( Value (..), (.:) )
import Data.Function
    ( (&) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Product.Typed
    ( HasType, typed )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( ToText (..) )
import Data.Time
    ( addUTCTime, defaultTimeLocale, formatTime, getCurrentTime )
import Network.HTTP.Client
    ( defaultManagerSettings, newManager )
import Network.Wai.Handler.Warp
    ( setBeforeMainLoop )
import Numeric.Natural
    ( Natural )
import System.Directory
    ( createDirectoryIfMissing )
import System.FilePath
    ( (</>) )
import System.IO
    ( IOMode (..), hClose, hSetEncoding, openFile, stderr, stdout, utf8 )
import System.IO.Temp
    ( createTempDirectory, getCanonicalTemporaryDirectory )
import Test.Hspec
    ( after, afterAll, beforeAll, describe, hspec )
import Test.Integration.Framework.DSL
    ( Context (..), KnownCommand (..), TxDescription (..), tearDown )
import Test.Integration.Framework.Request
    ( Headers (Default), Payload (Empty), request )
import Test.Utils.Ports
    ( findPort, randomUnusedTCPPorts )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Cardano.Wallet.HttpBridge.Network as HttpBridge
import qualified Cardano.Wallet.HttpBridge.NetworkSpec as HttpBridge
import qualified Cardano.Wallet.HttpBridge.Transaction as HttpBridge
import qualified Cardano.WalletSpec as Wallet
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp
import qualified Test.Integration.HttpBridge.Scenario.API.Transactions as TransactionsBridge
import qualified Test.Integration.HttpBridge.Scenario.CLI.Launcher as LauncherCLI
import qualified Test.Integration.HttpBridge.Scenario.CLI.Server as ServerCLI
import qualified Test.Integration.HttpBridge.Scenario.CLI.Transactions as TransactionsCLIBridge
import qualified Test.Integration.Scenario.API.Addresses as Addresses
import qualified Test.Integration.Scenario.API.Transactions as Transactions
import qualified Test.Integration.Scenario.API.Wallets as Wallets
import qualified Test.Integration.Scenario.CLI.Addresses as AddressesCLI
import qualified Test.Integration.Scenario.CLI.Miscellaneous as MiscellaneousCLI
import qualified Test.Integration.Scenario.CLI.Mnemonics as MnemonicsCLI
import qualified Test.Integration.Scenario.CLI.Port as PortCLI
import qualified Test.Integration.Scenario.CLI.Transactions as TransactionsCLI
import qualified Test.Integration.Scenario.CLI.Wallets as WalletsCLI

-- | Define the actual executable name for the bridge CLI
instance KnownCommand (HttpBridge n) where
    commandName = "cardano-wallet-http-bridge"

main :: forall t. (t ~ HttpBridge 'Testnet) => IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    hspec $ do
        describe "PR_DISABLED Server CLI timeout test" (ServerCLI.specNoBackend @t)
        describe "Cardano.WalletSpec" Wallet.spec
        describe "Cardano.Wallet.HttpBridge.NetworkSpec" HttpBridge.spec
        describe "Launcher CLI tests" (LauncherCLI.spec @t)
        describe "Mnemonics CLI tests" (MnemonicsCLI.spec @t)
        describe "Miscellaneous CLI tests" (MiscellaneousCLI.spec @t)
        describe "--port CLI tests [SERIAL]" $ do
            PortCLI.specNegative @t
            (findPort >>= cardanoWalletServer Nothing)
                & beforeAll
                $ afterAll killServer
                $ describe "with default port" $ do
                    PortCLI.specCommon @t
                    PortCLI.specWithDefaultPort @t
            (findPort >>= cardanoWalletServer (Just $ ListenOnPort $ getPort defaultPort))
                & beforeAll
                $ afterAll killServer
                $ describe "with specified port" $ do
                    PortCLI.specCommon @t
            (findPort >>= cardanoWalletServer (Just ListenOnRandomPort))
                & beforeAll
                $ afterAll killServer
                $ describe "with random port" $ do
                    PortCLI.specCommon @t
                    PortCLI.specWithRandomPort @t defaultPort
        beforeAll startCluster $ afterAll _cleanup $ after tearDown $ do
            describe "PR_DISABLED Wallets API endpoint tests" (Wallets.spec @t)
            describe "Transactions API endpoint tests" (Transactions.spec @t)
            describe "PR_DISABLED Addresses API endpoint tests" (Addresses.spec @t)
            describe "Wallets CLI tests" (WalletsCLI.spec @t)
            describe "Transactions CLI tests" (TransactionsCLI.spec @t)
            describe "Addresses CLI tests" (AddressesCLI.spec @t)
            describe "Server CLI tests" (ServerCLI.spec @t)
            describe "Transactions CLI tests (bridge specific)"
                (TransactionsCLIBridge.spec @t)
            describe "PR_DISABLED Transactions API endpoint tests (bridge specific)"
                (TransactionsBridge.spec @t)
  where
    oneSecond :: Int
    oneSecond = 1 * 1000 * 1000 -- 1 second in microseconds

    defaultPort :: Port "wallet"
    defaultPort = Port 8090

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
        [ nodeApiPort, docPort, bridgePort ] <- randomUnusedTCPPorts 3
        let [core0Port, core1Port, core2Port] = [3000..3002] :: [Int]
        let relayPort = 3100 :: Int -- ports are hardcoded in topology.json
        tmp <- flip createTempDirectory "cardano-wallet-http-bridge"
            =<< getCanonicalTemporaryDirectory
        putStrLn $ "Using directory: " ++ tmp
        let addr p = "127.0.0.1:" ++ show p
        let stateDir = "./test/data/cardano-node-simple"
        let networkDir = tmp </> "networks"
        let nodeDir = tmp </> "cardano-node-simple"
        let nodeApiAddress = addr nodeApiPort
        createDirectoryIfMissing True nodeDir
        handle <- openFile (tmp </> "cardano-http-bridge.log") WriteMode
        start <-
            formatTime defaultTimeLocale "%s" . addUTCTime 2 <$> getCurrentTime
        let openLog name = openFile (nodeDir </> name) WriteMode
        [h0, h1, h2, h3] <- mapM openLog ["core0", "core1", "core2", "relay"]
        cluster <- async $ throwIO =<< launch
            [ cardanoNodeSimple stateDir start ("core0", addr core0Port) h0 []
            , cardanoNodeSimple stateDir start ("core1", addr core1Port) h1 []
            , cardanoNodeSimple stateDir start ("core2", addr core2Port) h2 []
            , cardanoNodeSimple stateDir start ("relay", addr relayPort) h3
                [ "--node-api-address", nodeApiAddress
                , "--node-doc-address", addr docPort
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
        (_, walletPort, db, nl) <-
            cardanoWalletServer (Just ListenOnRandomPort) bridgePort
        wait "cardano-wallet" (threadDelay oneSecond)
        let baseURL = mkBaseUrl (getPort walletPort)
        manager <- (baseURL,) <$> newManager defaultManagerSettings
        faucet <- putStrLn "Creating money out of thin air..." *> initFaucet nl
        let estimator = mkFeeEstimator byronFeePolicy
        let cleanup = do
                cancel cluster
                hClose handle
                Sqlite.destroyDBLayer db
        return $ Context cleanup manager walletPort (Port bridgePort) faucet
            estimator Proxy

    killServer :: (HasType ThreadId s, HasType SqliteContext s) => s -> IO ()
    killServer ctx = do
        Sqlite.destroyDBLayer (ctx ^. typed @SqliteContext)
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
        -> Int
        -> IO (ThreadId, Port "wallet", SqliteContext, NetworkLayer network IO)
    cardanoWalletServer mlisten bridgePort = do
        nl <- HttpBridge.newNetworkLayer bridgePort
        logConfig <- CM.empty
        (ctx, db) <- Sqlite.newDBLayer logConfig nullTracer Nothing
        mvar <- newEmptyMVar
        thread <- forkIO $ do
            let tl = HttpBridge.newTransactionLayer
            let bp = byronBlockchainParameters
            wallet <- newWalletLayer nullTracer bp db nl tl
            let listen = fromMaybe (ListenOnPort $ getPort defaultPort) mlisten
            Server.withListeningSocket listen $ \(port, socket) -> do
                let settings = Warp.defaultSettings
                        & setBeforeMainLoop (putMVar mvar (Port port))
                Server.start settings nullTracer socket wallet
        (thread,,ctx,nl) <$> takeMVar mvar

    waitForCluster :: String -> IO ()
    waitForCluster addr = do
        manager <- newManager defaultManagerSettings
        let ctx = Identity ("http://" <> T.pack addr, manager)
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

mkFeeEstimator :: FeePolicy -> TxDescription -> (Natural, Natural)
mkFeeEstimator policy (TxDescription nInps nOuts) =
    let
        LinearFee (Quantity a) (Quantity b) = policy
        nChgs = nOuts
        feeMin = a + b * double (12 + (181*nInps) + (52*nOuts) + (52*nChgs))
            -- 12  bytes -- CBOR overhead for serializing a signed tx
            -- 42  bytes -- per TxIn, index < 23
            -- 52  bytes -- per TxOut, addr for testnet sequential, amount < 23
            -- 139 bytes -- per TxIn (Witness)
        feeMax = a + b * double (8 + (185*nInps) + (60*nOuts) + (60*nChgs))
            -- 8   bytes -- CBOR overhead for serializing a signed tx
            -- 46  bytes -- per TxIn, big index
            -- 60  bytes -- per TxOut, addr for testnet sequential, big amounts
            -- 139 bytes -- per TxIn (Witness)
    in
        (ceiling feeMin, ceiling feeMax)
  where
    double :: Int -> Double
    double = fromRational . toRational
