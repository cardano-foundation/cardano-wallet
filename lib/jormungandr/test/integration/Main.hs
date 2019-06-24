{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Cardano.Wallet.Api.Types
    ( ApiAddress, ApiWallet )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr, Network (..) )
import Cardano.Wallet.Jormungandr.Network
    ( BaseUrl (..), JormungandrLayer (..), Scheme (..), mkJormungandrLayer )
import Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx (..) )
import Cardano.Wallet.Network
    ( NetworkLayer (..), defaultRetryPolicy, waitForConnection )
import Cardano.Wallet.Primitive.Types
    ( Block (..), DecodeAddress, Hash (..) )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex, unsafeRunExceptT )
import Control.Concurrent
    ( forkIO )
import Control.Concurrent.Async
    ( async, cancel )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Monad
    ( void )
import Data.Coerce
    ( coerce )
import Data.Function
    ( (&) )
import Data.Proxy
    ( Proxy (..) )
import Database.Persist.Sql
    ( SqlBackend, close' )
import Network.HTTP.Client
    ( defaultManagerSettings, newManager )
import Network.Wai.Handler.Warp
    ( setBeforeMainLoop )
import System.Directory
    ( removePathForcibly )
import System.IO
    ( IOMode (..), hClose, openFile )
import Test.Hspec
    ( SpecWith, after, afterAll, beforeAll, describe, hspec, it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , balanceAvailable
    , balanceTotal
    , expectFieldEqual
    , expectListSizeEqual
    , expectResponseCode
    , fixtureWallet
    , getAddressesEp
    , getWalletEp
    , request
    , tearDown
    , verify
    )

import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Cardano.Wallet.Jormungandr.Network as Jormungandr
import qualified Cardano.Wallet.Jormungandr.NetworkSpec as Network
import qualified Cardano.Wallet.Jormungandr.Transaction as Jormungandr
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wai.Handler.Warp as Warp
import qualified Test.Integration.Scenario.API.Addresses as Addresses
import qualified Test.Integration.Scenario.API.Wallets as Wallets

-- | Temporary 'Spec' to illustrate that the integration scenario setup below
-- works as expected.
temporarySpec :: forall t. DecodeAddress t => SpecWith (Context t)
temporarySpec =
    it "Temporary example spec for Jörmungandr integration" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        r <- request @ApiWallet ctx (getWalletEp wSrc) Default Empty
        print r *> verify r
            [ expectResponseCode HTTP.status200
            , expectFieldEqual balanceAvailable 1000000000000
            , expectFieldEqual balanceTotal 1000000000000
            ]

        let q = "?state=used"
        r' <- request @[ApiAddress t] ctx (getAddressesEp wSrc q) Default Empty
        print r' *> verify r'
            [ expectResponseCode HTTP.status200
            , expectListSizeEqual 10
            ]

main :: IO ()
main = hspec $ do
    describe "Cardano.Wallet.NetworkSpec" Network.spec
    beforeAll start $ afterAll cleanup $ after tearDown $ do
        describe "Jörmungandr Temporary Spec" temporarySpec
        describe "Wallets API endpoint tests" Wallets.spec
        describe "Addresses API endpoint tests" Addresses.spec
  where
    start :: IO (Context (Jormungandr 'Testnet))
    start = do
        let dir = "./test/data/jormungandr"
        removePathForcibly "/tmp/cardano-wallet-jormungandr"
        logs <- openFile "/tmp/jormungandr" WriteMode
        let jormungandrLauncher = Command
                "jormungandr"
                [ "--genesis-block", dir ++ "/block0.bin"
                , "--config", dir ++ "/config.yaml"
                , "--secret", dir ++ "/secret.yaml"
                ] (return ())
                (UseHandle logs)
        handle <- async $ void $ launch [jormungandrLauncher]
        (port, db) <- cardanoWalletServer
        let baseUrl = "http://localhost:" <> T.pack (show port) <> "/"
        manager <- newManager defaultManagerSettings
        faucet <- initFaucet
        return $ Context handle (baseUrl, manager) port logs faucet db Proxy

    cleanup :: Context t -> IO ()
    cleanup ctx = do
        cancel (_cluster ctx)
        hClose (_logs ctx)
        close' (_db ctx)

-- NOTE
-- We start the wallet server in the same process such that we get
-- code coverage measures from running the scenarios on top of it!
cardanoWalletServer
    :: forall network. (network ~ Jormungandr 'Testnet)
    => IO (Int, SqlBackend)
cardanoWalletServer = do
    (nl, block0) <- newNetworkLayer jormungandrUrl block0H
    (conn, db) <- Sqlite.newDBLayer @_ @network nullTracer Nothing
    mvar <- newEmptyMVar
    void $ forkIO $ do
        let tl = Jormungandr.newTransactionLayer block0H
        wallet <- newWalletLayer nullTracer block0 db nl tl
        let listen = ListenOnRandomPort
        Server.withListeningSocket listen $ \(port, socket) -> do
            let settings = Warp.defaultSettings
                    & setBeforeMainLoop (putMVar mvar port)
            Server.start settings nullTracer socket wallet
    (,conn) <$> takeMVar mvar
  where
    jormungandrUrl :: BaseUrl
    jormungandrUrl = BaseUrl Http "localhost" 8081 "/api"
    block0H = Hash $ unsafeFromHex
        "78e6f4e2c463bae5d6318b96b203e48625c3a45227c4b80ea4d0dfc887c23621"
        -- ^ jcli genesis hash --input test/data/jormungandr/block0.bin

-- Instantiate a new 'NetworkLayer' for 'Jormungandr', and fetches the
-- genesis block for starting a 'WalletLayer'.
newNetworkLayer
    :: BaseUrl
    -> Hash "Genesis"
    -> IO (NetworkLayer (Jormungandr n) IO, Block Tx)
newNetworkLayer url block0H = do
    mgr <- newManager defaultManagerSettings
    let jormungandr = mkJormungandrLayer mgr url
    let nl = Jormungandr.mkNetworkLayer jormungandr
    waitForConnection nl defaultRetryPolicy
    block0 <- unsafeRunExceptT $ getBlock jormungandr (coerce block0H)
    return (nl, block0)

-- | One second in micro-seconds
oneSecond :: Int
oneSecond = 1000000
