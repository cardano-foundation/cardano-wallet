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

import Cardano.BM.Configuration.Model
    ( setMinSeverity )
import Cardano.BM.Configuration.Static
    ( defaultConfigStdout )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Setup
    ( setupTrace )
import Cardano.BM.Trace
    ( Trace, appendName )
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
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr, Network (..) )
import Cardano.Wallet.Jormungandr.Network
    ( BaseUrl (..), JormungandrLayer (..), Scheme (..), mkJormungandrLayer )
import Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx (..) )
import Cardano.Wallet.Network
    ( NetworkLayer (..), defaultRetryPolicy, waitForConnection )
import Cardano.Wallet.Primitive.Fee
    ( FeePolicy (..) )
import Cardano.Wallet.Primitive.Types
    ( Block (..), Hash (..) )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex, unsafeRunExceptT )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( Async, async, cancel )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Monad
    ( void )
import Data.Coerce
    ( coerce )
import Data.Function
    ( (&) )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Network.HTTP.Client
    ( defaultManagerSettings, newManager )
import Network.Wai.Handler.Warp
    ( setBeforeMainLoop )
import Numeric.Natural
    ( Natural )
import System.Directory
    ( removePathForcibly )
import System.IO
    ( IOMode (..), hClose, openFile )
import Test.Hspec
    ( after, afterAll, beforeAll, describe, hspec )
import Test.Integration.Framework.DSL
    ( Context (..), KnownCommand (..), TxDescription (..), tearDown )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Cardano.Wallet.Jormungandr.Network as Jormungandr
import qualified Cardano.Wallet.Jormungandr.NetworkSpec as Network
import qualified Cardano.Wallet.Jormungandr.Transaction as Jormungandr
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp
import qualified Test.Integration.Jormungandr.Scenario.API.Transactions as TransactionsJormungandr
import qualified Test.Integration.Jormungandr.Scenario.CLI.Launcher as LauncherCLI
import qualified Test.Integration.Jormungandr.Scenario.CLI.Server as ServerCLI
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
instance KnownCommand (Jormungandr n) where
    commandName = "cardano-wallet-jormungandr"

main :: forall t. (t ~ Jormungandr 'Testnet) => IO ()
main = hspec $ do
    describe "Cardano.Wallet.NetworkSpec" Network.spec
    describe "Mnemonics CLI tests" (MnemonicsCLI.spec @t)
    describe "Miscellaneous CLI tests" (MiscellaneousCLI.spec @t)
    describe "Ports CLI (negative) tests" (PortCLI.specNegative @t)
    describe "Launcher CLI tests" (LauncherCLI.spec @t)
    beforeAll (start Nothing) $ afterAll _cleanup $ after tearDown $ do
        -- API e2e Testing
        describe "Addresses API endpoint tests" Addresses.spec
        describe "Transactions API endpoint tests" Transactions.spec
        describe "Transactions API endpoint tests (Jormungandr specific)"
            (TransactionsJormungandr.spec @t)
        describe "Wallets API endpoint tests" Wallets.spec
        -- Command-Line e2e Testing
        describe "Addresses CLI tests" (AddressesCLI.spec @t)
        describe "Server CLI tests" (ServerCLI.spec @t)
        describe "Transactions CLI tests" (TransactionsCLI.spec @t)
        describe "Wallets CLI tests" (WalletsCLI.spec @t)
        describe "Ports CLI (default) tests" $ do
            PortCLI.specCommon @t
            PortCLI.specWithDefaultPort @t
    let explicitPort = Just $ ListenOnPort defaultPort
    beforeAll (start explicitPort) $ afterAll _cleanup $ after tearDown $ do
        describe "Ports CLI (explicit) tests" $ do
            PortCLI.specCommon @t
    let randomPort = Just ListenOnRandomPort
    beforeAll (start randomPort) $ afterAll _cleanup $ after tearDown $ do
        describe "Ports CLI (random) tests" $ do
            PortCLI.specCommon @t
            PortCLI.specWithRandomPort @t defaultPort
  where
    start :: Maybe Listen -> IO (Context (Jormungandr 'Testnet))
    start listen = do
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
        (handle', port, feePolicy, db) <- cardanoWalletServer listen
        let baseUrl = "http://localhost:" <> T.pack (show port) <> "/"
        manager <- (baseUrl,) <$> newManager defaultManagerSettings
        faucet <- initFaucet
        let estimator = mkFeeEstimator feePolicy
        let cleanup = do
                cancel handle
                cancel handle'
                hClose logs
                Sqlite.destroyDBLayer db
                threadDelay oneSecond
        return $ Context cleanup manager port faucet estimator Proxy

-- | Initialize logging at the specified minimum 'Severity' level.
initTracer :: Severity -> Text -> IO (Trace IO Text)
initTracer minSeverity cmd = do
    c <- defaultConfigStdout
    setMinSeverity c minSeverity
    setupTrace (Right c) "cardano-wallet" >>= appendName cmd

-- NOTE
-- We start the wallet server in the same process such that we get
-- code coverage measures from running the scenarios on top of it!
cardanoWalletServer
    :: forall network. (network ~ Jormungandr 'Testnet)
    => Maybe Listen
    -> IO (Async (), Int, FeePolicy, SqliteContext)
cardanoWalletServer mlisten = do
    logConfig <- CM.empty
    tracer <- initTracer Info "serve"
    (nl, block0, feePolicy) <- newNetworkLayer jormungandrUrl block0H
    (sqlCtx, db) <- Sqlite.newDBLayer @_ @network logConfig tracer Nothing
    mvar <- newEmptyMVar
    handle <- async $ do
        let tl = Jormungandr.newTransactionLayer block0H
        wallet <- newWalletLayer tracer block0 feePolicy db nl tl
        let listen = fromMaybe (ListenOnPort defaultPort) mlisten
        Server.withListeningSocket listen $ \(port, socket) -> do
            let settings = Warp.defaultSettings
                    & setBeforeMainLoop (putMVar mvar port)
            Server.start settings tracer socket wallet
    (handle,,feePolicy,sqlCtx) <$> takeMVar mvar
  where
    jormungandrUrl :: BaseUrl
    jormungandrUrl = BaseUrl Http "localhost" 8080 "/api"
    block0H = Hash $ unsafeFromHex
        "dba597bee5f0987efbf56f6bd7f44c38158a7770d0cb28a26b5eca40613a7ebd"
        -- ^ jcli genesis hash --input test/data/jormungandr/block0.bin

-- Instantiate a new 'NetworkLayer' for 'Jormungandr', and fetches the
-- genesis block for starting a 'WalletLayer'.
newNetworkLayer
    :: BaseUrl
    -> Hash "Genesis"
    -> IO (NetworkLayer (Jormungandr n) IO, Block Tx, FeePolicy)
newNetworkLayer url block0H = do
    mgr <- newManager defaultManagerSettings
    let jormungandr = mkJormungandrLayer mgr url
    let nl = Jormungandr.mkNetworkLayer jormungandr
    waitForConnection nl defaultRetryPolicy
    block0 <- unsafeRunExceptT $
        getBlock jormungandr (coerce block0H)
    feePolicy <- unsafeRunExceptT $
        getInitialFeePolicy jormungandr (coerce block0H)
    return (nl, block0, feePolicy)

mkFeeEstimator :: FeePolicy -> TxDescription -> (Natural, Natural)
mkFeeEstimator policy (TxDescription nInps nOuts) =
    let
        LinearFee (Quantity a) (Quantity b) = policy
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

-- | One second in micro-seconds
oneSecond :: Int
oneSecond = 1000000

-- | Default port for the wallet server
defaultPort :: Int
defaultPort = 8090
