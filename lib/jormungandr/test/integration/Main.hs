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
import Cardano.CLI
    ( Port (..) )
import Cardano.Faucet
    ( initFaucet )
import Cardano.Wallet
    ( newWalletLayer )
import Cardano.Wallet.Api.Server
    ( Listen (..) )
import Cardano.Wallet.DB.Sqlite
    ( SqliteContext )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr, Network (..) )
import Cardano.Wallet.Jormungandr.Launch
    ( setupConfig, teardownConfig )
import Cardano.Wallet.Jormungandr.Network
    ( JormungandrBackend (..)
    , JormungandrConfig (..)
    , connParamsPort
    , withNetworkLayer
    )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.Fee
    ( FeePolicy (..) )
import Cardano.Wallet.Primitive.Model
    ( BlockchainParameters (..) )
import Control.Concurrent.Async
    ( async, waitEither )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Exception
    ( bracket, throwIO )
import Control.Monad
    ( void )
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
import Test.Hspec
    ( after, afterAll, beforeAll, describe, hspec, parallel )
import Test.Integration.Framework.DSL
    ( Context (..), KnownCommand (..), TxDescription (..), tearDown )
import Test.Utils.Ports
    ( findPort )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Cardano.Wallet.Jormungandr.NetworkSpec as Network
import qualified Cardano.Wallet.Jormungandr.Transaction as Jormungandr
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp
import qualified Test.Integration.Jormungandr.Scenario.API.Transactions as TransactionsApiJormungandr
import qualified Test.Integration.Jormungandr.Scenario.CLI.Launcher as LauncherCLI
import qualified Test.Integration.Jormungandr.Scenario.CLI.Server as ServerCLI
import qualified Test.Integration.Jormungandr.Scenario.CLI.Transactions as TransactionsCliJormungandr
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
    describe "Server CLI timeout test" $ parallel (ServerCLI.specNoBackend @t)
    describe "Cardano.Wallet.NetworkSpec" $ parallel Network.spec
    describe "Mnemonics CLI tests" $ parallel (MnemonicsCLI.spec @t)
    describe "Miscellaneous CLI tests" $ parallel (MiscellaneousCLI.spec @t)
    describe "Ports CLI (negative) tests" $ parallel (PortCLI.specNegative @t)
    describe "Launcher CLI tests" $ parallel (LauncherCLI.spec @t)
    beforeAll (start Nothing) $ afterAll _cleanup $ after tearDown $ do
        -- API e2e Testing
        describe "Addresses API endpoint tests" Addresses.spec
        describe "Transactions API endpoint tests" Transactions.spec
        describe "Transactions API endpoint tests (Jormungandr specific)"
            (TransactionsApiJormungandr.spec @t)
        describe "Transactions CLI endpoint tests (Jormungandr specific)"
            (TransactionsCliJormungandr.spec @t)
        describe "Wallets API endpoint tests" Wallets.spec
        -- Command-Line e2e Testing
        describe "Addresses CLI tests" (AddressesCLI.spec @t)
        describe "Server CLI tests" (ServerCLI.spec @t)
        describe "Transactions CLI tests" (TransactionsCLI.spec @t)
        describe "Wallets CLI tests" (WalletsCLI.spec @t)
        describe "Ports CLI (default) tests [SERIAL]" $ do
            PortCLI.specCommon @t
            PortCLI.specWithDefaultPort @t
    beforeAll (start (Just True)) $ afterAll _cleanup $ after tearDown $ do
        describe "Ports CLI (explicit) tests [SERIAL]" $ do
            PortCLI.specCommon @t
    beforeAll (start (Just False)) $ afterAll _cleanup $ after tearDown $ do
        describe "Ports CLI (random) tests [SERIAL]" $ do
            PortCLI.specCommon @t
            PortCLI.specWithRandomPort @t defaultPort
  where
    withServer :: Maybe Bool -> IO () -> (Context (Jormungandr 'Testnet) -> IO a) -> IO a
    withServer useExplicitPort cleanup action = do
        unusedPort <- findPort
        let listen = fmap (\e -> if e then ListenOnPort unusedPort else ListenOnRandomPort) useExplicitPort
        bracket setupConfig teardownConfig $ \jmConfig -> do
            withCardanoWalletServer jmConfig listen $ \(wPort, jPort, feePolicy, _db) -> do
                let baseUrl = "http://localhost:" <> T.pack (show wPort) <> "/"
                manager <- (baseUrl,) <$> newManager defaultManagerSettings
                faucet <- initFaucet
                action $ Context cleanup manager wPort jPort
                    faucet (mkFeeEstimator feePolicy) Proxy

    -- fixme: generalise into an aroundAll SpecWith function
    start :: Maybe Bool -> IO (Context (Jormungandr 'Testnet))
    start useExplicitPort = do
        ctx <- newEmptyMVar
        done <- newEmptyMVar
        void . async $ withServer useExplicitPort (putMVar done ()) $ \c -> do
            putMVar ctx c
            takeMVar done
        takeMVar ctx

-- | Initialize logging at the specified minimum 'Severity' level.
initTracer :: Severity -> Text -> IO (Trace IO Text)
initTracer minSeverity cmd = do
    c <- defaultConfigStdout
    setMinSeverity c minSeverity
    appendName cmd <$> setupTrace (Right c) "cardano-wallet"

withCardanoWalletServer
    :: forall network a. (network ~ Jormungandr 'Testnet)
    => JormungandrConfig
    -> Maybe Listen
    -> ((Port "wallet", Port "node", FeePolicy, SqliteContext) -> IO a)
    -> IO a
withCardanoWalletServer jmConfig mlisten action = do
    logConfig <- CM.empty
    tracer <- initTracer Info "serve"
    withNetworkLayer tracer (Launch jmConfig) $ \cp -> either throwIO $ \nl -> do
        let (block0, bp) = staticBlockchainParameters nl
        let setupDB = Sqlite.newDBLayer @_ @network logConfig tracer Nothing
        let teardownDB = Sqlite.destroyDBLayer . fst
        bracket setupDB teardownDB $ \(sqlCtx, db) -> do
            mvar <- newEmptyMVar
            handle1 <- async $ do
                let tl = Jormungandr.newTransactionLayer (getGenesisBlockHash bp)
                wallet <- newWalletLayer tracer (block0, bp) db nl tl
                let listen = fromMaybe (ListenOnPort $ getPort defaultPort) mlisten
                Server.withListeningSocket listen $ \(port, socket) -> do
                    let settings = Warp.defaultSettings
                            & setBeforeMainLoop (putMVar mvar (Port port))
                    Server.start settings tracer socket wallet
                throwIO $ userError "The test API server exited."
            wPort <- takeMVar mvar
            let ctx = (wPort, Port (connParamsPort cp), getFeePolicy bp, sqlCtx)
            handle2 <- async $ action ctx
            either id id <$> waitEither handle1 handle2

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
defaultPort :: Port "wallet"
defaultPort = Port 8090
