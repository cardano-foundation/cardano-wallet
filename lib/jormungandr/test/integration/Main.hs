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

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.CLI
    ( initTracer )
import Cardano.Faucet
    ( initFaucet )
import Cardano.Launcher
    ( ProcessHasExited (..) )
import Cardano.Wallet.Api.Server
    ( Listen (..) )
import Cardano.Wallet.Jormungandr
    ( serveWallet )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr, Network (..) )
import Cardano.Wallet.Jormungandr.Launch
    ( setupConfig, teardownConfig )
import Cardano.Wallet.Jormungandr.Network
    ( JormungandrBackend (..) )
import Cardano.Wallet.Primitive.Fee
    ( FeePolicy (..) )
import Cardano.Wallet.Primitive.Model
    ( BlockchainParameters (..) )
import Control.Concurrent.Async
    ( async, race, wait )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Exception
    ( bracket, throwIO )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( showT )
import Network.HTTP.Client
    ( defaultManagerSettings, newManager )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( after, beforeAll, describe, hspec, parallel )
import Test.Integration.Framework.DSL
    ( Context (..), KnownCommand (..), TxDescription (..), tearDown )

import qualified Cardano.Wallet.Jormungandr.NetworkSpec as Network
import qualified Data.Text as T
import qualified Test.Integration.Jormungandr.Scenario.API.StakePools as StakePoolsApiJormungandr
import qualified Test.Integration.Jormungandr.Scenario.API.Transactions as TransactionsApiJormungandr
import qualified Test.Integration.Jormungandr.Scenario.CLI.Launcher as LauncherCLI
import qualified Test.Integration.Jormungandr.Scenario.CLI.Server as ServerCLI
import qualified Test.Integration.Jormungandr.Scenario.CLI.StakePools as StakePoolsCliJormungandr
import qualified Test.Integration.Jormungandr.Scenario.CLI.Transactions as TransactionsCliJormungandr
import qualified Test.Integration.Scenario.API.Addresses as Addresses
import qualified Test.Integration.Scenario.API.ByronWallets as ByronWallets
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
    describe "No backend required" $ do
        describe "Cardano.Wallet.NetworkSpec" $ parallel Network.spec
        describe "Mnemonics CLI tests" $ parallel (MnemonicsCLI.spec @t)
        describe "Miscellaneous CLI tests" $ parallel (MiscellaneousCLI.spec @t)
        describe "Launcher CLI tests" $ parallel (LauncherCLI.spec @t)

    describe "API Specifications" $ beforeAll start $ after tearDown $ do
        Addresses.spec
        StakePoolsApiJormungandr.spec
        Transactions.spec
        TransactionsApiJormungandr.spec @t
        TransactionsCliJormungandr.spec @t
        Wallets.spec
        ByronWallets.spec

    describe "CLI Specifications" $ beforeAll start $ after tearDown $ do
        AddressesCLI.spec @t
        ServerCLI.spec @t
        StakePoolsCliJormungandr.spec @t
        TransactionsCLI.spec @t
        WalletsCLI.spec @t
        PortCLI.spec @t

start :: IO (Context (Jormungandr 'Testnet))
start = do
    ctx <- newEmptyMVar
    logCfg <- initTracer Info "integration"
    pid <- async $ bracket setupConfig teardownConfig $ \jmCfg -> do
        let listen = ListenOnRandomPort
        serveWallet logCfg Nothing listen (Launch jmCfg) $ \wPort nPort bp -> do
            let baseUrl = "http://localhost:" <> T.pack (showT wPort) <> "/"
            manager <- (baseUrl,) <$> newManager defaultManagerSettings
            faucet <- initFaucet
            putMVar ctx $  Context
                { _cleanup = pure ()
                , _manager = manager
                , _nodePort = nPort
                , _walletPort = wPort
                , _faucet = faucet
                , _feeEstimator = mkFeeEstimator (getFeePolicy bp)
                , _target = Proxy
                }
    race (takeMVar ctx) (wait pid) >>=
        either pure (throwIO . ProcessHasExited "integration")

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
