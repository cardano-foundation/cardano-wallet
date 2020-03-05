{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Trace
    ( Trace, logInfo )
import Cardano.CLI
    ( Port (..), withLogging )
import Cardano.Faucet
    ( initFaucet )
import Cardano.Launcher
    ( ProcessHasExited (..) )
import Cardano.Pool.Metadata
    ( envVarMetadataRegistry )
import Cardano.Startup
    ( withUtf8Encoding )
import Cardano.Wallet.Api.Server
    ( Listen (..) )
import Cardano.Wallet.Jormungandr
    ( serveWallet, setupTracers, tracerSeverities )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Jormungandr.Launch
    ( withConfig )
import Cardano.Wallet.Jormungandr.Network
    ( JormungandrBackend (..) )
import Cardano.Wallet.Network.Ports
    ( unsafePortNumber )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Fee
    ( FeePolicy (..) )
import Cardano.Wallet.Primitive.Types
    ( BlockchainParameters (..), SyncTolerance (..) )
import Control.Concurrent.Async
    ( race )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Exception
    ( throwIO )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Tuple.Extra
    ( thd3 )
import Network.HTTP.Client
    ( defaultManagerSettings
    , managerResponseTimeout
    , newManager
    , responseTimeoutMicro
    )
import Numeric.Natural
    ( Natural )
import System.Environment
    ( setEnv )
import System.FilePath
    ( (</>) )
import System.IO.Temp
    ( withSystemTempDirectory )
import Test.Hspec
    ( Spec, SpecWith, after, beforeWith, describe, hspec )
import Test.Hspec.Extra
    ( aroundAll )
import Test.Integration.Framework.DSL
    ( Context (..), KnownCommand (..), TxDescription (..), tearDown )
import Test.Utils.Paths
    ( getTestData )
import Test.Utils.StaticServer
    ( withStaticServer )

{--
import qualified Cardano.Pool.MetricsSpec as MetricsSpec
import qualified Cardano.Wallet.Jormungandr.NetworkSpec as NetworkLayer
--}
import qualified Data.Text as T
import qualified Test.Integration.Jormungandr.Scenario.API.StakePools as StakePoolsApiJormungandr
{--
import qualified Test.Integration.Jormungandr.Scenario.API.Transactions as TransactionsApiJormungandr
import qualified Test.Integration.Jormungandr.Scenario.CLI.Keys as KeysCLI
import qualified Test.Integration.Jormungandr.Scenario.CLI.Launcher as LauncherCLI
import qualified Test.Integration.Jormungandr.Scenario.CLI.Mnemonics as MnemonicsJormungandr
import qualified Test.Integration.Jormungandr.Scenario.CLI.Server as ServerCLI
import qualified Test.Integration.Jormungandr.Scenario.CLI.StakePools as StakePoolsCliJormungandr
import qualified Test.Integration.Jormungandr.Scenario.CLI.Transactions as TransactionsCliJormungandr
import qualified Test.Integration.Scenario.API.Addresses as Addresses
import qualified Test.Integration.Scenario.API.ByronTransactions as ByronTransactions
import qualified Test.Integration.Scenario.API.ByronWallets as ByronWallets
import qualified Test.Integration.Scenario.API.HWWallets as HWWallets
import qualified Test.Integration.Scenario.API.Network as Network
--}
import qualified Test.Integration.Scenario.API.Transactions as Transactions
{--
import qualified Test.Integration.Scenario.API.Wallets as Wallets
import qualified Test.Integration.Scenario.CLI.Addresses as AddressesCLI
import qualified Test.Integration.Scenario.CLI.Miscellaneous as MiscellaneousCLI
import qualified Test.Integration.Scenario.CLI.Mnemonics as MnemonicsCLI
import qualified Test.Integration.Scenario.CLI.Network as NetworkCLI
import qualified Test.Integration.Scenario.CLI.Port as PortCLI
import qualified Test.Integration.Scenario.CLI.Transactions as TransactionsCLI
import qualified Test.Integration.Scenario.CLI.Wallets as WalletsCLI
--}
-- | Define the actual executable name for the bridge CLI
instance KnownCommand Jormungandr where
    commandName = "cardano-wallet-jormungandr"

main :: forall t. (t ~ Jormungandr) => IO ()
main = withUtf8Encoding $ withLogging Nothing Info $ \(_, tr) -> do
    hspec $ do
        {--
        describe "No backend required" $ do
            describe "Cardano.Wallet.NetworkSpec" $ parallel NetworkLayer.spec
            describe "Mnemonics CLI tests" $ parallel (MnemonicsCLI.spec @t)
            describe "Mnemonics CLI tests (Jormungandr)" $ parallel (MnemonicsJormungandr.spec @t)
            describe "Miscellaneous CLI tests" $ parallel (MiscellaneousCLI.spec @t)
            describe "Launcher CLI tests" $ parallel (LauncherCLI.spec @t)
            describe "Stake Pool Metrics" MetricsSpec.spec
            describe "Key CLI tests" KeysCLI.spec
--}
        describe "API Specifications" $ specWithServer tr $ do
            --withCtxOnly Addresses.spec
            withCtxOnly Transactions.spec
            --withCtxOnly Wallets.spec
            --withCtxOnly ByronWallets.spec
            --withCtxOnly ByronTransactions.spec
            --withCtxOnly Network.spec
            --withCtxOnly HWWallets.spec
            --withCtxOnly $ TransactionsApiJormungandr.spec @t
            --withCtxOnly $ TransactionsCliJormungandr.spec @t
            StakePoolsApiJormungandr.spec

{--
        describe "CLI Specifications" $ specWithServer tr $ do
            withCtxOnly $ AddressesCLI.spec @t
            withCtxOnly $ StakePoolsCliJormungandr.spec @t
            withCtxOnly $ TransactionsCLI.spec @t
            withCtxOnly $ WalletsCLI.spec @t
            withCtxOnly $ PortCLI.spec @t
            withCtxOnly $ NetworkCLI.spec @t
            ServerCLI.spec @t
--}
  where
    withCtxOnly
        :: SpecWith (Context Jormungandr)
        -> SpecWith (Port "node", FeePolicy, Context Jormungandr)
    withCtxOnly =
        beforeWith (pure . thd3)

specWithServer
    :: Trace IO Text
    -> SpecWith (Port "node", FeePolicy, Context Jormungandr)
    -> Spec
specWithServer tr = aroundAll withContext . after (tearDown . thd3)
  where
    withContext action = do
        ctx <- newEmptyMVar
        let setupContext wAddr nPort bp = do
                let baseUrl = "http://" <> T.pack (show wAddr) <> "/"
                logInfo tr baseUrl
                let sixtySeconds = 60*1000*1000 -- 60s in microseconds
                manager <- (baseUrl,) <$> newManager (defaultManagerSettings
                    { managerResponseTimeout =
                        responseTimeoutMicro sixtySeconds
                    })
                let feePolicy = getFeePolicy bp
                faucet <- initFaucet feePolicy
                putMVar ctx (nPort, feePolicy, Context
                    { _cleanup = pure ()
                    , _manager = manager
                    , _walletPort = Port . fromIntegral $ unsafePortNumber wAddr
                    , _faucet = faucet
                    , _feeEstimator = mkFeeEstimator feePolicy
                    , _target = Proxy
                    })
        race
            (takeMVar ctx >>= action)
            (withServer setupContext)
            >>= either pure (throwIO . ProcessHasExited "integration")

    withServer setup =
        withConfig $ \jmCfg ->
        withMetadataRegistry $
        withSystemTempDirectory "cardano-wallet-databases" $ \db ->
            serveWallet @'Testnet
                tracers
                (SyncTolerance 10)
                (Just db)
                "127.0.0.1"
                ListenOnRandomPort
                (Launch jmCfg)
                setup

    tracers = setupTracers (tracerSeverities (Just Info)) tr

-- | Run a HTTP file server on any free port, serving up the integration tests
-- stake pool metadata registry.
withMetadataRegistry :: IO a -> IO a
withMetadataRegistry action = withStaticServer root $ \baseUrl -> do
    let registryUrl = baseUrl <> "test-integration-registry.zip"
    setEnv envVarMetadataRegistry registryUrl
    action
  where
    root = $(getTestData) </> "jormungandr" </> "stake_pools" </> "registry"

-- NOTE²
-- We use a range (min, max) and call it an "estimator" because for the
-- bridge (and probably cardano-node on Shelley), it's not possible to
-- compute the fee precisely by only knowing the number of inputs and
-- ouputs since the exact fee cost depends on the values of the
-- outputs and the values of the input indexes.
mkFeeEstimator :: FeePolicy -> TxDescription -> (Natural, Natural)
mkFeeEstimator policy = \case
    PaymentDescription nInps nOuts nChgs ->
        let fee = linear (nInps + nOuts + nChgs) 0
        in (fee, fee)
    DelegDescription nInps nOuts nCerts ->
        let fee = linear (nInps + nOuts) nCerts
        in (fee, fee)
  where
    LinearFee (Quantity a) (Quantity b) (Quantity c) = policy
    -- NOTE¹
    -- We safely round BEFORE the multiplication because we know that
    -- Jormungandr' fee are necessarily naturals constants. We carry doubles
    -- here because of the legacy with Byron. In the end, it matters not
    -- because in the spectrum of numbers we're going to deal with, naturals
    -- can be represented without any rounding issue using 'Double' (or,
    -- transactions have suddenly become overly expensive o_O)
    linear nb nc = fromIntegral $ round a + nb * round b + nc * round c
