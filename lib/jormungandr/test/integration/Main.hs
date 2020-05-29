{-# LANGUAGE AllowAmbiguousTypes #-}
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
import Cardano.Launcher
    ( ProcessHasExited (..) )
import Cardano.Pool.Jormungandr.Metadata
    ( envVarMetadataRegistry )
import Cardano.Startup
    ( withUtf8Encoding )
import Cardano.Wallet.Api.Server
    ( Listen (..) )
import Cardano.Wallet.Api.Types
    ( DecodeAddress (..), EncodeAddress (..) )
import Cardano.Wallet.Jormungandr
    ( serveWallet, setupTracers, tracerSeverities )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Jormungandr.Faucet
    ( initFaucet )
import Cardano.Wallet.Jormungandr.Launch
    ( withConfig )
import Cardano.Wallet.Jormungandr.Network
    ( JormungandrBackend (..) )
import Cardano.Wallet.Network.Ports
    ( unsafePortNumber )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DelegationAddress (..)
    , NetworkDiscriminant (..)
    , NetworkDiscriminantVal (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation.Jormungandr
    ( JormungandrKey )
import Cardano.Wallet.Primitive.Fee
    ( FeePolicy (..) )
import Cardano.Wallet.Primitive.Types
    ( NetworkParameters (..), SyncTolerance (..), TxParameters (..) )
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
    ( Spec, SpecWith, after, beforeWith, describe, hspec, parallel )
import Test.Hspec.Extra
    ( aroundAll )
import Test.Integration.Framework.DSL
    ( Context (..), KnownCommand (..), TxDescription (..), tearDown )
import Test.Utils.Paths
    ( getTestData )
import Test.Utils.StaticServer
    ( withStaticServer )

import qualified Cardano.Pool.Jormungandr.MetricsSpec as MetricsSpec
import qualified Cardano.Wallet.Jormungandr.NetworkSpec as NetworkLayer
import qualified Data.Text as T
import qualified Test.Integration.Jormungandr.Scenario.API.StakePools as StakePoolsApiJormungandr
import qualified Test.Integration.Jormungandr.Scenario.API.Transactions as TransactionsApiJormungandr
import qualified Test.Integration.Jormungandr.Scenario.CLI.Keys as KeysCLI
import qualified Test.Integration.Jormungandr.Scenario.CLI.Launcher as LauncherCLI
import qualified Test.Integration.Jormungandr.Scenario.CLI.Mnemonics as MnemonicsJormungandr
import qualified Test.Integration.Jormungandr.Scenario.CLI.Port as PortCLIJormungandr
import qualified Test.Integration.Jormungandr.Scenario.CLI.Server as ServerCLI
import qualified Test.Integration.Jormungandr.Scenario.CLI.StakePools as StakePoolsCliJormungandr
import qualified Test.Integration.Jormungandr.Scenario.CLI.Transactions as TransactionsCliJormungandr
import qualified Test.Integration.Scenario.API.Byron.Transactions as ByronTransactions
import qualified Test.Integration.Scenario.API.Byron.Wallets as ByronWallets
import qualified Test.Integration.Scenario.API.Network as Network
import qualified Test.Integration.Scenario.API.Shelley.Addresses as Addresses
import qualified Test.Integration.Scenario.API.Shelley.HWWallets as HWWallets
import qualified Test.Integration.Scenario.API.Shelley.Transactions as Transactions
import qualified Test.Integration.Scenario.API.Shelley.Wallets as Wallets
import qualified Test.Integration.Scenario.CLI.Miscellaneous as MiscellaneousCLI
import qualified Test.Integration.Scenario.CLI.Mnemonics as MnemonicsCLI
import qualified Test.Integration.Scenario.CLI.Network as NetworkCLI
import qualified Test.Integration.Scenario.CLI.Port as PortCLI
import qualified Test.Integration.Scenario.CLI.Shelley.Addresses as AddressesCLI
import qualified Test.Integration.Scenario.CLI.Shelley.HWWallets as HWWalletsCLI
import qualified Test.Integration.Scenario.CLI.Shelley.Transactions as TransactionsCLI
import qualified Test.Integration.Scenario.CLI.Shelley.Wallets as WalletsCLI

-- | Define the actual executable name for the bridge CLI
instance KnownCommand Jormungandr where
    commandName = "cardano-wallet-jormungandr"

main :: forall t n. (t ~ Jormungandr, n ~ 'Testnet 0) => IO ()
main = withUtf8Encoding $ withLogging Nothing Info $ \(_, tr) -> do
    hspec $ do
        describe "No backend required" $ do
            describe "Cardano.Wallet.NetworkSpec" $ parallel NetworkLayer.spec
            describe "Mnemonics CLI tests" $ parallel (MnemonicsCLI.spec @t)
            describe "Mnemonics CLI tests (Jormungandr)" $ parallel (MnemonicsJormungandr.spec @t)
            describe "Miscellaneous CLI tests" $ parallel (MiscellaneousCLI.spec @t)
            describe "Launcher CLI tests" $ parallel (LauncherCLI.spec @t)
            describe "Stake Pool Metrics" MetricsSpec.spec
            describe "Key CLI tests" KeysCLI.spec

        describe "API Specifications" $ specWithServer @n tr $ do
            withCtxOnly $ Addresses.spec @n
            withCtxOnly $ Transactions.spec @n
            withCtxOnly $ Wallets.spec @n
            withCtxOnly $ ByronWallets.spec @n
            withCtxOnly $ ByronTransactions.spec @n
            withCtxOnly $ HWWallets.spec @n
            withCtxOnly $ TransactionsApiJormungandr.spec @n @t
            withCtxOnly $ TransactionsCliJormungandr.spec @n @t
            withCtxOnly Network.spec
            StakePoolsApiJormungandr.spec @n

        describe "CLI Specifications" $ specWithServer @n tr $ do
            withCtxOnly $ AddressesCLI.spec @n @t
            withCtxOnly $ TransactionsCLI.spec @n @t
            withCtxOnly $ WalletsCLI.spec @n @t
            withCtxOnly $ HWWalletsCLI.spec @n @t
            withCtxOnly $ PortCLI.spec @t
            withCtxOnly $ PortCLIJormungandr.spec @t
            withCtxOnly $ NetworkCLI.spec @t
            withCtxOnly $ StakePoolsCliJormungandr.spec @t
            ServerCLI.spec @t
  where
    withCtxOnly
        :: SpecWith (Context Jormungandr)
        -> SpecWith (Port "node", FeePolicy, Context Jormungandr)
    withCtxOnly =
        beforeWith (pure . thd3)

specWithServer
    :: forall (n :: NetworkDiscriminant).
        ( NetworkDiscriminantVal n
        , DecodeAddress n
        , EncodeAddress n
        , DelegationAddress n JormungandrKey
        )
    => Trace IO Text
    -> SpecWith (Port "node", FeePolicy, Context Jormungandr)
    -> Spec
specWithServer tr = aroundAll withContext . after (tearDown . thd3)
  where
    withContext action = do
        ctx <- newEmptyMVar
        let setupContext wAddr nPort gbp = do
                let baseUrl = "http://" <> T.pack (show wAddr) <> "/"
                logInfo tr baseUrl
                let sixtySeconds = 60*1000*1000 -- 60s in microseconds
                manager <- (baseUrl,) <$> newManager (defaultManagerSettings
                    { managerResponseTimeout =
                        responseTimeoutMicro sixtySeconds
                    })
                let feePolicy = getFeePolicy (txParameters gbp)
                faucet <- initFaucet feePolicy
                putMVar ctx (nPort, feePolicy, Context
                    { _cleanup = pure ()
                    , _manager = manager
                    , _walletPort = Port . fromIntegral $ unsafePortNumber wAddr
                    , _faucet = faucet
                    , _feeEstimator = mkFeeEstimator feePolicy
                    , _blockchainParameters = genesisParameters gbp
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
            serveWallet @n
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
-- bridge (and probably cardano-node on Jormungandr), it's not possible to
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
